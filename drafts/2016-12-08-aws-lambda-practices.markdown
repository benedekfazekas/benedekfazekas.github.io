---
title: AWS lambda practices
date: 2016-11-25
description: AWS lambda practices
tags: aws, lambda, serverless, microservies, clojure
---

### Why lambdas?

I work in a relatively small team on a web application that surfaces big data (how big is big?!) backed real time data visualisations and aggregated data sets. Our site is not public. It is a pay-for application with low traffic that mostly occurs within office hours. For some features we also need involved computations on the fly. More of that later...

Our business runs on cloud based services almost exclusively, AWS mostly but also some google cloud. When the wider team started moving away from a monorepo of puppet configs to spin up AWS machines and experimented with docker we thought to give AWS lambdas a try.

The recommended usecase for lambdas seems to be event processing where the event is an upload to an S3 bucket or message in a Kinesis stream. We found that we are fine with invoking lambdas directly (even synchronously sometimes but mostly asynchronously) from our existing web application.

Maintaining an already existing web application and adding lambdas as separately scalable backend services has the advantage that we do not need the AWS API gateway which frankly is a big pain to set up correctly. We could also re-use our existing stack/platform: most of our lambdas are implemented in JVM clojure.

### Calling a lambda, lambdas calling lambdas and callbacks

Calling a lambda is easy. In clojure:

<!--?prettify lang=clojure linenums=true -->

    (amazonica.aws.lambda/invoke {:endpoint "eu-west-1"}
                                  :invocation-type "RequestResponse"
                                  :function-name lambda-arn
                                  :payload json-message)

For a synchronous call. Or you can change the `invocation-type` to `"Event"` for an asynchronous call. The equivalent in ruby:

<!--?prettify lang=ruby linenums=true -->

    lambda = Aws::Lambda::Client.new(region: "eu-west-1", http_read_timeout: 300)
    lambda.invoke ({function_name: lambda_name,
                    payload: payload.to_json,
                    invocation_type: "RequestResponse"})

We do use synchronous calls sometimes. Apart from the known characterestics this also means that the lambda won't be retried by AWS if it fails.

For our usecase we use asynchronous calls. We call lambdas from lambdas and we do not want the caller sitting around waiting for the callee to finish. We rather use a Redis pub/sub topic to signal when a lambda completes in order to trigger some other behaviour somehwere else when the call is done. The redis topic name contains a reference to the lambda and also a request id so that we can identify and separate runs and lambdas. We can also tap into these topics if we are interested in how far the process got for tracing purposes; for example to implement a progress bar.

At one stage we tried out using SNS topics as well instead of calling lambdas directly as a way of making lambda calls indirect but have not seen much benefit of this so moved back to direct, asynchronous calls.

### The usecase for a chain of parallel lambdas

Some time ago we decided to give a try to a very involved feature on our site. We would attempt to predict how a hypothetical energy tariff would perform on a given energy market. To achieve this we would combine our data on the ever changing energy market and the behavioural data the visitors of the uSwitch web site generate.

The actual prediction is done by a statistical model developed in `R`. The model is quite an interesting topic in itself but out of the scope of this post. For this model to work though we needed to generate a big bulk (a couple of millions) of hypothetical visits of a made up result table showing energy deals based on a hypothetical market.

This task involves creating and pricing custom energy markets, sampling user profiles based on real visits and replay those profiles visiting our hypothetical markets. Some steps here were parallelisable and more importantly we could break up the user profiles sample data set into chunks and work on those chunks in parallel as well all the way up to the end of the task.

The following diagram shows part of our lambda based system to solve the above problem and also to demonstrate parallel processing of chunks of a data set. Note the use of the redis pub/sub callback topic described in the previous section.



                                                              async     +-.-.-.-.-.-.-.-.--+
                                                         +-------------->     chunk...     |
                                                         |    async   +------------------+ +
                                                         | +---------->    chunk 2       | |
                                         +---------------+--+ async +------------------+ | |
                                         |                  +------->                  | +-+
                                async    | filter population|       | filter population| |
                             +----------->                  |       | chunk            +-+
    +------------------+     |           |                  |       |                  |
    |                  |     |           +------------------+       +-------------.----+
    | run simulation   +-----+                    .                               .
    |                  |                          .                               .
    |                  |                          .                               .
    +-------------^----+                          .                               .
                  .                               .                               .
                  .                               .                               .
                  .                               .                               .
                  .                               .                               .
                  .                               .                               .
                  .                               .   redis pub/sub callbacks     .
                  .................................................................


### Monolith or microservices: well, does that really matter?!

Lambdas per definition are microservices and we plan to run a multitude of them (somewhat happens to microservices frequently). Some lambdas coordinate the process while others do the work on chunks of a data set. Testing and developing an intertwined web of microservices can be challenging. To simplify this and all the yak shaving of handling projects, dependencies and the like we decided to host all the logically related lambdas in a monorepo.

We also abstract away the way how we call lambdas from lambdas:

<!--?prettify lang=clojure linenums=true -->

    (defn invoke [target message]
      (if config/development?
        (future ((lookup-fn target) message))
        (let [json-message (cheshire.core/generate-string message)
              lambda-arn (->> (name *lambda-environment*)
                               str/upper-case
                               (format arn-pattern (get target->lambda-name-mapping target target)))]
          (log "sending" json-message "to lambda " lambda-arn)
          (lambda/invoke {:endpoint "eu-west-1"}
                          :invocation-type "Event"
                          :function-name lambda-arn
                          :payload json-message))))

To clarify makes sense to show here how `config/development?` is implemented too:

<!--?prettify lang=clojure linenums=true -->

    (def development?
      (not (System/getenv "AWS_LAMBDA_FUNCTION_NAME")))

This environment variable is set by the AWS lambda platform usually.

The point here that locally we turn our monorepo of lambdas into a small monolith. So by default we have everything at hand for the REPL driven development. Also it is easy to put a webserver between our client and the lambdas in development environment only. To summarise in development we can start up our code as a good old restful API or just play with it in the REPL while in production we do the asynchronous messaging dance to maximise performane and use the benefits of the platform.

This also demonstrates how easy it would be to switch back to a restful API with the same codebase if we need to abandon lambda land at some point for some undefined reasons.

To wrap up this section
* Vendor lock in does not seem to be a real issue for AWS lambdas.
* The monorepo approach really nicely postpones/does away with the decision of microservices vs. monolith.

### Environments, CI, logging

The above code snippet accidentally gives away an other practice. Binding a dynamic var with the environment we are in. Derriving the environment we just use the qualifier/alias feature of AWS lambdas:

<!--?prettify lang=clojure linenums=true -->

    (defn- lambda-environment [context]
      (let [fn-arn (.getInvokedFunctionArn context)]
        (name (cond
                (.endsWith fn-arn "DEV") :dev
                (.endsWith fn-arn "PROD") :prod
                (.endsWith fn-arn "STAGING") :staging
                :default :dev))))

As the above code shows aliases are part of the ARN (Amazon Resource Name) of a lambda therefore make it easy to handle environments from clients: client in development will call the development lambda entry point which in turn will call its downstream lambdas with development alias.

Using aliases makes handling deployments a breeze too. We use a CI tool, drone to build our lambdas on every commit, upload the code on S3 and update the lambda code on AWS. We also set the `DEV` alias on the latest version as part of the build. After sufficient testing to promote one, some or all lambdas to `STAGING` or `PROD` all we need to do is to move the appropriate alias to point to the `DEV` version. To perform this promotion and some other tasks we built a command line tool, you are generally much better off if the AWS console is used as less as possible.

We keep the Logging very simple. We don't buy into the logging libraries of the java ecosystem, instead we bind a few dynamic vars again and make sure that all our log messages have a reference in them to the environment and the client id the call is originated from. Our way of marshaling output and decoding input shows this:

<!--?prettify lang=clojure linenums=true -->
    (defn wrap-handler-impl
      "Marshals output and decodes input.

       The function f should accept two parameters: the logger and the request. Returned value if any
       is considered to be the response. Also binds the `*lambda-logger*`, the `*lambda-environment*`,
       and the `*request-id*` variables."
      [context f]
      (fn [in out]
        (binding [*lambda-logger* (.getLogger context)
                  *lambda-environment* (lambda-environment context)]
          (let [w (io/writer out)
                input (cheshire.core/decode-stream (io/reader in) true)]
            (binding [*request-id* (:request-id input)]
              (try
                (-> (f input)
                    (cheshire.core/encode-stream w))
                (.flush w)
                (catch Exception e
                  (log-error (str (.getMessage e) "\n") (str/join "\n    at " (.getStackTrace e)))
                  (throw e))))))))

    (defn -handleRequest [_ in out context]
      ((lu/wrap-handler-impl context execute!) in out))

Also for completeness sake here is the way how logging is implemented:

<!--?prettify lang=clojure linenums=true -->
    (defn- log-tagged [tags msg & msg-chunks]
      (let [tag-string (str/join (map #(str "[" % "]") tags))
            full-msg (str (tf/unparse time-format (time/now)) " " tag-string " -- " (str/join " " (cons msg msg-chunks)))]
        (if (bound? #'*lambda-logger*)
          (.log *lambda-logger* full-msg)
          (println "[logger missing]" full-msg))))

    (defn log-error [msg & msg-chunks]
      (apply log-tagged [*lambda-environment* *request-id* "ERROR"] msg msg-chunks))


Logging automatically gets captured by AWS CloudWatch which gives us the option to easily load the json formatted logs into an ELK installation either via directly loading it into Elasticsearch or forwarding it to a kinesis stream or a lambda for some preprocessing.

### Problems, Gotchas

There is the dreaded [concurrent executions limit](http://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html) which supposed to be there as a safety net against costs due to potential runaway or recursive functions during initial development and testing. Struggling to figure out how this makes sense being a global limit. As we have a quite a few lambdas in production now they equally get throttled if I manage to do something silly with a lambda in development/testing so to say. Also, obviously all our lambdas in constant development...

An other limit exists related to the code size. Limits are 50 Megabytes for the uberjar and 75 Gigabytes for all deployment packages. The first means that you have to watch your dependencies and perhaps exclude some transient dependencies if you don't use them. The 75 Gigabytes sounds like a lot for all deployment packages but if you don't clean up older versions of your lambdas (lambda versions are immutable!) you eventually reach this limit. To mitigiate this the best perhaps is to implement a cleanup as part of your automated build that takes your roll back strategy into consideration.

The occasional JVM warm up time perhaps put some people off to write lambdas in Java/JVM clojure/any JVM language. To mitigate this we tend to write our lambdas in ClojureScript for Node.js lately.

Just lately lambdas got landed with [environment variables](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-environment-variables-and-serverless-application-model/). Woot, woot... Only it turns out as a given lambda version is immutable you can only change the values of these variable on the latest version. Since we use aliases to identify environments as described above it would be really useful if we could tie a set of values to an alias. So `STAGING` lambda can talk to a `STAGING` database while `PROD` lambda can safely talk to a production cluster instead. Alas, that is not possible for now.

Last but not least lambdas are supposed to be pretty cheap. That is a good thing only it is quite difficult to predict your expenses, see yourself [here](https://aws.amazon.com/lambda/pricing/).
