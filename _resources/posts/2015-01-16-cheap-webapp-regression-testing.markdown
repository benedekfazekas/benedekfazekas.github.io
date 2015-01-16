---
title: Webapp regression testing for cheap
date: 2015-01-16
description: Regression testing of webapps by comparing html output as data
tags: testing, regression, tdd, CI, clojure, enlive, MailOnline
---

An automated regression test suite is a very valuable asset, however most of the times it [does not come cheap](http://www.pitheringabout.com/?p=995&cpage=1#comment-21254). You need to write your unit and/or functional tests and add them to the regression suite and then, and this the real pain: maintain them. Almost every single refactoring means some work on the test code too. We've found a way around this at the MailOnline, and get a really valuable regression test suite for cheap. Perhaps it worths to share the details.

Our regression test suite was not originally created as such. We aimed to solve a different testing problem. Since our project started off as a rewrite project, ie. rewriting the application producing the MailOnline website writen originally in java and spring MVC in clojure, the problem was how to make sure and prove that we producing the same output. Mind you, even defining what is the same output here is not trivial as we did not care about whitespaces or other not really important, minor differences in the html output. Later we realised that the same idea can be reused to catch regressions and solve other testing problems too. More on this later.

The key idea of solving this was to use [enlive](https://github.com/cgrand/enlive) for two crucial features:

- its ability to turn html into data structures we can work with easily in clojure,
- its ability to write selectors to easily filter out parts of html we are interested in.

With this at hand it is quite staright forward what we need to do:

1. parse the html output for both the legacy and the new application
1. select some parts of the html from both the legacy's and the new application's output. We can even drill down to certain attributes
1. preprocess the selected data optionally. For example for an `img` tag we are only interested in the `src` and `class` attributes: the other attributes are not crucial. Or can munge the data a bit, let's say because we have timestamps here and there: they won't be exactly the same ever.
1. compare the preselected and preprocessed data
1. produce some human or machine readable output of the comparison

In fact we came up with a protocol defining what we exactly need to do after the html is parsed already:

<!--?prettify lang=clojure linenums=true -->

    (ns fe-adm.ab-test.detective)

    (defprotocol DetectiveProtocol
      "An investigator for A/B testing. Understands what parts of a page are
       interesting and how to compare those parts"
      (title [self]
        "Descriptive id")

      (extract [self page-html]
        "Extract the interesting part of the page")

      (keyfn [self]
        "Prime elements for use in sort-by")

      (render [self a-html b-html]
        "Print HTML diff")

      (render-unit [self a-html b-html]
        "Print unit compliant xml")

      (diff [self a b]
        "Calculate missing and surplus"))

One simple example of using this protocol for checking divs across the page:

<!--?prettify lang=clojure linenums=true -->

    (ns fe-adm.ab-test.div
      (:require [fe-adm.ab-test.detective :refer :all]
                [net.cgrand.enlive-html :as html]
                [fe-adm.ab-test.utils :as ab-utils]))

    (defn- ignore-divs [div]
      (empty? (-> div :attrs)))

    (defn- munge-divs [div]
      (-> div
          (ab-utils/trim-attrs-str :class)
          (ab-utils/replace-attrs-str :id #"^.*$" "{{some-id}}")))

    (defrecord Detective []
      DetectiveProtocol
      (title [self] :div)

      (extract [self html]
        (->> (html/select html [:div])
             (remove ignore-divs)
             (map :attrs)
             (map munge-divs)
             set))

      (keyfn [self]
        (fn [attrs] (vec (sort (dissoc attrs :content)))))

      (render [self a-html b-html]
        (ab-utils/render-diff self a-html b-html))

      (render-unit [self a-html b-html]
        (ab-utils/render-unit-xml self a-html b-html))

      (diff [self a b]
        (clojure.set/difference a b)))

Note that the really interesting part happens in line 18. `extract` function. All `div` tags are selected although some are ignored (this part could be done by enhancing the selector actually). Then the value of the class attribute is trimmed: bit of clean up. And since div ids are uuid-s of some sort they are replaced with a constant so they won't fail the test needlessly.

As I said above after going live with the new application and retiring the legacy one we quickly realised that we can reuse the same idea and code for regression testing. We put together a box in production (not actually serving real traffic) getting a build for every single commit and compare its output with our production output. Presto, we have a regression test suite catching possible differences in our output for every single commit. If there is a difference we need to check if the change was intentional: we use our CI tool feature of muting failed tests for that. Easy to do as you can see from the `render-unit` function we are producing unit compliant output so our CI tool nicely listed our passed and failed tests.

Bigger pieces of work, refactorings frequently done on branches. With this approach it was easy to push a build on our prod test box from the branch and test the produced output against the production output to get some feedback of the state of the refactoring.

An other use case is when we decided to set up an other cluster of our application in a new data center. We just run this test suite comparing one of the new node's output with the output of one of our live node's. Again presto: we were able to say when the new cluster was ready to server real traffic.

In retrospective my main take away from this story is how much value you can get for how less if you take the context of your application into consideration when you create your test suits.
