---
title: Java8 stream API vs Clojure fizzbuzzed
date: 2015-02-06
description: Play a bit with fizzbuzz to try java8 stream API and compare with vanilla clojure
tags: java, java8, clojure, streams, streamAPI, fizzbuzz, fp
---

Lately I came across a vanilla java solution for the good old [**fizzbuzz**](http://en.wikipedia.org/wiki/Fizz_buzz) problem. This gave me the idea to try (finally) the java8 stream API: fizzbuzz seemed a small but fitting problem. And while at it why not code up a solution in clojure too so I can compare the two. This post documents the one afternoon adventure.

Let's also spice fizzbuzz up a bit with adding two extra requirements:

    - if the number in question has 3 in it we print 'bingo'
    - at the end we print a kind of statistics where we show the number of occurrences of 'Fizz', 'Buzz', 'FizzBuzz', 'Bingo' and not changed numbers

Please note that although the first one is just an other condition it is also an other type of condition not related to any modulo of or any arithmetic operation with the given number. The second is interesting too because the solution needs say something about the transformed list. A bit of state might need to be maintened while doing the transformation, right? ;)

So first of all as a starting point let's see the vanilla solution in java:

<!--?prettify lang=java linenums=true -->

    public class FizzBuzz {

        public void fizzBuzz(int rangeStart, int rangeEnd, PrintStream out) {

            Map<String, AtomicInteger> statMap = new HashMap<String, AtomicInteger>();

            List<String> transformedNumbers = new ArrayList<String>();
            for(int i = rangeStart; i <= rangeEnd; i++){
                String elem = "Integer";
                String iAsString = String.valueOf(i);
                if (iAsString.contains("3")) {
                    elem = "Bingo";
                } else if (i % 15 == 0){
                    elem = "FizzBuzz";
                } else if(i % 3 == 0){
                    elem = "Fizz";
                } else if(i % 5 == 0){
                    elem = "Buzz";
                }
                transformedNumbers.add(("Integer".equals(elem))? iAsString : elem);
                statMap.computeIfAbsent(elem, k -> new AtomicInteger(0));
                statMap.get(elem).getAndIncrement();
            }

            out.println(transformedNumbers);
            out.println();
            out.println(statMap);
        }
    }

Pretty naive approach but hey-ho we just started. Let's see the (kinda) equivalent in clojure:

<!--?prettify lang=clojure linenums=true -->

    (defn fizzbuzz [start end]
      (letfn [(calc-stats [cmap elem] (let [key (if (string? elem) (keyword elem) :integer)]
                                        (update-in cmap [key] #(if % (inc %) 1))))]
        (let [fizzbuzzed (->> end
                              inc
                              (range start)
                              (map #(cond
                                     (.contains (str %) "3") "bingo"
                                     (= (mod % 15) 0) "fizzbuzz"
                                     (= (mod % 3) 0) "fizz"
                                     (= (mod % 5) 0) "buzz"
                                     :default %)))
              stats (reduce calc-stats {} fizzbuzzed)]
          [fizzbuzzed stats])))

Just to point out the obvious differences in clojure you don't need to iterate through the list when fizzbuzzing it: just give a recipe in the anonymous function (line 7.) how to fizzbuzz a given element. In the java code you iterate through all the elements and update a bit of local state as a side effect to get the new list and the statistics. The usage of a REPL is obvious too when you look at the clojure code. You don't really need to worry about how to print your results while developing, playing with the code: the REPL will take care of it. My last point is types: in java you have to convert the original int to string while clojure being a dynamic language you are fine with a list with partly string and partly number elements.

In clojure after you had done the fizzbuzzing you can easily reduce the new list into the stats. That is a bit awkward though, you might want a solution which does the stats and the fizzbuzzing in one step. Also using the `cond` is equivalent with the java `if`/`else-if`/`else` construct but not really nice either. So bit of work on these points:

<!--?prettify lang=clojure linenums=true -->

    (defn- fizzbuzz&stats [list&stats n]
      (let [mod-pred (fn [n word] #(when (= (mod % n) 0) word))
            safe-inc (fn [n] (if n (inc n) 1))]
        (if-let [s (some #(% n) [#(when (.contains (str %) "3") "bingo")
                                 (mod-pred 15 "fizzbuzz")
                                 (mod-pred 3 "fizz")
                                 (mod-pred 5 "buzz")])]
          (-> list&stats
              (assoc :list (concat (:list list&stats) (seq [s])))
              (update-in [(keyword s)] safe-inc))
          (-> list&stats
              (assoc :list (concat (:list list&stats) (seq [n])))
              (update-in [:integer] safe-inc)))))

    (defn fizzbuzz-enhanced [start end]
      (->> end
           inc
           (range start)
           (reduce fizzbuzz&stats {})))

The fizzbuzzed list and the stats is constructed in one reduce. Also eliminated the `cond` and use `some` with a vector of predicates: the first which applies wins and returns the fizzbuzz string for the element. If there is no winner stick with the original number. And of course either case add the element after fizzbuzzing to the result list inside the result hash. Reduce takes care of passing a fresh version of it to the next round.

Now it is time to get to the cake: code up something similar using the java8 stream API. I am sure there are other nice, very likely better solutions, don't hesitate to ping them to me on twitter ;)

<!--?prettify lang=java linenums=true -->

    public void fizzBuzz(int rangeStart, int rangeEnd, PrintStream out) {

        String[] fizzbuzzed = rangeClosed(rangeStart, rangeEnd).mapToObj(FizzBuzz::intToFizzBuzz).toArray(String[]::new);
        System.out.println("start ---");
        System.out.println(Arrays.toString(fizzbuzzed));
        System.out.println("end   ---");
        System.out.println("stats start ---");
        System.out.print(Arrays.stream(fizzbuzzed).collect(Stats::new, Stats::accept, Stats::combine));
        System.out.println("stats end   ---");
    }

    class Stats implements Consumer<String> {

        private int lucky, fizz, buzz, fizzbuzz, integer = 0;

        public void accept(String elem) {
            if ("lucky".equals(elem)) lucky++;
            else if ("fizz".equals(elem)) fizz++;
            else if ("buzz".equals(elem)) buzz++;
            else if ("fizzbuzz".equals(elem))
                fizzbuzz++;
            else
                integer++;
        }

        public void combine(Stats other) {
            lucky += other.lucky;
            fizz += other.fizz;
            buzz += other.buzz;
            fizzbuzz += other.fizzbuzz;
            integer += other.integer;
        }

        public String toString() {
            return "lucky: " + lucky +
                "\n fizz: " + fizz +
                "\n buzz: " + buzz +
                "\n fizzbuzz: " + fizzbuzz +
                "\n integer: " + integer;
        }
    }

    public static String intToFizzBuzz(int nr) {
        if (String.valueOf(nr).contains("3")) return "lucky";
        if (nr > 0 && nr % (3 * 5) == 0) return "fizzbuzz";
        if (nr > 0 && nr % 3 == 0) return "fizz";
        if (nr > 0 && nr % 5 == 0) return "buzz";
        return String.valueOf(nr);
    }

Couple of notes. This solution is rather equivalent of the first iteration of clojure as it first creates the fizzbuzzed list and the reduces/consumes it to calculate the statistics. That is a bit awkwardly though as a stream which you run a terminal operation against you can't consume again. Although this makes sense it also means that the stream needs to be recreated for the reduction into the statistics because reducing it for printing is also a terminal operation. In Clojure there is no such problem as all the collections are persistent therefore you can easily reuse them. See the first clojure iteration code: the fizbuzzed list can be safely returned **and** reduced into the statistics.

That said the stream API felt something I would not mind to work with on a more realistic problem. And mainly because of this API I tend to agree with Sean Corfield:

> I think I can safely say that I would no longer rather fall on a sword than program in Java – as long as it’s Java 8!

[from [**here**](http://seancorfield.github.io/blog/2014/06/20/some-thoughts-on-java-8/)]

As wrapping up I mention two other fizzbuzz solutions. One I came across on the [**rosette code**](http://rosettacode.org/wiki/FizzBuzz) and found it really elegant:

<!--?prettify lang=clojure linenums=true -->

    (map #(let [s (str %2 %3) ] (if (seq s) s (inc %)))
            (range)
            (cycle ["" "" "Fizz"])
            (cycle ["" "" "" "" "Buzz"]))

I think this is my favourite as it does not even use `mod` just simply exploits positions in lists and the fact that `map` can go through multiple collections. Also laziness shines through this example: just mind blowingly awesome.

The [**other solution**](https://github.com/EnterpriseQualityCoding/FizzBuzzEnterpriseEdition) shows how much fizzbuzz is a rabbit hole and also well... funny? sad? humbling? ;)
