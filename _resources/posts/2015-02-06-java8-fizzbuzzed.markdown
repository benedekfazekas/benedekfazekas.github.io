---
title: Java8 stream API and clojure compared with fizzbuzz
date: 2015-02-06
description: Play a bit with fizzbuzz to try java8 stream API and compare with vanilla clojure
tags: java8, clojure, streams, streamAPI, fizzbuzz
---

Lately I came across a vanilla java solution for the good old [**fizzbuzz**]() problem. This gave me the idea to try (finally) the java8 stream API: fizzbuzz seemed a small but fitting problem. And while at it why not code up a solution in clojure too so I can compare the two. This post documents the one afternoon adventure.

Lets also spice fizzbuzz up a bit with adding two extra requirements:

    - if the number in question has 3 in it we print 'bingo'
    - at the end we print a kind of statistics where we show the number of occurences of 'Fizz', 'Buzz', 'FizzBuzz', 'Bingo' and not changed numbers

Please note that although the first one is just an other condition it is also an other type of condition not related to any modulo of or any arithmetic operation with the given number. The second is interesting too because the solution needs say something about the transformed list. We might need to maintain a bit of state while doing the transformation, or should we?

So first of all as a starting point let's see the vanilla solution in java:

<!--?prettify lang=java linenums=true -->

    public class FizzBuzz {

        public void fizzBuzz(int rangeStart, int rangeEnd, PrintStream out) {

            Map<String, AtomicInteger> statMap = new HashMap<String, AtomicInteger>();
            statMap.put("Bingo", new AtomicInteger(0));
            statMap.put("Fizz", new AtomicInteger(0));
            statMap.put("Buzz", new AtomicInteger(0));
            statMap.put("FizzBuzz", new AtomicInteger(0));
            statMap.put("Integer", new AtomicInteger(0));

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
                statMap.get(elem).getAndIncrement();
            }

            out.println(transformedNumbers);
            out.println();
            out.println(statMap);
        }
      }

Pretty naive approach but heyho we just started. Let's see the equivalent in clojure:

<!--?prettify lang=clojure linenums=true -->

    (defn fizzbuzz [start end]
      (letfn [(rplace [preds n] (if-let [s (some #(% n) preds)] s n))
              (mod-pred [n word] #(when (= (mod % n) 0) word))
              (count-occ [cmap elem] (let [key (if (string? elem) (keyword elem) :integer)]
                                       (update-in cmap [key] inc)))]
        (let [fizzbuzzed (->> end
                              inc
                              (range start)
                              (map (partial rplace [#(when (.contains (str %) "3") "bingo")
                                                    (mod-pred 15 "fizzbuzz")
                                                    (mod-pred 3 "fizz")
                                                    (mod-pred 5 "buzz")])))
              stats (reduce count-occ {:fizz 0
                                       :buzz 0
                                       :fizzbuzz 0
                                       :bingo 0
                                       :integer 0} fizzbuzzed)]
          [fizzbuzzed stats])))
