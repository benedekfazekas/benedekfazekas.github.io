---
title: Clean up a messy namespace with cider and clj-refactor
date: 2015-12-06
description: Demo how to clean up a messy namespace, function using cider and clj-refactor in emacs
tags: clojure, emacs, clj-refactor, refactor
---

On [**clojureX**](https://skillsmatter.com/conferences/6861-clojure-exchange-2015) this year we almost had a panel discussion on editors; it was cancelled at the last minute unfortunately. So I thought why not publish the longer version of the screencast I prepared demonstrating how to use some [**clj-refactor**](https://github.com/clojure-emacs/clj-refactor.el/wiki) features in Emacs to clean up a messy namespace.

I highly recommend [**Parens of the dead**](http://www.parens-of-the-dead.com) to everybody by the way. The series is real joy to watch and demonstrates many things, among them is how to use Emacs with [**cider**](https://github.com/clojure-emacs/cider) and [**clj-refactor**](https://github.com/clojure-emacs/clj-refactor.el/wiki) to work with clojure code in a very effective way. On the other hand Magnar uses a certain bunch of clj-refactor features possibly because he builds his project up from scratch. In my brief demo/screencast I'd like to rather demo some features which are very helpful in an other typical refactoring scenario: when you want to clean up an already existing (legacy?!) namespace.

Also both cider and clj-refactor has so many features now that discoverability is an issue. This screencast and the [**demonstrated use**](https://github.com/clojure-emacs/clj-refactor.el/wiki/hydra-code) of [**hydras**](https://github.com/abo-abo/hydra) tries to offer some remedies for that too.

<iframe width="630" height="473" src="https://www.youtube.com/embed/mOSUE3czp9w?rel=0&vq=large" frameborder="0" allowfullscreen></iframe>

#### `ml` move to let ####
Move the current form to the closest `let`. More details [**here**](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let). This is an elisp only feature meaning that you don't even need a running REPL to use it.

#### `rs` rename (local) symbol ####
Rename all occurrences of a local symbol. More details [**here**](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-symbol#or-a-locally-defined-symbol). This is a feature which uses the analyzer to figure out the location of the local symbol therefore you need a running REPL with [**refactor-nrepl**](https://github.com/clojure-emacs/refactor-nrepl) middleware added to it.

#### `is` inline symbol ####
Inline all occurrences of the symbol at point. Can be used both globally and locally. See screencast gif [**here**](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-inline-symbol). Again this is feature needing the analyzer to figure out the location of the occurrences of the symbol you want to inline.

#### `pf` promote function ####
Can promote `#(foo %)` style anonymus function to `(fn [arg1] (foo arg1))` or all the way to first level `defn` or a `(fn [arg1] (foo arg1))` style anonymus function into a first level defn. See details [**here**](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-promote-function#promote-anonymous-function-to-defn). Again this needs the analyzer.

#### `ef` extract function ####
Extract the form at point, or the nearest enclosing form, into a toplevel defn. Newly created function is either private or public, depending on `cljr-favor-private-functions`. See a screencast [**here**](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function). This needs the analyzer as well to figure out which locally bound vars the s-expression uses that need to be turned into arguments in the new function.
