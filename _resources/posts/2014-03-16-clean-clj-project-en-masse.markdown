---
title: Clean clojure files en masse
date: 2014-03-16
description: How to clean clojure project files in bulk with clj-refactor
tags: clojure, emacs-lisp, elisp, emacs, clj-refactor
---

[**clj-refactor**](https://github.com/magnars/clj-refactor.el) is a very lightweight elisp library for Emacs to support every day refactorings for clojure.  I've even read that it is an [über-paredit](https://twitter.com/cemerick/statuses/431608319552548864): kinda fair.  It definitely melds into editing experience defined by the mix of cider, clojure-mode, paredit well.  Adopting it you get loads of nice transformations which help you with your every day clojure coding.  There are simpler ones like **add require to namespace declaration**  `ar` and **cycle collection type** `cc` or **cycle privacy**  `cp`.  This latter sounds silly first: why an earth you would not just go to your `defn` and add that '-' to make it private.  But once your muscle memory has remembered `cp` it is just natural to turn a function private with one easy key combo when you figured out it does not make sense to keep it public -- you don't even need to think about what to press.  There are more arcane ones which give the wtf-just-happened-with-my-code experience first but then they start making absolute sense.  The threading macro related ones for example: **wrap in thread first** `tf` and **fully unwind threaded expression** `ua` or the **introduce let**, **expand let**, **move to let** triumvirate.  Used in a correct way the latter one helps you to handle let expressions in a very clean way across your code.  These are only example of course, check out the full list on the project's really cleanly written github readme.

Just a side note on the shortcuts.  Well, that is the usual Emacs story.  People tend to freak out: 'not that many extra key combination I need to learn again'.  I find that my muscle memory kicks in very fast and what is even more helpful it is contextual.  I don't even try certain key combos when in the repl or when editing a text file.  So after a bit of practice you won't need to think how to move the form after the cursor to the already existing let expression.

I also mentioned that **clj-refactor** is lightweight.  It is in the sense that it does not use any code analyzer library or such and it does not enforce you to use [**cider**](https://github.com/clojure-emacs/cider) either.  (Cider still can come in handy.  For example you can use completion when adding something to the require section in your ns declaration.) It is an emacs-lisp library -- started out as tiny -- which uses some other elisp libraries like paredit, yasnippet, dash.el, s.el, multiple cursors (the good work of [Magnar Sveen](https://github.com/magnars) and others just like **clj-refactor** itself).  So **clj-refactor** does not really 'understand' your code, but by using paredit it does understand the structure of your clojure files.  This with some more elisp foo gives it enough power to be extremely handy: **clj-refactor** is not so tiny anymore.  There are plans to add some code analyzer to the mix but to be honest I would prefer at least to keep that if ever added as an optional feature even with the drawback of falling back to simpler behaviour.  Lightweight means fast and speed is extremely important in order to avoid the productivity killer **IDE** experience.

That leads me to the next group of offered refactorings, the ones which need to be performant because it makes sense to run them against a whole bunch of clojure files in one go.  These are the 'clean up', house keeping features:

- **replace `use` with `:refer :all`**
- **sort use, require and import in your ns form**
- **remove unused requires**

When working on a sort of larger clojure project or multiple of those or as a member of a team of six, eight plus developers you might find running these against a whole project periodically a really nice thing as it helps fixing the [broken window](http://blog.codinghorror.com/the-broken-window-theory/) effect.  I personally am not really a big fan of coding conventions, rules of how to write code or code style checkers but the number of namespaces which have unused requires is a good indicator of how much the code started to get disorganised.  Better to fix those as fast as possible.

Here comes a small elisp function which does the trick:

<!--?prettify lang=emacs-lisp-->

    (defun cleanup-project-clj-files ()
      (interactive)
      (dolist (filename (cljr--project-files))
        (when (s-ends-with? "clj" filename)
          (find-file filename)
          (ignore-errors (cljr-remove-unused-requires))
          (save-buffer)
          (kill-buffer))))

Some quick points about the above code.  It uses **clj-refactor**'s own `cljr--project-files` to find the clojure files.  There are other ways but I thought the best is to use this as we are anyway using this library.  One caveat that it will only work with leiningen managed projects for now.  There are plans in the project as far as I know to eventually improve this.  When that is done the above script will start working for any kind of projects.  `ignore errors` is needed because there is an error thrown if the given file does not have an ns declaration (as the project file itself does not).  `save buffer` might also trigger some additional clean up if you some hooks defined on save.  For example my Emacs configuration turns all tabs into spaces when I save.  Adding the above function to your Emacs is easy: either just copy it into your Emacs init file or save it in a separate file and make sure that file is loaded via your `init.el`.  If that is done you can just open any files in the project and `M-x cleanup-project-clj-files`.

I saw something very similar originally in [Alex Baranosky](https://github.com/AlexBaranosky)'s Emacs config repo on github and simplified it a bit.  Also lots of information in the post comes from Alex and the small team around **clj-refactor** while chatting with them about my pull requests.  A very nice, fun team around a fun project.
