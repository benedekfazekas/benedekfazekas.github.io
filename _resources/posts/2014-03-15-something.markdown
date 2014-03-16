---
title: some blog post enhanced
date: 2014-03-15
description: just some post
tags: clojure, elisp, emacs
---

some hello world *text*

A paragraph:

- bullet
- points

this is some clojure:

<!--?prettify lang=clj-->

    (fn [system]
      (clj-components.bootstrap/init!
         system system system
         (merge {
           :http-handler #'fe.web/handler
           :http-request-logs? true
           :nrepl-port 7888
           :loggers [{:ns 'fe :level :info}]}
          bootstrap-args)
         fe.system.component-registry/constructors))

this is some elisp:

<!--?prettify lang=emacs-lisp-->

    (defun cljr--delete-and-extract-sexp-with-nested-sexps ()
      "Returns list of strings representing the nested sexps if there is any.
       In case there are no nested sexp the list will have only one element.
       Not recursive, does not drill down into nested sexps
       inside the first level nested sexps."
      (let* ((beg (point))
             (sexp-start beg)
             (end (progn (paredit-forward)
                         (point)))
             nested)
        (paredit-backward)
        (paredit-forward-down)
        (while (/= sexp-start end)
          (paredit-move-forward)
          (push (s-trim (buffer-substring sexp-start (point))) nested)
          (setq sexp-start (point)))
        (delete-region beg end)
        (nreverse (cons (concat (nth 1 nested) (car nested)) (or (nthcdr 2 nested) '())))))
