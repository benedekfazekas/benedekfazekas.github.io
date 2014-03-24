---
title: Clean clojure files en masse continued
date: 2014-03-24
description: Clean clojure project files in bulk with clj-refactor part 2
tags: clojure, emacs-lisp, elisp, emacs, clj-refactor
---

The project clean mentioned at the end of the previous post got integrated into [**clj-refactor**](https://github.com/magnars/clj-refactor.el) and changed a bit during the process. Just a quicky about the code.

<!--?prettify lang=emacs-lisp linenums=true -->

    (defun cljr-project-clean ()
      (interactive)
      (when (or (not cljr-project-clean-prompt)
                (yes-or-no-p "Cleaning your project might change many of your clj files. Do you want to proceed?"))
        (dolist (filename (cljr--project-files))
          (when (s-ends-with? "clj" filename)
            (let ((buffer (get-file-buffer filename))
                  find-file-p)
              (if buffer
                  (set-buffer buffer)
                (setq find-file-p t)
                (find-file filename))
              (ignore-errors (-map 'funcall cljr-project-clean-functions))
              (save-buffer)
              (when find-file-p
                (kill-buffer)))))))

There are a few small changes basically just to make the this more configurable and more comfortable to use.

1. **Line 3**: There is a `defcustom` controlling if **clj-refactor** should make sure that we want to proceed with a possible destructive change (en masse). Easy to get rid of this: `(setq cljr-project-clean-prompt nil)` if we know what we are doing.
1. **Line 7**: **clj-refactor** nicely tries to find the buffer first. If it is found it uses it and then won't get killed when all the changes are done (see **line 15**): there is a good guy. Otherwise we open the file with `find-file` and kill it when done. Would be interesting to find a way to use `temp-buffer` and/or `temp-file` actually.
1. **Line 13**: There is an other `defcustom` to configure the list of functions to run on a given clojure file in the project. By default this is removing unusued requires and sort ns forms but easy to remove one of these or add others, even perhaps some project dependent cleaning functions implemented in the project itself.

Hope that really helps out there to fix some broken windows.
