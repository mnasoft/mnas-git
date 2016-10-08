;;;; mnas-git-test.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(git-init)

(git-command "git status" t)

(git-remote-readd t)

(git-commit-a t)

(git-command (concatenate 'string "git push" " "  *m-i* " " "master") t )

