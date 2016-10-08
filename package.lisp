;;;; package.lisp

(defpackage #:mnas-git
  (:use #:cl #:mnas-string #:mnas-list #:mnas-path)
  (:export git-command
	   git-init
	   git-commit-a
	   git-clone--bare
	   git-remote-readd))
