;;;; package.lisp

(defpackage #:mnas-git
  (:use #:cl #:mnas-string #:mnas-list #:mnas-path)
  (:export command
	   init
	   commit-a
	   clone--bare
	   clone--origin
	   remote-readd))
