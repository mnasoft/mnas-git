;;;; package.lisp

(defpackage #:mnas-git
  (:use #:cl #:mnas-string #:mnas-list #:mnas-path)
  (:export command
	   init
	   commit-a
	   clone--bare
	   clone--origin
	   remote-readd)
  (:export help))
;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
