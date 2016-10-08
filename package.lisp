;;;; package.lisp

(defpackage #:mnas-git
  (:use #:cl #:mnas-string #:mnas-list #:mnas-path)
  (:export make-init-non-git-repos
	   make-commit-a
	   make-git-script
	   *git-dir*
	   )
  )

