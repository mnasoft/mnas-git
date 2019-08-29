;;;; package.nlisp

(defpackage #:mnas-git)

(defpackage #:mnas-git
  (:documentation "Mnas-Git. Проект предназначен для безсетевого способа синхронизации проектов Common Lisp между различными персональными компьютерами.")
  (:nicknames "MGIT")
		  
  (:use #:cl #:mnas-string #:mnas-list #:mnas-path)
  (:export mnas-git::command
	   mnas-git::sh-command
	   mnas-git::init
	   mnas-git::commit-a
	   mnas-git::clone--bare
	   mnas-git::clone--origin
	   mnas-git::remote-readd
	   )
  (:export mnas-git::man
	   mnas-git::help))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
