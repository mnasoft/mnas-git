;;;; ./package.lisp

(defpackage :mnas-git
  (:documentation
   "
   @b(Описание:) проект @b(Mnas-Git) предназначен для безсетевого способа
  синхронизации проектов Common Lisp между различными персональными
  компьютерами")
  (:nicknames "MGIT")
  (:use #:cl #:mnas-string #:mnas-path)
  (:export init
           commit-a 
           command 
           sh-command 
           clone--bare 
           dialog-remote-readd 
           remote-readd 
           clone--origin 
           man 
           help)
  (:export invoke-git
           *mnasoft-git*))


(in-package :mnas-git)
