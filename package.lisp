;;;; ./package.lisp

(defpackage :mnas-git
  (:documentation
   "Mnas-Git. Проект предназначен для безсетевого способа
  синхронизации проектов Common Lisp между различными персональными
  компьютерами.")
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
  (:export invoke-git))


(in-package :mnas-git)
