;;;; package.nlisp

(defpackage :mnas-git
  (:documentation "Mnas-Git. Проект предназначен для безсетевого способа синхронизации проектов Common Lisp между различными персональными компьютерами.")
  (:nicknames "MGIT")
  (:use #:cl #:mnas-string #:mnas-path)
  )

(in-package :mnas-git)
