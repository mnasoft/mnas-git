;;;; mnas-git.asd

(defsystem #:mnas-git
  :description "Mnas-Git. Проект предназначен для безсетевого способа синхронизации проектов Common Lisp между различными персональными компьютерами."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (#:cl-fad #:mnas-string #:mnas-path #:mnas-file-dialog) 
  :components ((:file "package")
	       (:file "mnas-git-vars")
               (:file "mnas-git")))
