;;;; mnas-git.asd

(defsystem #:mnas-git
  :description "Describe mnas-git here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (#:cl-fad #:mnas-string #:mnas-list #:mnas-path) ;; 
  :components ((:file "package")
	       (:file "mnas-git-vars")
               (:file "mnas-git")))

