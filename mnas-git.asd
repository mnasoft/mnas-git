;;;; mnas-git.asd

(asdf:defsystem #:mnas-git
  :description "Describe mnas-git here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-fad #:mnas-string #:mnas-list #:mnas-path) ;; 
  :components ((:file "package")
               (:file "mnas-git")))

