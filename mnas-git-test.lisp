;;;; mnas-git-test.lisp

(in-package :mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(git-init)

(git-command "git status" t)

(git-remote-readd t)

(git-commit-a t)

(git-command (concatenate 'string "git push" " "  *m-i* " " "master") t )

;;;; 


*git-bare-dir*          "/home/namatv/git--bare/"
*clisp-dir*             "/home/namatv/quicklisp/local-projects/"
*root-prefix*           "d:/prg/msys32"
*clisp-dir-win*         "D:/PRG/msys32/home/namatv/quicklisp/local-projects/"  
*git-bare-dir-win*      "D:/PRG/msys32/home/namatv/quicklisp/local-projects/"

