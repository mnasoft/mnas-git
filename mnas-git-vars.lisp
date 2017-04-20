;;;; mnas-git-vars.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(defparameter *mahine-git_dir-clisp_dir*
  '(("mnasoft-pi"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"           "/home/namatv/develop/git/clisp/")
    ("mnasoft-00"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"           "/home/namatv/develop/git/clisp/")
    ("MNASOFT-01"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"           "c:/msys64/home/namatv/develop/git/clisp/")
    ("hp1.zorya.com" "/_storage/otd11/namatv/develop/" "/_storage/otd11/namatv/develop/git/clisp/" "/_storage/otd11/namatv/develop/git/clisp/")
    ("KO11-118383"   "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"           "D:/PRG/msys32/home/namatv/develop/git/clisp/")))

(defparameter *m-i* (machine-instance))

(defparameter *m-l* (mapcar #'first *mahine-git_dir-clisp_dir*))

(defparameter *git-dir* (second (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

(defparameter *clisp-dir* (third (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

(defparameter *sh-dir* (fourth (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))
