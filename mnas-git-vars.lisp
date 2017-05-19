;;;; mnas-git-vars.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(defparameter *mahine-git_dir-clisp_dir*
  '(("mnasoft-pi"    "/home/namatv/develop/"           "/home/namatv/quicklisp/local-projects/clisp/"      "/home/namatv/quicklisp/local-projects/clisp/")
    
    ("mnasoft-00"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"                   "/home/namatv/develop/git/clisp/")
    ("MNASOFT-01"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"                   "e:/PRG/msys64/home/namatv/develop/git/clisp/")
    ("hp1.zorya.com" "/_storage/otd11/namatv/develop/" "/_storage/otd11/namatv/develop/git/clisp/"         "/_storage/otd11/namatv/develop/git/clisp/")
    ("KO11-118383"   "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/"                   "D:/PRG/msys32/home/namatv/develop/git/clisp/"))
  "Список каждым элементом которого является список, относящийся к определенному хосту,
состоящий из:
1 - имени хоста;
2 - имени каталога в котором расположен корень разработки;
3 - имени каталога в котором находятся исходные файлы проектов common lisp;
4 - имени каталога в котором сохраняются пути к сценариям bash.
На основе данной переменной для конкретного хоста формируются переменные 
*m-l*, *git-dir*, *clisp-dir* , *sh-dir*.
Более в дальнейших вычислениях она не участвует.")

(defparameter *m-i* (machine-instance)
  "Имя хоста")

(defparameter *m-l* (mapcar #'first *mahine-git_dir-clisp_dir*)
  "Список имен хостов, участвующих в синхронизации")

(defparameter *git-dir* (second (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

(defparameter *clisp-dir* (third (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

(defparameter *sh-dir* (fourth (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))
