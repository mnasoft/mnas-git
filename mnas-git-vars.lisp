;;;; mnas-git-vars.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(progn
  (defparameter *user* (pathname-name (sb-posix:getenv "HOME")))

  (defparameter *mahine-git_dir-clisp_dir*
    (list
     (list "mnasoft-pi"    (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")                       "")
     (list "mnasoft-00"    (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")                       "")
     (list "mnasoft-dev"   (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")                       "")
     (list "MNASOFT-01"    (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")          "D:/PRG/msys32")
     (list "MNASOFT-10"    (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")          "D:/PRG/msys"  )
     (list "KO11-118383"   (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")          "D:/PRG/msys32")
     (list "N118389"       (concatenate 'string "/home/" *user* "/git--bare/")  (concatenate 'string "/home/" *user* "/quicklisp/local-projects/")              "C:/msys32")
;;;; (list "hp1.zorya.com" "/_storage/otd11/namatv/develop/"  "/_storage/otd11/namatv/develop/git/clisp/"           "/_storage/otd11/namatv/develop/git/clisp/")
     )
    "Список каждым элементом которого является список, 
относящийся к определенному хосту, состоящий из:
@begin(enum)
@item(имени хоста;)
@item(имени каталога, в котором находятся:

  - чистые репозитории git;

  - архивы чистых репозиториев git;
)
@item(имени каталога в котором находятся исходные файлы проектов common lisp;)
@item(путь к каталогу MSYS2 для os Windows.)
@end(enum)
 На основе этой переменной для конкретного хоста формируются переменные:
@begin(list)
 @item(*m-l*;)
 @item(*git-bare-dir*;)
 @item(*clisp-dir*;)
 @item(*root-prefix*;)
 @item(*clisp-dir-win*;)
 @item(*git-bare-dir-win*.)
@end(list)
 Более в дальнейших вычислениях она не участвует. ")

  (defparameter *m-i* (machine-instance)
    "Имя хоста")

  (defparameter *m-l* (mapcar #'first *mahine-git_dir-clisp_dir*)
    "Список имен хостов, участвующих в синхронизации")

  (defparameter *git-bare-dir* (second (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=))
    "Имя каталога, в котором находятся:
- чистые репозитории git;
- архивы чистых репозиториев git.
Пути как в Unix")

  (defparameter *clisp-dir* (third (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

  (defparameter *root-prefix* (fourth (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=))
    "Имя каталога, в котором располагается корень системы MSYS2")

  (defparameter *clisp-dir-win*
    (concatenate 'string *root-prefix* *clisp-dir*))

  (defparameter *git-bare-dir-win*
    (concatenate 'string *root-prefix* *git-bare-dir*)
    "Имя каталога, в котором находятся:
@begin(list)
 @item(чистые репозитории git;)
 @item(архивы чистых репозиториев git.)
@end(list)
Пути как в Windows")) 
