;;;; mnas-git-test.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(git-init)

(git-command "git status" t)

(git-commit-a t)

(git-command (concatenate 'string "git push" " "  *m-i* " " "master"))

(make-git-script *clisp-dir* "git pull other master")

(make-git-script *clisp-dir* "git remote remove hp1.zorya.com")

(progn 
  (make-git-script *clisp-dir* "git add *.lisp *.txt *.asd" )
  (make-commit-a *clisp-dir*)
  (make-git-script *clisp-dir* "git push share master")
  )

(progn
  (make-git-script *clisp-dir* "git remote remove share")
  (make-git-clone--bare (concatenate 'string *git-dir* "git/")
			(concatenate 'string *git-dir* "git-share/") *clisp-dir* "share" "git-clone--bare-share.sh")
  )

(progn
  (make-git-script *clisp-dir* "git remote remove other")
  (make-git-clone--bare (concatenate 'string *git-dir* "git/")
			(concatenate 'string *git-dir* "git-other/")  *clisp-dir* "other" "git-clone--bare-other.sh")
  )

(git-remote-add t) ;;;; Добавление удаленных чистых репозиториев

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(directory-namestring  #P"/home/namatv/develop/git/clisp/mnas-git/")

(length (find-filenames-directory *clisp-dir* "\"*.lisp\""))

(length (find-filenames *clisp-dir* "\"*.asd\""))

(find-filenames *clisp-dir* "\".git\"")

(directory-pathname-p (read-line isstr))

(uiop:directorize-pathname-host-device (read-line isstr))

(pathname-directory (uiop:directorize-pathname-host-device (read-line isstr)))

(pathname-type (uiop:directorize-pathname-host-device (read-line isstr)))

(pathname-name (uiop:directorize-pathname-host-device (read-line isstr)))

(pathname-encoding-name (uiop:directorize-pathname-host-device (read-line isstr)))

(file-namestring  (uiop:directorize-pathname-host-device (read-line isstr)))
