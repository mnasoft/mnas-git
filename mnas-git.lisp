;;;; mnas-git.lisp

(in-package #:mnas-git)

;;; "mnas-git" goes here. Hacks and glory await!

(defparameter *mahine-git_dir-clisp_dir*
  '(("mnasoft-pi"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/")
    ("mnasoft-00"    "/home/namatv/develop/"           "/home/namatv/develop/git/clisp/")
    ("hp1.zorya.com" "/_storage/otd11/namatv/develop/" "/_storage/otd11/namatv/develop/git/clisp/")
    ("KO11-118383"   "/d/home/_namatv/develop/"        "/d/home/_namatv/develop/git/clisp/")))

(defparameter *m-i* (machine-instance))

(defparameter *machine-list* (mapcar #'first *mahine-git_dir-clisp_dir*))

(defparameter *git-dir* (second (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

(defparameter *clisp-dir* (third (assoc *m-i* *mahine-git_dir-clisp_dir* :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cd-path (path &optional (os t))
  (format os "cd ~A~%" (directory-namestring path))
  (format os "echo -e \"\\033[1;31m~A\\033[0m\"~%" (directory-namestring path)))

(defun preamble-bash(&optional (os t))
  (format os "#!/bin/bash~%"))

(defun decoded-time-out (&optional (os nil))
  (multiple-value-bind (ss mm hh day mon year) (get-decoded-time)
    (format os "~A-~A-~A ~A:~A:~A" year mon day hh mm ss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-filenames (path name)
  "Осуществляет поиск файлов, удовлетворяющих маске поиска name
начиная с каталога path вглубь дерева каталогов;
Возвращает список полных путей к найденным файлам;
Пример использования:
;;(find-filenames \"/_storage/otd11/namatv/git/clisp/\" \"*.asd\")
"
  (let ((osstr (make-string-output-stream)))
    (uiop:run-program (concatenate 'string "find " path " -name " name) :output osstr :ignore-error-status t)
    (do* ((isstr (make-string-input-stream (get-output-stream-string osstr)))
	  (rez nil)
	  (line (read-line isstr nil 'eof) (read-line isstr nil 'eof)))
	 ((eql line 'eof) (reverse rez))
      (setf rez (cons line rez)))))

(defun find-filenames-directory (path name)
  "Возвращает список директорий
- элементы которого не повторяются;
- в которых удалось найти файлы удовлетворяющие шаблону name;
- поиск файлов начинается с каталога path;
Пример использования:
;;;;(find-filenames-directory *clisp-dir* \"*.lisp\")
"
  (unique
   (mapcar
    #'(lambda (el)
	(make-pathname
	 :directory
	 (pathname-directory
	  (uiop:directorize-pathname-host-device el))))
    (find-filenames path name)) :test #'equal))

(defun find-filenames-directory-clisp-git()
  (find-filenames-directory *clisp-dir* ".git"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-not-giting-lisp-projects (path)
  "Осуществляет рекурсивный поиск лисп-проектов *.asd 
вглубь дерева каталогов, для которых в каталоге их 
размещения отвутствует каталог .git;
Пример использования:
;;;;(find-not-giting-lisp-projects *clisp-dir*)
"
  (let ((asd-dirs (find-filenames-directory path "\"*.asd\""))
	(git-dirs  (find-filenames-directory path "\".git\"")))
    (set-difference asd-dirs git-dirs :test #'pathname-match-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-init( &optional (os nil))
  "Генерирует bash-сценарий, инициализирующий git репозитории;
Пример использования:
;;;;(git-init)
;;;;(git-init t)
"
  (labels ((make-init-non-git-repo (path &optional (os t))
	     (cd-path path os)
	     (format os "git init~%"))
	   (func (os)
	     (mapcar #'(lambda (el) (make-init-non-git-repo el os))
		     (find-not-giting-lisp-projects *clisp-dir*))))
    (let ((f-name (concatenate 'string *clisp-dir* "git-init.sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn 
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-commit-a (&optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* 
текущей машины *m-i*, генерирует сценарий, выполняющий команду git commit -a;
В качестве комментария используется строка предтвляющая,
значение текущей даты и времени;
   Пример использования:
;;;;(git-commit-a)
;;;;(git-commit-a t)"
  (labels ((commit-a (path &optional (os t))
	   (cd-path path os)
	   (format os "git commit -a -m \"~A ~A\"~%" (decoded-time-out) *m-i*))
	 (func (os)
	   (mapcar #'(lambda (el) (commit-a el os)) (find-filenames-directory-clisp-git)))
	 )
    (let ((f-name (concatenate 'string *clisp-dir* "git-commit-a.sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-command (git-command &optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* текущей 
машины *m-i*, генерирует сценарий, выполняющий команду git-command;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
Пример использования:
;;;;(git-command  \"git remote remove other\")
;;;;(git-command  \"git remote remove other\" t)
"
  (labels (
	 (git-script(path script &optional (os t))
	   (cd-path path os)
	   (format os "~A~%" script))
	 (func (os)
	   (mapcar #'(lambda (el) (git-script el git-command os))
		   (find-filenames-directory-clisp-git))))
    (let ((f-name (concatenate 'string *clisp-dir* (string-replace-all (string-replace-all git-command " " "-") "*" "all")  ".sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t))
	    )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-clone--bare (&optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* текущей 
машины *m-i*, создает список команд, который выполняет клонирование чистого 
репозитория в каталог (concatenate 'string *git-dir* \"git-\" *m-i*); 
После создания таким образом каталога с чистыми репозиториями его можно
перенести на другую машину для выполнения слияния;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
  Пример использования:
;;;;(git-clone--bare)
;;;;(git-clone--bare t)
  Рекоемндации:
  Перед выполнением даной функции следует удалить соответствующий 
каталог:  (concatenate 'string *git-dir* \"git-\" *m-i*),
содержащий чистые репозитории
"
  (flet ((func (os) 
	   (mapcar 
	    #'(lambda (el) 
		(cd-path el os)
		(format os  "git clone --bare . ~Agit-~A/~A.git ~%" 
			*git-dir* *m-i* (file-namestring (string-right-trim "/" (format nil "~A" el)))))
	    (find-filenames-directory-clisp-git))))
    (let ((f-name (concatenate 'string *clisp-dir* "git-clone--bare.sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
		   (func os))
		 (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-remote-readd (&optional (os nil))
  "Для каждого репозитория расположенного в каталоге *clisp-dir* создает список команд, который выполняет:
- отсоединение от внешних репозиториев (список *machine-list*), которые для данной машины *m-i* вожможно
  имеют неправильное расположение;
- присоединение к внешним репозиториям (список *machine-list*), которые для данной машины *m-i* должны
  иметь правильное расположение;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
   Пример использования:
;;;;(git-remote-readd)
;;;;(git-remote-readd t)
   Рекомендации:
   Следует выполнять данную функцию ...
"
  (flet ((func (os) 
	   (mapcar 
	    #'(lambda (mi)
		(mapcar 
		 #'(lambda (el) 
		     (cd-path el os)
		     (format os  "git remote remove ~A~%" mi)
		     (format os  "git remote add ~A ~Agit-~A/~A.git~%" 
			     mi *git-dir* mi (file-namestring (string-right-trim "/" (format nil "~A" el)))))
		 (find-filenames-directory-clisp-git)))
	    *machine-list*)))
    (let ((f-name (concatenate 'string *clisp-dir* "git-remote-re-add.sh")))
      (if (null os)
	  (progn (func t) t)
	  (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	    (func os)
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))
