;;;; mnas-git.lisp

(in-package #:mnas-git)

(defun cd-path (path &optional (os t))
  (if (eq os t)
      (format os "~%cd ~A~%" path)
      (format os "cd ~A~%echo~%pwd~%" path)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-filenames-directory-clisp-git()
;;;;  (mnas-path:find-directory-parent *sh-dir* ".git")
  (mnas-path:find-directory-parent *clisp-dir* ".git"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-not-giting-lisp-projects (path)
  "Осуществляет рекурсивный поиск лисп-проектов *.asd 
вглубь дерева каталогов, для которых в каталоге их 
размещения отвутствует каталог .git;
Пример использования:
;;;;(find-not-giting-lisp-projects *clisp-dir*)
"
  (let ((asd-dirs (mnas-path:find-filename-directory path "asd"))
	(git-dirs  (mnas-path:find-directory-parent path ".git")))
    (set-difference asd-dirs git-dirs :test #'pathname-match-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init( &optional (os nil))
  "Генерирует bash-сценарий, инициализирующий git репозитории;
Пример использования:
;;;;(init)
;;;;(init t)
"
  (labels ((make-init-non-git-repo (path &optional (os t))
	     (cd-path path os)
	     (format os "git init~%"))
	   (func (os)
	     (mapcar #'(lambda (el) (make-init-non-git-repo el os))
;;;;		     (find-not-giting-lisp-projects *sh-dir*)
		     (find-not-giting-lisp-projects *clisp-dir*))))
    (let (
;;;;	  (f-name (concatenate 'string *sh-dir* "init.sh"))
	  (f-name (concatenate 'string *clisp-dir* "init.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (progn 
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun commit-a (&optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* 
текущей машины *m-i*, генерирует сценарий, выполняющий команду git commit -a;
В качестве комментария используется строка предтвляющая,
значение текущей даты и времени;
   Пример использования:
;;;;(commit-a)
;;;;(commit-a t)"
  (labels ((commit-a (path &optional (os t))
	   (cd-path path os)
	   (format os "git commit -a -m \"~A ~A\"~%" (decoded-time-out) *m-i*))
	 (func (os)
	   (mapcar #'(lambda (el) (commit-a el os)) (find-filenames-directory-clisp-git)))
	 )
    (let (
;;;;	  (f-name (concatenate 'string *sh-dir* "commit-a.sh")))
	  (f-name (concatenate 'string *clisp-dir* "commit-a.sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command (command &optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* текущей 
машины *m-i*, генерирует сценарий, выполняющий команду git-command;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
Пример использования:
;;;;(command  \"git remote remove other\")
;;;;(command  \"git remote remove other\" t)
"
  (let ((git-command (concatenate 'string "git" " " command)))
    (labels (
	     (git-script(path script &optional (os t))
	       (cd-path path os)
	       (format os "~A~%" script))
	     (func (os)
	       (mapcar #'(lambda (el) (git-script el git-command os))
		       (find-filenames-directory-clisp-git))))
      (let (
;;;;	    (f-name (concatenate 'string *sh-dir* (string-replace-all (string-replace-all git-command " " "-") "*" "all")  ".sh"))
	    (f-name (concatenate 'string *clisp-dir* (string-replace-all (string-replace-all git-command " " "-") "*" "all")  ".sh"))
	    )
	(if (null os)
	    (progn (func t) t)
	    (progn
	      (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
		(func os))
	      (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clone--bare (&optional (os nil))
  "Для каждого репозитория, расположенного в каталоге *clisp-dir* текущей 
машины *m-i*, создает список команд, который выполняет клонирование чистого 
репозитория в каталог (concatenate 'string *git-bare-dir* \"git-\" *m-i*); 
После создания таким образом каталога с чистыми репозиториями его можно
перенести на другую машину для выполнения слияния;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
  Пример использования:
;;;;(clone--bare)
;;;;(clone--bare t)
  Рекоемндации:
  Перед выполнением даной функции следует удалить соответствующий 
каталог:  (concatenate 'string *git-bare-dir* \"git-\" *m-i*),
содержащий чистые репозитории
"
  (flet ((func (os) 
	   (mapcar 
	    #'(lambda (el) 
		(cd-path el os)
		(format os  "git clone --bare . ~Agit-~A/~A.git ~%" 
			*git-bare-dir* *m-i* (file-namestring (string-right-trim "/" (format nil "~A" el)))))
	    (find-filenames-directory-clisp-git))))
    (let (
;;;;	  (f-name (concatenate 'string *sh-dir* "clone--bare.sh"))
	  (f-name (concatenate 'string *clisp-dir* "clone--bare.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (progn (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
		   (func os))
		 (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remote-readd (&optional (os nil))
  "Для каждого репозитория расположенного в каталоге *clisp-dir* создает список команд, который выполняет:
- отсоединение от внешних репозиториев (список *m-l*), которые для данной машины *m-i* вожможно
  имеют неправильное расположение;
- присоединение к внешним репозиториям (список *m-l*), которые для данной машины *m-i* должны
  иметь правильное расположение;
   Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла;
   Пример использования:
;;;;(remote-readd)
;;;;(remote-readd t)
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
			     mi *git-bare-dir* mi (file-namestring (string-right-trim "/" (format nil "~A" el)))))
		 (find-filenames-directory-clisp-git)))
	    *m-l*)))
    (let (
;;;;	  (f-name (concatenate 'string *sh-dir* "git-remote-re-add.sh"))
	  (f-name (concatenate 'string *clisp-dir* "git-remote-re-add.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	    (func os)
	    (values f-name
		    (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clone--origin (origin)
  "Генерирует сценарий, который выполяет клонирование чистых из репозиториев, для которых 
в каталоге с проектами не нашлось соответствующего проекта,
во виз расположения origin 
;;;;(clone--origin \"mnasoft-00\")
"
  (let (
	(b-r (cl-fad:list-directory (concatenate 'string *sh-dir* "git-" origin )))
;;;;	(b-r (cl-fad:list-directory (concatenate 'string *git-bare-dir* "git-" origin )))
	(g-r  (find-filenames-directory-clisp-git))
	(tmp-dir "_temp")
	)
    (cd-path *clisp-dir* t)
    (format t "mkdir ~A~%" tmp-dir)
    (format t "cd ~A~%" tmp-dir)
    (mapcar
     #'(lambda (el)
	 (format t "git clone --origin ~A ~Agit-~A/~A.git~%" origin *git-bare-dir* origin (pathname-name (cl-fad:pathname-as-file el)))
	 )
     (set-difference
      b-r g-r :test #'(lambda (br gr)
			(string=
			 (pathname-name (cl-fad:pathname-as-file br))
			 (pathname-name (cl-fad:pathname-as-file gr))))))))


(defun help ()
  (write-line
   "  MNAS-GIT - проект предназначен для несетевого способа синхроизации репозиториев git;
  Функции, выполняющие формирование сценариев коммандной оболочки для управления репозиториями git 
перечислены в таблице 1;

Таблица 1 - Перечень функций
|---------------+--------------+------------------------------------------------------------------|
| Функция       | Параметры    | Описание, генерируемого сценария                                 |
|---------------+--------------+------------------------------------------------------------------|
| init          | &optional os | Выполняет инициализацию git-репозиториев, содержащих             |
|               |              | asd-проекты Common Lisp, в которых перед выполнением             |
|               |              | данной команды git-реозитории отсутствовали;                     |
|---------------+--------------+------------------------------------------------------------------|
| commit-a      | &optional os | Выполняет коммит изменений git-репозиториев, содержащих          |
|               |              | asd-проекты Common Lisp, при этом информационое сообщение        |
|               |              | содержит дату, время и хост, на котором коммит выполнялся        |
|---------------+--------------+------------------------------------------------------------------|
| clone--bare   | &optional os | Создает чистый клон для каждого git-репозитория, содержащего     |
|               |              | asd-проекты Common LISP                                          |
|---------------+--------------+------------------------------------------------------------------|
| clone--origin | str-origin   | Выполняет клонирование git-репозиториев из удаленного чистого    |
|               |              | репозитория, задаваемого параметром str-origin, в каталог _temp, |
|               |              | где находятся asd-проекты Common Lisp, при чем клонируются       |
|               |              | только недостающие git-репозитории                               |
|---------------+--------------+------------------------------------------------------------------|
| remote-readd  | &optional os | Выполняет удаление и последующее добавление удаленных чистых     |
|               |              | git-репозиториев для git-репозиториев, содержащих проекты        |
|               |              | Common Lisp;                                                     |
|---------------+--------------+------------------------------------------------------------------|
| command       | str-command  | Выполняет git-команду, задаваемую параметром str-command;        |
|               | &optoinal os |                                                                  |
|---------------+--------------+------------------------------------------------------------------|
| help          |              | Вывод настоящей справки                                          |
|---------------+--------------+------------------------------------------------------------------|
| Примечание    | &optional os | Опциональный параметр, по-умолчанию принимающий значение nil     |
|               |              | Если параметр имеет значение:                                    |
|               |              | - nil - сценарий коммандной оболочки выводится на                |
|               |              | стандартный вывод но не выполняется;                             |
|               |              | - t   - сценарий коммандной оболочки выводится в файл и          |
|               |              | затем выполняется, а функция возвращает имя сгенерированного     |
|               |              | сценария                                                         |
|---------------+--------------+------------------------------------------------------------------|
")
  (format t "Имя этой машины                         : ~A~%" *m-i*)
  (format t "Список удаленных чистых git-репозиториев: ~A~%" *m-l*)
  (format t "Расположение каталога с:~%")
  (format t "- удаленными чистыми git-репозиториями  : ~A~%" *git-bare-dir*)
  (format t "- asd-проектами Common Lisp             : ~A~%" *clisp-dir*)
;;;;  (format t "- asd проектов для команд sh : ~A~%" *sh-dir*)
  (write-line "
Примеры использования функций:
==============================
;;;;(mnas-git:init)
;;;;
;;;;(mnas-git:remote-readd)
;;;;(mnas-git:clone--origin \"MNASOFT-01\")
;;;;
;;;;(progn (mnas-git:command \"add *.lisp *.asd\")   (mnas-git:commit-a)   (mnas-git:clone--bare))
;;;;(progn (mnas-git:command \"add *.lisp *.asd\" t) (mnas-git:commit-a t) (mnas-git:clone--bare t))
;;;;
;;;;(mnas-git:command \"pull MNASOFT-01 master\")
;;;;(mnas-git:command \"push MNASOFT-01 master\")
;;;;(mnas-git:help)")
  (write-line "
Пример использования команд сжатия:
==================================")
  (format t "cd ~A~%" *git-bare-dir*)
  (format t "tar -cvJf git-~A.tar.xz git-~A/~%" *m-i* *m-i*)
  (format t "rm -rf ~Agit-~A/~%" *git-bare-dir* *m-i*)
  (values))

(help)
