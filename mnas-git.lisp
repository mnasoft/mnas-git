;;;; mnas-git.lisp

(in-package :mnas-git)

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
начиная с каталога path вглубь дерева каталогов

Возвращает список полных путей к найденным файлам;

Пример использования:
@begin[lang=lisp](code)
  (find-filenames \"/_storage/otd11/namatv/git/clisp/\" \"*.asd\")
@end(code)
"
  (let ((osstr (make-string-output-stream)))
    (uiop:run-program (concatenate 'string "find " path " -name " name) :output osstr :ignore-error-status t)
    (do* ((isstr (make-string-input-stream (get-output-stream-string osstr)))
	  (rez nil)
	  (line (read-line isstr nil 'eof) (read-line isstr nil 'eof)))
	 ((eql line 'eof) (reverse rez))
      (setf rez (cons line rez)))))

(defun find-filenames-directory (path name)
  "Возвращает список директорий:
@begin(list)
 @item(элементы которого не повторяются;)
 @item(в которых удалось найти файлы удовлетворяющие шаблону name;)
 @item(поиск файлов начинается с каталога path.)
@end(list)

Пример использования:
@begin[lang=lisp](code)
 (find-filenames-directory *clisp-dir-win* \"*.lisp\")
@end(code)
"
  (delete-duplicates
   (mapcar
    #'(lambda (el)
	(make-pathname
	 :directory
	 (pathname-directory
	  (uiop:directorize-pathname-host-device el))))
    (find-filenames path name)) :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-filenames-directory-clisp-git()
;;;;  (mnas-path:find-directory-parent *git-bare-dir-win* ".git")
  (mnas-path:find-directory-parent *clisp-dir-win* ".git"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-not-giting-lisp-projects (path)
  "Осуществляет рекурсивный поиск лисп-проектов *.asd 
вглубь дерева каталогов, для которых в каталоге их 
размещения отвутствует каталог .git;
Пример использования:
;;;;(find-not-giting-lisp-projects *clisp-dir-win*)
"
  (let ((asd-dirs (mnas-path:find-filename-directory path "asd"))
	(git-dirs  (mnas-path:find-directory-parent path ".git")))
    (set-difference asd-dirs git-dirs :test #'pathname-match-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init( &optional (os nil))
"@b(Описание:) init генерирует bash-сценарий, инициализирующий git репозитории.

 @b(Пример использования:)
 @begin[lang=lisp](code)
  (init) 
  (init t)
 @end(code)
"
  (labels ((make-init-non-git-repo (path &optional (os t))
	     (cd-path path os)
	     (format os "git init~%"))
	   (func (os)
	     (mapcar #'(lambda (el) (make-init-non-git-repo el os))
;;;;		     (find-not-giting-lisp-projects *git-bare-dir-win*)
		     (find-not-giting-lisp-projects *clisp-dir-win*))))
    (let (
;;;;	  (f-name (concatenate 'string *git-bare-dir-win* "init.sh"))
	  (f-name (concatenate 'string *clisp-dir-win* "init.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (progn 
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun commit-a (&optional (os nil))
"@b(Описание:) commit-a для каждого репозитория, расположенного в каталоге 
*clisp-dir-win* текущей машины *m-i*, генерирует сценарий, выполняющий 
команду git commit -a.

 В качестве комментария используется строка предтвляющая,
значение текущей даты и времени.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (commit-a)
 (commit-a t)
@end(code)
"
  (format t "mnas-git:commit-a ...Start...~%" ) 
  (labels ((commit-a (path &optional (os t))
	     (cd-path path os)
	     (format os "git commit -a -m \"~A ~A\"~%" (decoded-time-out) *m-i*))
	   (func (os)
	     (mapcar #'(lambda (el) (commit-a el os)) (find-filenames-directory-clisp-git)))
	   )
    (let (
;;;;	  (f-name (concatenate 'string *git-bare-dir-win* "commit-a.sh")))
	  (f-name (concatenate 'string *clisp-dir-win* "commit-a.sh")))
      (if (null os)
	  (progn (func t) t)
	  (progn
	    (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	      (func os))
	    (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command (command &optional (os nil))
"@b(Описание:) command для каждого репозитория, расположенного в каталоге
*clisp-dir-win* текущей машины *m-i*, генерирует сценарий, выполняющий команду
git-command.

 Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (command  \"git remote remove other\")
 (command  \"git remote remove other\" t)
@end(code)
"
  (format t "mnas-git:command ~A ...Start...~%" command) 
  (let ((git-command (concatenate 'string "git" " " command)))
    (labels (
	     (git-script(path script &optional (os t))
	       (cd-path path os)
	       (format os "~A~%" script))
	     (func (os)
	       (mapcar #'(lambda (el) (git-script el git-command os))
		       (find-filenames-directory-clisp-git))))
      (let (
;;;;	    (f-name (concatenate 'string *git-bare-dir-win* (mnas-string:replace-all (string-replace-all git-command " " "-") "*" "all")  ".sh"))
	    (f-name (concatenate 'string *clisp-dir-win* (mnas-string:replace-all (string-replace-all git-command " " "-") "*" "all")  ".sh"))
	    )
	(if (null os)
	    (progn (func t) t)
	    (progn
	      (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
		(func os))
	      (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sh-command (cmd-string &key (output t) (ignore-error-status t))
"@b(Описание:) sh-command позволяет выполнить команды."
  (uiop:run-program
   (concatenate 'string "sh -c \"" cmd-string  "\"")
   :output output
   :ignore-error-status ignore-error-status))

(sh-command "cd ; cd org; git pull MNASOFT-01 master;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun clone--bare (&optional (os nil))
"@b(Описание:) clone--bare для каждого репозитория, расположенного в каталоге 
*clisp-dir-win* текущей машины *m-i*, создает список команд, который выполняет
клонирование чистого репозитория в каталог (concatenate 'string *git-bare-dir*
 \"git-\" *m-i*); 

 После создания таким образом каталога с чистыми репозиториями его можно
перенести на другую машину для выполнения слияния.

 Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t.

 Иначе -- вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла.

@b(Пример использования:)
@begin[lang=lisp](code)
 (progn 
   (clone--bare)
   (clone--bare t))
@end(code)

 Рекоемндации:
@begin(list)
 @item(перед выполнением даной функции следует удалить каталог содержащий чистые репозитории.)
@end(list)
 Например:
@begin[lang=lisp](code)
 (concatenate 'string *git-bare-dir* \"git-\" *m-i*)
@end(code)
"
  (format t "mnas-git:clone--bare ...Start...~%") 
  (flet ((func (os) 
	   (mapcar 
	    #'(lambda (el) 
		(cd-path el os)
		(format os  "git clone --bare . ~Agit-~A/~A.git ~%" 
			*git-bare-dir* *m-i* (file-namestring (string-right-trim "/" (format nil "~A" el)))))
	    (find-filenames-directory-clisp-git))))
    (let (
;;;;	  (f-name (concatenate 'string *git-bare-dir-win* "clone--bare.sh"))
	  (f-name (concatenate 'string *clisp-dir-win* "clone--bare.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (progn (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
		   (func os))
		 (values f-name (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readd-single-path-mashine (path mi &optional (os nil))
  (cd-path path os)
  (format os  "git remote remove ~A~%" mi)
  (format os  "git remote add ~A ~Agit-~A/~A.git~%" 
	  mi *git-bare-dir* mi (file-namestring (string-right-trim "/" (format nil "~A" path)))))

(defun readd-single-path-mashines (path &key (mashines *m-l*) (os t))
  (mapcar
   #'(lambda (mi)
       (readd-single-path-mashine path mi os))
   mashines))


(defun dialog-remote-readd (&optional (initialdir "~/quicklisp/local-projects"))
"@b(Описание:) dialog-remote-readd выполняет диалог для указания каталога, 
в котором находится репозиторий git, для пересоздания удаленных репозиториев. 

 @b(Пример использования:)
@begin[lang=lisp](code)
  (dialog-remote-readd)
@end(code)
"
  (let ((dir (mnas-file-dialog:choose-directory :initialdir initialdir)))
    (when (string/= dir "")
      (readd-single-path-mashines dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remote-readd (&optional (os nil))
"@b(Описание:) remote-readd для каждого репозитория расположенного в каталоге 
*clisp-dir-win* создает список команд, который выполняет:
@begin(list)
@item(отсоединение от внешних репозиториев (список *m-l*), которые для данной машины *m-i* вожможно имеют неправильное расположение;)
@item(присоединение к внешним репозиториям (список *m-l*), которые для данной машины *m-i* должны иметь правильное расположение;)
@end(list)
 Если опциональный параметр os имеет значение nil,
вывод функции направляется на стандартный вывод при этом функция возврвщает t,
иначе - вывод направляется в командный файл и затем этот выполняется при этом 
функция возврвшает путь к командному файлу и результат выполнения командного файла.

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (prong
  (remote-readd)
  (remote-readd t))
 @end(code)
 
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
;;;;	  (f-name (concatenate 'string *git-bare-dir-win* "git-remote-re-add.sh"))
	  (f-name (concatenate 'string *clisp-dir-win* "git-remote-re-add.sh"))
	  )
      (if (null os)
	  (progn (func t) t)
	  (with-open-file (os f-name :direction :output :if-does-not-exist :create :if-exists :supersede)
	    (func os)
	    (values f-name
		    (uiop:run-program (concatenate 'string "sh" " " f-name) :ignore-error-status t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clone--origin (origin)
"@b(Описание:) clone--origin генерирует сценарий, который выполяет клонирование 
чистых из репозиториев, для которых в каталоге с проектами не нашлось 
соответствующего проекта, в калалоге для удаленной машины origin.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (clone--origin \"mnasoft-00\")
@end(code)
"
  (let (
	(b-r (cl-fad:list-directory (concatenate 'string *git-bare-dir-win* "git-" origin )))
;;;;	(b-r (cl-fad:list-directory (concatenate 'string *git-bare-dir* "git-" origin )))
	(g-r  (find-filenames-directory-clisp-git))
	(tmp-dir "_temp")
	)
    (cd-path *clisp-dir-win* t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-list (lst &key (clear-line 2))
  (let ((i 0))
    (format t (format nil "~~~A%" clear-line))
    (mapcar
     #'(lambda (el)
	 (format t "~A: ~A~%" i el)
	 (incf i))
     lst)))

(defun command-machine ()
  (do ((inp nil (read))
       (len (length *m-l*) (length *m-l*)))
      ((and (integerp inp) (<= 0 inp) (< inp len)) (nth inp *m-l*))
    (print-list *m-l*)
    (format t "~&Введите число, соответствующее удалённой (remote) машине:")))


(defun man ()
"Формирует строку, содержащую подсказку."
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
"))


(defun help (&optional (remote-machine-name (command-machine)))
"@b(Описание:) help формирует подсказку с вариантами использования"
  (format t "Имя этой машины                         : ~A~%" *m-i*)
  (format t "Имя удалённой (remote) машины           : ~A~%" remote-machine-name)
  (format t "Список удаленных чистых git-репозиториев: ~A~%" *m-l*)
  (format t "Расположение каталога с:~%")
  (format t "- удаленными чистыми git-репозиториями  : ~A~%" *git-bare-dir-win*)
  (format t "- asd-проектами Common Lisp             : ~A~%" *clisp-dir-win*)
;;;;  (format t "- asd проектов для команд sh : ~A~%" *git-bare-dir-win*)
  (write-line "
Примеры использования функций:
==============================
 1. Инициализация каталогов git
 (mnas-git:init)

 2. Клонирование каталогов из чистого репозитория")
  (format t "~& (mnas-git:clone--origin ~S)" remote-machine-name)

  (format t "~&~% 3. Переопределение расположения чистых репозирориев")
  (format t "~& (mnas-git:remote-readd t)")

  (format t "~&~% 4. Получение изменений из чистых репозирориев")
  (format t "~&(progn (mnas-git:command \"pull ~A master\" t)" remote-machine-name)
  (format t "~&  (mnas-git:sh-command \"cd ; cd org; git pull ~A master;\")" remote-machine-name)
  (format t "~&  (mnas-git:sh-command \"cd ; cd elisp; git pull ~A master;\"))" remote-machine-name)
  
  (format t "~&~% 5. Добавление, коммит и отправка изменений в чистый репозиторий")
  (format t "~&(progn (mnas-git:command \"add *.lisp *.org *.asd\" t) (mnas-git:commit-a t) (mnas-git:command \"push ~A master\" t))" *m-i*)
  
  (format t "~&  cd; cd   org; find . -name \"*.org\" | xargs git add; git commit -a -m \"`date`\"; git push ~A master;" *m-i*)
  (format t "~&  cd; cd   org; find . -name \"*.trd\" | xargs git add; git commit -a -m \"`date`\"; git push ~A master;" *m-i*)

  (format t "~&  cd; cd elisp; find . -name \"*.org\" | xargs git add; git commit -a -m \"`date`\"; git push ~A master;" *m-i*)
  (format t "~&  cd; cd elisp; find . -name \"*.el\"  | xargs git add; git commit -a -m \"`date`\"; git push ~A master;" *m-i*)

  (format t "~&~% 6. Архивирование чистого репозитория:")

  (format t "~&(mnas-git:sh-command \"rm -rf ~Agit-~A.tar.xz; cd ~A; tar -cJf git-~A.tar.xz git-~A/; echo DONE;\")"
	  *git-bare-dir-win*  *m-i* *git-bare-dir-win* *m-i* *m-i*)

  (format t "~&~% 7. Колнирование в чистый репозиторий")
  (format t "~&# (mnas-git:clone--bare t)")
  
  (format t "~&~% 8. Удаление чистого репозитория:")
  (format t "~&# rm -rf ~Agit-~A/~%" *git-bare-dir-win* *m-i*)
  
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "(mnas-git:help)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun invoke-git (repo-name start-dir command)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (invoke-git \"mnas-spring\"
             \"/home/namatv/quicklisp/local-projects/\" command
             (list \"tree\" \"readd\" \"gh\"))
@end(code)
"
  (let* ((s-stream (make-string-output-stream))
         (directory 
           (progn
             (sb-ext:run-program "/usr/bin/find"
                                 (list
                                  "."
                                  "-name"
                                  repo-name)
                                 :output s-stream
                                 :directory start-dir
                                 )
             (read-line
              (make-string-input-stream
               (get-output-stream-string s-stream))
              nil ""))))
    (when (< 0 (length directory))
    (sb-ext:run-program "/usr/bin/git" command
                        :directory (concatenate 'string start-dir directory)
                        :output s-stream)
    (format t "~A" (get-output-stream-string s-stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mnasoft-git*
  '("mnas-git"
    "math"
    "mnas-string"
    "mnas-path"
    "enter-box"
    "gases"
    "mnas-package"
    "mnas-ansys"
    "gnuplot"
    "git-tree"
    "mnas-clock"
    "qt-conv"
    "mnas-vse-gost"
    "ecology"
    "mnas-site"
    "mnas-dns"
    "mnas-spring"
    "mnas-logical"
    "mnas-heat-transfer"
    "mnas-hash-table"
    "mnas-format"
    "mnas-dim-value"
    "mnas-defclass"
    "vse-gost"
    "varghaftik"
    "trender"
    "time"
    "slad"
    "recoder"
    "pcl-test"
    "lst-arr"
    "idelchik"
    "html-table"
    "half-div"
    "fors-lic"
    "dims"
    "convertion"
    "areas"
    "algorithm"
    "vlisp"
    "dwg-geom"
    "acad-formats"
    "dxf"
    "mnas-graph"
    "docparser"
    "codex"
    "kons-9"
    "MNAS_acad_utils"
    "elisp"
    "rclone"
    "slynk-start-server"
    "EnterBoxPlugin"
    "emacs_slime_asdf_setup_instr"
    "icacls"
    "exel-read"
    "cl-cffi-gtk"
    "ost"
    "cl-annot"
    "mnas-list"
    "FlowSoploCulc"
    "EnterBoxPlugin_test"
    "mnasoft_lib"
    "Coating"
    "AddressBook"
    "Engine"
    "cl-irregsexp"
    "GasCulc"
    "AutoShape"
    "SBo_Port_script"))

#+nil
(loop :for i :in *mnasoft-git* :do
  (invoke-git i "/home/namatv/quicklisp/local-projects/" '("tree" "all")))
