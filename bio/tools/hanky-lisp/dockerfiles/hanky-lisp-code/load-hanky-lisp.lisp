(require :asdf)
(require :uiop)
;;(ql:quickload :cl-ppcre :silent t)    ; regex
;;(ql:quickload :clog :silent t)        ; gui

(defpackage :hanky
  (:use :cl :asdf :uiop)
  (:export
   ;;从这里导出的
   *home-dir* *posix-argv*
   ;;从 man 模块导出的
   man-load man man-search man-rely man-example man-test
   ;;从 hanky/toolbox 模块导出的
   with-all-gensyms defreload formatf
   last1 single clean-string null-string not-null-string
   ask-q mkstr symb copy-n-string smember flatten remove-if-tree
   cut-string-by cut-string split-by unite maptree ignore-redef
   load-list-by load-lines save-list-by save-lines
   parse-number right-or-wrong-char color-string connect-char
   quit-n quit-1 exit-1 while replace-string
   ;;Shell 相关函数
   run-shell run-sh run-sh-sudo run-shell-s run-sh-s
   run-python python-repl
   if-opt get-opt let-args-opts-string let-args-opts))

(in-package :hanky)

(defparameter *posix-argv*
  (append (list "sbcl") (uiop:command-line-arguments)))

(defparameter *home-dir*
  (directory-namestring (or *load-truename* "./")))
'(docs "hanky-lisp 所在的主目录")
'(example (*home-dir*))

(defparameter *loaded-files*
  (list "code/toolbox.lisp"
        "code/man.lisp"
        "code/shell.lisp"
        "code/gui.lisp"))

(declaim (ftype (function) man-load))

(defun load-hanky-files (&key (hanky-files *loaded-files*)
                           (if-man-load t)
                           (package :hanky))
  (let ((all-files
          (remove-if-not
           #'probe-file
           (mapcar #'(lambda (file)
                       (format nil "~A~A" *home-dir* file))
                   hanky-files))))
    (mapcar #'load all-files)
    (when if-man-load
      (mapcar #'(lambda (f) (hanky:man-load f package)) all-files))))

(load-hanky-files)

(hanky:defreload 'rehanky *load-truename* :package :hanky)

(in-package :cl-user)

(defun inhanky ()
  (in-package :hanky-user))

(defpackage :hanky-user
  (:use :cl :asdf :uiop :hanky))

(in-package :hanky-user)

(defun outhanky ()
  (in-package :cl-user))

(hanky:defreload 'rehanky *load-truename* :package :hanky)
