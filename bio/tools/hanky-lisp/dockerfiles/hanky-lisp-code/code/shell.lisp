;;;;定义与 linux 命令相似的命令
(declaim (ftype (function) run-sh-s))

(defun get-pid-command (pid)
  (let* ((ps-command (run-sh-s (format nil "ps -p ~A -o command" pid)))
         (command-list (cdr (remove-if #'null-string
                                       (split-by ps-command #\Newline))))
         (command (format nil "~{~A~^ \\\\ ~}" command-list)))
    command))
'(docs "从 pid 获取对应的 command")
'(example ((get-pid-command (run-sh "sleep 3"))
           (get-pid-command (run-sh "echo 1; sleep 2; echo 3"))))

(defun run-shell (command &key (stdin "")
                            (output :interactive)
                            (error-output :interactive)
                            (break-output *error-output*)
                            (format-pid nil)
                            (wait-process t))
  (let* ((in (make-string-input-stream stdin))
         (run (uiop:launch-program command
                                   :input in
                                   :output output
                                   :error-output error-output))
         (pid (uiop:process-info-pid run)))
    (when format-pid (format t "<pid>: ~A~%" pid))
    (if wait-process
        (unwind-protect
             (progn (uiop:wait-process run)
                    pid)
          (when (uiop:process-alive-p run)
            (format break-output "~%[Warning]: 程序异常...~%")
            (let* ((all-pid (run-sh-s (format nil "pgrep -P ~A" pid)))
                   (pids (mapcar #'parse-integer
                                 (remove-if #'null-string
                                            (split-by all-pid #\Newline))))
                   (pid-commands
                     (mapcar #'(lambda (pid)
                                 (list pid (get-pid-command pid)))
                             pids)))
              (uiop:terminate-process run)
              (format break-output
                      "[Warning]: 中断父进程 <pid>: ~A ===> \"~A\"~%"
                      pid command)
              (let ((ld (code-char 9492))
                    (li (code-char 9500))
                    (m- (code-char 9472)))
                (dolist (pid-cmd pid-commands)
                  (let* ((the-pid (car pid-cmd))
                         (command (second pid-cmd)))
                    (multiple-value-bind (out error)
                        (run-sh-s (format nil "kill ~A" the-pid)
                                  :format-pid nil)
                      (cond
                        ((and (null-string out) (null-string error))
                         (format break-output
                                 "[Warning]:  ~A~A 子进程 <pid>: ~A ===> \"~A\"~%"
                                 (if (= the-pid (last1 pids)) ld li) m-
                                 the-pid command))
                        ((not-null-string error)
                         (format break-output
                                 "[Warning]:  ~A~A 子进程 <pid>: ~A ===> \"~A\"; <?>: ~A~%"
                                 (if (= the-pid (last1 pids)) ld li) m-
                                 the-pid command error))
                        (t
                         (format break-output
                                 "[Warning]:  ~A~A 子进程 <pid>: ~A ===> \"~A\"; <?>: ~A~%"
                                 (if (= the-pid (last1 pids)) ld li) m-
                                 the-pid command out))))))))))
        (values pid run))))
'(docs "直接调用外部 shell 命令，并传递给 sh 运行，中断时会同步中断所有子程序，不过会把该 shell 命令放在后台运行并返回该进程的 pid")
'(keys (shell sh))
'(example ((run-shell "echo 1" :format-pid t)
           (run-shell "echo 2")
           (run-shell "echo 3")))

(defun run-sh (string &key (output :interactive)
                        (error-output :interactive)
                        (break-output *error-output*)
                        (format-pid nil)
                        (wait-process t))
  (run-shell "sh" :stdin string
                  :output output
                  :error-output error-output
                  :break-output break-output
                  :format-pid format-pid
                  :wait-process wait-process))
'(docs "直接调用外部 shell 命令，并传递给 sh 运行，中断时会同步中断所有子程序，不过会把该 shell 命令放在后台运行并返回该进程的 pid。")
'(keys (shell sh))
'(example ((run-sh "echo 1" :format-pid t)
           (run-sh "echo 2")
           (run-sh "echo 3")))

(defun run-sh-sudo (string &key (output :interactive)
                             (error-output :interactive)
                             (break-output *error-output*)
                             (format-pid nil)
                             (wait-process t))
  (run-sh (format nil "
if [ $(id -u) -eq 0 ]
then
~A
else
    echo \"[Error]: Need root/sudo !!!\" >&2
fi" string) :output output
            :error-output error-output
            :break-output break-output
            :format-pid format-pid
            :wait-process wait-process))
'(docs "只能在 sudo 环境下运行的命令")

(defun run-shell-s (command &key (stdin "")
                              (break-output *error-output*)
                              (format-pid nil))
  (let ((out-stream   (make-string-output-stream))
        (error-stream (make-string-output-stream)))
    (let ((pid (run-shell command :stdin stdin
                                  :output out-stream
                                  :error-output error-stream
                                  :break-output break-output
                                  :format-pid format-pid
                                  :wait-process t)))
      (values (clean-string (get-output-stream-string out-stream)
                            '(#\Newline #\Space #\Tab))
              (clean-string (get-output-stream-string error-stream)
                            '(#\Newline #\Space #\Tab))
              pid))))
'(docs "直接调用外部 shell 命令，类似 run-sh，但是返回运行的结果的字符串")
'(keys (shell sh run-sh))
'(example ((run-shell-s "echo 1")
           (run-shell-s "echo 2" :format-pid t)
           (run-shell-s "sleep 1" :format-pid t)))

(defun run-sh-s (string &key (break-output *error-output*) (format-pid nil))
  (run-shell-s "sh" :stdin string
                    :break-output break-output
                    :format-pid format-pid))
'(docs "直接调用外部 shell 命令，类似 run-sh，但是返回运行的结果的字符串")
'(keys (shell sh run-sh))
'(example ((run-sh-s "echo 1")
           (run-sh-s "echo 2" :format-pid t)
           (run-sh-s "sleep 1" :format-pid t)))

(defun run-python (string)
  (run-shell-s "python" :stdin string))
'(docs "运行 python 代码并返回结果的字符串")

(defun python-repl ()
  (handler-case
      (labels ((ipy (py-string head return-n)
                 (formatf t "~A " head)
                 (let ((input-line (read-line)))
                   (cond
                     ((null-string (clean-string input-line))
                      (multiple-value-bind (out error) (run-python py-string)
                        (formatf t "~%[~A]: ~A~%~A~%" return-n out
                                 (if (null-string error)
                                     ""
                                     (format nil "[Error]: ~A~%" error))))
                      (ipy "" ">>>" (1+ return-n)))
                     ((member (clean-string input-line) '("q" "Q" "quit" "exit" "quit()" "exit()")
                              :test #'string-equal)
                      (return-from python-repl))
                     (t
                      (ipy (format nil "~A~%~A" py-string input-line)
                           "..." return-n))))))
        (ipy "" ">>>" 1))
    (condition (e) (formatf t "[Condition]: ~A~%" e))))
'(docs "开启类似 ipython 的 python 的 repl，使用 exit 或 quit 来退出")



;;接下来是接受命令行参数相关的功能
(defun if-opt (opt-name opt-test-list &key (do-body nil))
  `(,opt-name ,opt-test-list nil #'(lambda (&rest all) (declare (ignore all)) t) #'cdr ,do-body))
'(docs "将 opt 转换为仅判断是否存在的 opts 标准格式，往往配合 let-opts-args-exe 使用，此时可以当作 macro")
'(example ((if-opt 'if-help (list "-h" "--help"))
           (if-opt 'if-version (list "-v" "--version"))))

(defun get-opt (opt-name opt-test-list
                &key (default nil) (fun #'identity)
                  (do-body nil))
  (with-all-gensyms (x)
    `(,opt-name ,opt-test-list ,default
                #'(lambda (,x) (funcall ,fun (second ,x)))
                #'cddr
                ,do-body)))
'(docs "将 opt 转换为仅判断是否存在的 opts 标准格式")
'(example ((get-opt 'resolution (list "-r" "--resolution")
            :default 5000 :fun #'parse-integer)))

(defmacro let-args-opts-string (args-list args-name
                                (opts-name opts-bindings)
                                &body body)
  (let ((new-opts-bindings
          (mapcar #'(lambda (x) (apply (car x) (cdr x)))
                  opts-bindings)))
    (let ((all-opts-names (mapcar #'car new-opts-bindings))
          (all-opts-defaults (mapcar #'third new-opts-bindings)))
      (with-all-gensyms (clean-args left-args new-args)
        (let ((label-args
                `(,left-args ,@all-opts-names ,new-args))
              (cond-form
                `(cond
                   ,@(mapcar
                      #'(lambda (opt-list)
                          `((member (car ,left-args) ,(second opt-list)
                                    :test #'equal)
                            ,(sixth opt-list)
                            (,clean-args
                             (funcall ,(fifth opt-list) ,left-args)
                             ,@(mapcar #'(lambda (a-opt)
                                           (if (eql (car a-opt)
                                                    (car opt-list))
                                               `(funcall ,(fourth opt-list)
                                                         ,left-args)
                                               (car a-opt)))
                                       new-opts-bindings)
                             ,new-args)))
                      new-opts-bindings)
                   (t
                    (,clean-args (cdr ,left-args)
                                 ,@all-opts-names
                                 (push (car ,left-args) ,new-args))))))
          `(labels ((,clean-args ,label-args
                      (if ,left-args
                          ,cond-form
                          (values (nreverse ,new-args)
                                  ,@all-opts-names))))
             (multiple-value-bind (,args-name ,@all-opts-names)
                 (,clean-args (if (eql ,args-list :EOF) *posix-argv* ,args-list)
                              ,@all-opts-defaults
                              nil)
               (let ((,opts-name
                       (list ,@(mapcar #'(lambda (opt)
                                           `(list ',opt ,opt))
                                       all-opts-names))))
                 (progn ,args-name ,opts-name ,@all-opts-names)
                 ,@body))))))))
'(docs "对于 string 的列表作为参数，分离对应的 -xxx 与普通参数")
'(example ((let-args-opts-string '("sbcl" "-h" "-p" "-r" "~/.bashrc" "axx" "bxx")
               args
               (opts ((if-opt  if-help    '("-h"))
                      (if-opt  if-version '("-v"))
                      (if-opt  if-print   '("-p" "--print"))
                      (get-opt if-random  '("-r" "--random")
                               :fun #'probe-file)))
             (format t "~%~77~~%")
             (format t "<ARGS>      : ~A~%" args)
             (format t "<OPTS>      : ~A~%" opts)
             (format t "<if-help>   : ~A~%" if-help)
             (format t "<if-version>: ~A~%" if-version)
             (format t "<if-print>  : ~A~%" if-print)
             (format t "<if-random> : ~A~%" if-random)
             (format t "~%"))
           (let-args-opts-string '("sbcl" "-h" "-r" "~/no-this-file" "axx" "-h")
               args
               (opts ((if-opt  if-help    '("-h")
                               :do-body (format t "<<<HELP>>>~%"))
                      (if-opt  if-version '("-v"))
                      (if-opt  if-print   '("-p" "--print"))
                      (get-opt if-random  '("-r" "--random")
                               :fun #'probe-file
                               :do-body (progn
                                          (format t "!!!RANDOM!!!~%")
                                          (format t "!!!STATES!!!~%")))))
             (format t "~%~77~~%")
             (format t "<ARGS>      : ~A~%" args)
             (format t "<OPTS>      : ~A~%" opts)
             (format t "<if-help>   : ~A~%" if-help)
             (format t "<if-version>: ~A~%" if-version)
             (format t "<if-print>  : ~A~%" if-print)
             (format t "<if-random> : ~A~%" if-random)
             (format t "~%"))))


(defmacro let-args-opts ((args-name &optional (args-list :EOF))
                         (opts-name opts-bindings) &body body)
  `(let-args-opts-string ,args-list ,args-name
       (,opts-name ,opts-bindings) ,@body))
'(docs "")
'(example ((let-args-opts (args) (opts ((if-opt  if-help    '("-h"))
                                        (if-opt  if-version '("-v"))
                                        (if-opt  if-print   '("-p" "--print"))
                                        (get-opt if-random  '("-r" "--random")
                                                 :fun #'probe-file)))
             (format t "~%~77~~%")
             (format t "<ARGS>      : ~A~%" args)
             (format t "<OPTS>      : ~A~%" opts)
             (format t "<if-help>   : ~A~%" if-help)
             (format t "<if-version>: ~A~%" if-version)
             (format t "<if-print>  : ~A~%" if-print)
             (format t "<if-random> : ~A~%" if-random)
             (format t "~%"))
           (let ((all-args '("-h" "-r" "192" "-p" "arg1" "agr2")))
             (let-args-opts (args all-args)
                 (opts ((if-opt  if-help    '("-h"))
                        (if-opt  if-version '("-v"))
                        (if-opt  if-print   '("-p" "--print"))
                        (get-opt the-random '("-r" "--random"))))
               (format t "~%~77~~%")
               (format t "<ARGS>      : ~A~%" args)
               (format t "<OPTS>      : ~A~%" opts)
               (format t "<if-help>   : ~A~%" if-help)
               (format t "<if-version>: ~A~%" if-version)
               (format t "<if-print>  : ~A~%" if-print)
               (format t "<the-random>: ~A~%" the-random)
               (format t "~%")))))
