;;;; 这是一个自动管理 def 开头的所有定义的工具

;;(defpackage :man (:use :cl) (:export man-load man rely example test))
;;(in-package :man)

(defstruct def file code type name args docs rely example test package)
'(docs "定义 def 这种结构体来储存所有 def 类型的数据")

(defvar *all-defs* (make-hash-table :test 'equalp))
'(docs "储存所有 def 结构体的哈希表")

(defun load-by (read-fun file)
  (with-open-file (in file)
    (labels ((keep-read (out)
               (let* ((done (gensym))
                      (now (funcall read-fun in nil done)))
                 (if (eq now done)
                     (nreverse out)
                     (keep-read (push now out))))))
      (keep-read nil))))
'(example ((let ((lines (load-by #'read-line (or (probe-file "~/.bashrc")
                                                 (probe-file "~/.zshrc")))))
             (subseq lines 0 (min 10 (length lines))))
           (let ((lines (load-by #'read-char (or (probe-file "~/.bashrc")
                                                 (probe-file "~/.zshrc")))))
             (subseq lines 0 (min 10 (length lines))))))
'(docs "通过某种 read 来获取整个文件，如通过 read 获取文件的 list 状态，通过 read-line 获取文件的行列表状态等")

(defun keep-def-lists (all-read-list)
  (labels ((kdl (left now-def out)
             (if left
                 (let ((now (car left)))
                   (when (listp now)
                     (let ((head (car now)))
                       (cond
                         ;;当是 def 开头时，结束当前 now-def 并开启下一个
                         ((uiop:string-prefix-p
                           "DEF" (format nil "~A" head))
                          (kdl (cdr left)
                               (list now)
                               (push (nreverse now-def) out)))
                         ;;当是 ' 开头时，更新当前 now-def
                         ((string-equal head 'quote)
                          (kdl (cdr left)
                               (if now-def (push now now-def) now-def)
                               out))
                         ;;其余情况则跳过并下一个
                         (t
                          (kdl (cdr left) now-def out))))))
                 (nreverse (if now-def (push (nreverse now-def) out) out)))))
    (remove-if #'null (kdl all-read-list nil nil))))
'(docs "从 read 的原始数据中保留 (def...) 开头的与 '(...) 的内容")

(defun get-def-string-tail (def-string)
  (cond
    ((uiop:string-prefix-p "DEFINE-" def-string)
     (intern (subseq def-string (length "DEFINE-"))))
    ((uiop:string-prefix-p "DEFINE" def-string)
     (intern (subseq def-string (length "DEFINE"))))
    ((uiop:string-prefix-p "DEF-" def-string)
     (intern (subseq def-string (length "DEF-"))))
    ((uiop:string-prefix-p "DEF" def-string)
     (intern (subseq def-string (length "DEF"))))
    (t (intern def-string))))
'(docs "从 defun 等开头 def 上获取所 def 的数据类型")

(defun parse-def-type (def-head)
  (cond
    ((string-equal def-head 'defun)
     'function)
    (t
     (get-def-string-tail (format nil "~A" def-head)))))
'(docs "对 function 特殊对待获取 def 类型的符号")

(defun get-type-symb (type)
  (cond
    ((string-equal type 'type)    :type)
    ((string-equal type 'name)    :name)
    ((string-equal type 'args)    :args)
    ((string-equal type 'docs)    :docs)
    ((string-equal type 'example) :example)
    ((string-equal type 'test)    :test)))
'(docs "将对应的符号转换成关键字")

(defun parse-a-def-sets-list (def-sets-list)
  (let ((type (car def-sets-list))
        (body (cdr def-sets-list)))
    (let ((type-symb (get-type-symb type)))
      (when type-symb (cons type-symb body)))))

(defun parse-def-sets-lists (def-sets-lists)
  (labels ((pdsls (left out)
             (if left
                 (pdsls (cdr left)
                        (push (parse-a-def-sets-list (car left)) out))
                 (remove-if #'null (nreverse out)))))
    (pdsls def-sets-lists nil)))

(defun get-def-rely (def-code def-type &optional (package (package-name *package*)))
  (labels ((gdr (left out)
             (if left
                 (gdr (cdr left)
                      (or (let ((now (car left)))
                            (when (and now
                                       (or (listp now)
                                           (symbolp now)))
                              (if (listp now)
                                  (let ((head (car now)))
                                    (if (and (atom head)
                                             (symbolp head)
                                             (or (fboundp (find-symbol (format nil "~A" head) package))
                                                 (fboundp head)))
                                        (append (gdr (cdr now) nil)
                                                (push head out))
                                        (append (apply #'append
                                                       (mapcar #'(lambda (x)
                                                                   (gdr x nil))
                                                               (list now)))
                                                out)))
                                  (when (and (symbolp now)
                                             (or (fboundp (find-symbol (format nil "~A" now) package))
                                                 (fboundp now))
                                             (not (or (boundp (find-symbol (format nil "~A" now) package))
                                                      (boundp now))))
                                    (push now out)))))
                          out))
                 (nreverse out))))
    (gdr (if (member def-type '(function macro))
             (subseq def-code 3) def-code)
         nil)))

(defun get-the-def-set-value (type-key all-sets-list &optional default)
  (or (second (assoc type-key all-sets-list)) default))

(defun make-def-by-list (a-def-list file &optional (package (sb-int:sane-package)))
  (let* ((def-file file)
         (def-code (car a-def-list))
         (def-type (parse-def-type (car def-code)))
         (def-name (intern (format nil "~A" (nth 1 def-code)) package))
         (def-args (nth 2 def-code))
         (def-docs "Not Found")
         (def-package package)
         (def-rely (remove-duplicates (get-def-rely def-code def-type def-package)))
         (left-sets (parse-def-sets-lists (mapcar #'second (cdr a-def-list)))))
    (make-def :file def-file
              :code def-code
              :type (get-the-def-set-value :type left-sets def-type)
              :name (get-the-def-set-value :name left-sets def-name)
              :args (get-the-def-set-value :name left-sets def-args)
              :docs (get-the-def-set-value :docs left-sets def-docs)
              :rely def-rely
              :test (get-the-def-set-value :test left-sets)
              :example (get-the-def-set-value :example left-sets)
              :package (get-the-def-set-value :package left-sets def-package))))

(defun from-def-lists-to-structs (def-lists def-file &optional (package (sb-int:sane-package)))
  (mapcar #'(lambda (x) (make-def-by-list x def-file package)) def-lists))

(defun man-load (file &optional (package (sb-int:sane-package)))
  (let* ((the-file (probe-file file))
         (raw-reads (load-by #'read file))
         (def-lists (keep-def-lists raw-reads))
         (def-structs (from-def-lists-to-structs def-lists the-file (package-name package))))
    (dolist (s def-structs)
      (setf (gethash (def-name s) *all-defs*) s))
    file))
'(docs "从文件读取 defs")

(defun man (name &key (base '("NAME" "TYPE" "DOCS" "ARGS" "FILE")) add except)
  (let ((the-def (gethash name *all-defs*))
        (all-shows (copy-list (append base (if (and add (atom add)) (list add) add)))))
    (when the-def
      (dolist (i (if (and except (atom except)) (list except) except))
        (setf all-shows (remove i all-shows :test #'string-equal)))
      (dolist (type all-shows)
        (format t "<~A>: ~S~%"
                type (funcall (intern (format nil "DEF-~A" type) :hanky) the-def)))
      t)))
'(docs "查看某个 def 的详细内容")
'(example ((man 'man)
           (man 'man :add 'example)
           (man 'man :add '(code example) :except 'args)))

(defun get-rely-list (name &key show-all same-parts (test #'eql))
  (multiple-value-bind (value if-have-def) (gethash name *all-defs*)
    (declare (ignore value))
    (if if-have-def
        (labels
            ((get-rely (n &optional (already-rely (list n)))
               (let ((the-def (gethash n *all-defs*)))
                 (if the-def
                     (let* ((all-rely
                              (def-rely the-def))
                            (next-rely
                              (remove-if-not
                               #'(lambda (r)
                                   (and (or show-all
                                            (gethash r *all-defs*))
                                        (or (not same-parts)
                                            (let ((r-def (gethash (intern (format nil "~A" r)
                                                                          (package-name (symbol-package n)))
                                                                  *all-defs*)))
                                              (when r-def
                                                (not (some #'null
                                                           (mapcar #'(lambda (x)
                                                                       (funcall test
                                                                                (funcall (intern (format nil "DEF-~A" x) :hanky)
                                                                                         the-def)
                                                                                (funcall (intern (format nil "DEF-~A" x) :hanky)
                                                                                         r-def)))
                                                                   (if (atom same-parts) (list same-parts) same-parts)))))))))
                               all-rely)))
                       (when next-rely
                         (mapcar #'(lambda (r)
                                     (cond
                                       ((member r already-rely) (list r))
                                       ((gethash r *all-defs*)  (remove-if
                                                                 #'null
                                                                 (list r (get-rely r (append (list r) next-rely already-rely)))))
                                       (t r)))
                                 next-rely)))
                     name))))
          (remove-if #'null (list name (get-rely name)))))))

(defun man-rely (name &key show-all same-parts (test #'eql))
  (get-rely-list name :show-all show-all :same-parts same-parts :test test))
'(docs "展示某个 def 的所有依赖(可以递归展示到底)")

(defun man-example (name)
  (let* ((the-def (gethash name *all-defs*))
         (example (if the-def (def-example the-def))))
    (format t "<Show Example> [~A]:" name)
    (cond
      ((null the-def)
       (format t " [NOT FOUND: ~A]~%" name))
      ((null example)
       (format t " [Dose Not Have Example: ~A]~%" name))
      (t
       (format t "~%~A~%" (copy-n-string 55 #\=))
       (dolist (i example)
         (format t "$ ~S~%~35~~%" i)
         (finish-output)
         (format t "~%~15~~%~{> ~S~%~}~A~%"
                 (multiple-value-list (eval i))
                 (copy-n-string 55 #\=))
         (finish-output))
       t))))
'(docs "展示某个 def 的案例帮助用户观察和使用该 def")

(defun man-test (name)
  (let* ((the-def (gethash name *all-defs*))
         (test (def-test the-def)))
    test))
'(docs "进行 test 测试看看该 def 是否可以通过测试无误")

(defun man-search (keyword &key by test silent return show-all)
  (let ((by? (if by
                 (if (listp by) by (list by))
                 '(file code type name args docs rely example test package)))
        (out nil))
    (maphash #'(lambda (def-name def-struct)
                 (let ((def-name-string (format nil "~A" def-name)))
                   ;;(multiple-value-bind (symb state) (find-symbol def-name-string) (format t "~S: <F: ~S> <S: ~S>~%" def-name-string symb state))
                   (when (or show-all (find-symbol def-name-string))
                     (let* ((by-part (mapcar #'(lambda (b)
                                                 (list b
                                                       (funcall (intern (format nil "DEF-~A" b) :hanky)
                                                                def-struct)))
                                             by?))
                            (result (remove-if #'null (mapcar #'(lambda (a-part)
                                                                  (let ((by-name (car a-part))
                                                                        (by-body (second a-part)))
                                                                    (if (funcall (or test
                                                                                     #'(lambda (x y)
                                                                                         (smember x y t)))
                                                                                 keyword by-body)
                                                                        by-name)))
                                                              by-part))))
                       (if result (push (list def-name result) out))))))
             *all-defs*)
    (unless silent
      (if out
          (let ((max-name-len (reduce #'max
                                      (mapcar #'(lambda (x)
                                                  (length (format nil "~A" (car x))))
                                              out))))
            (format t "[FIND]  : ~A~%[RESULT]: ~{~A~^~%......... ~}~%"
                    keyword (mapcar #'(lambda (i)
                                        (format nil "~A~A :: ~A"
                                                (car i)
                                                (copy-n-string (- max-name-len (length (format nil "~A" (car i)))))
                                                (second i)))
                                    out)))
          (format t "[FIND]  : ~A~%[RESULT]: [NOT FOUND]" keyword)))
    (when return out)))
