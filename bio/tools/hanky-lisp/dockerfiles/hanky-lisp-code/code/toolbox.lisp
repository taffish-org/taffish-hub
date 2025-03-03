;;这里是很多实用函数

(defmacro with-all-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))
'(docs "十分重要的宏工具, 防止出现变量捕获, 接受需要 gensym 的符号与对应 body")
'(example ((with-all-gensyms (x y) (print x) (print y))))

(defun defreload (reload-name file
                  &key (if-man-load t) (package (sb-int:sane-package)))
  (setf (symbol-function reload-name)
        (lambda ()
          (load file)
          (if if-man-load (hanky:man-load file package))
          file))
  file)
'(docs "制作 reload 函数，使用 reload-name 这一符号可以重新 load file 文件")

(defun formatf (destination control-string &rest format-arguments)
  (apply #'format destination control-string format-arguments)
  (finish-output))
'(docs "在 format 后 finish-output 从而保证 format 间不会串")

;;;短小需要内联编译的函数
(proclaim '(inline last1 single clean-string null-string not-null-string))

(defun last1 (lst)
  (car (last lst)))
'(docs "不同于 last, 取最后一个元素的 car, 而不是 cdr")
'(example ((last1 '(1 2 3 4 5 4 3))))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))
'(docs "判断是否是一个单元素列表")
'(example ((single '(1)) (single '(1 2))))

(defun clean-string (string &optional (char-bag '(#\Space #\Tab #\Newline)))
  (string-trim char-bag string))
'(docs "清除一个字符串两侧的一些重复字符")
'(example ((clean-string "  123  ")
           (clean-string "\t  abc \t  \t ")
           (clean-string "\txyt ")))

(defun null-string (string)
  (string= "" string))
'(docs "判断一个字符串是不是空字符串")
'(example ((null-string "a")
           (null-string " ")
           (null-string "")))

(defun not-null-string (string)
  (not (null-string string)))
'(docs "判断一个字符串是不是空字符串")
'(example ((not-null-string "a")
           (not-null-string " ")
           (not-null-string "")))

;;;其余函数
(defun ask-q (question &optional default)
  (format t "~A (y/n [~A])~%>>> " question (if default "y" "n"))
  (finish-output)
  (let ((answer (read-line)))
    (cond ((string-equal "y" answer) t)
          ((string-equal "n" answer) nil)
          (t                         default))))
'(docs "询问一个问题并根据用户输入的 y 或别的返回 T 或 NIL")
'(example ((let ((if-bigger (ask-q "if 3>2 ? [may print your bigger one...]")))
             (format t "~%You Think The Bigger One is: ~A~%[~A]~%"
                     (if if-bigger 3 2) (if if-bigger "YES!" "WRONG!!!")))
           (let ((if-bigger (ask-q "if 3>2 ? [may print your bigger one...]"
                                   t)))
             (format t "~%You Think The Bigger One is: ~A~%[~A]~%"
                     (if if-bigger 3 2) (if if-bigger "YES!" "WRONG!!!")))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
'(docs "将 args 连在一起构建一个 string")
'(example ((mkstr pi " pieces of " 'Pi)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
'(docs "将 args 连在一起构建 string 然后转换成 symbol")
'(example ((symb 'ar "Madi" #\L #\L 0)))

(defun copy-n-string (n &optional (thing #\space))
  (labels ((cns (i s)
             (if (plusp i)
                 (cns (1- i) (format nil "~A~A" s thing))
                 s)))
    (cns n "")))
'(docs "复制 n 个 thing 并将其构成一个字符串")
'(example ((copy-n-string 10 #\=)
           (copy-n-string 5 "<>")
           (copy-n-string 4 'io)))

(defun smember (obj string &optional ignore-case)
  (let ((string? (if (stringp string) string (format nil "~S" string))))
    (labels ((sm (o s &optional (n 0))
               (if (not-null-string s)
                   (if (uiop:string-prefix-p o s)
                       (return-from smember
                         (values (subseq string? n) n))
                       (sm o (subseq s 1) (1+ n))))))
      (sm (if ignore-case (string-upcase obj)     obj)
          (if ignore-case (string-upcase string?) string?)))))
'(docs "判断一个内容是否属于一个 string 的一部分")
'(example ((smember 'c   "AbCdE")
           (smember #\C  "AbCdE")
           (format t "~A" (smember "C"  "AbCdE"))
           (smember #\c  "AbCdE")
           (print (smember "c"  "AbCdE"))
           (smember 'cd  "AbCdE" t)
           (smember "cd" "AbCdE" t)
           (smember "ef" "AbCdE" t)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
'(docs "将一个列表压平. 接受一个带嵌套的列表, 将其压平")
'(example ((flatten '(1 (2 3) (4 (5 (6)))))))

(defun remove-if-tree (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
'(docs "remove-if 的向下递归版, 类似于 copy-tree/list. 对 tree 的每一项进行 test, 删除匹配项")
'(example ((remove-if-tree #'evenp '(1 (2 3) (4 (5 6))))
           (remove-if-tree #'consp '(1 (2 3) (4 5)))))

(defun cut-string-by (string by)
  (labels ((csb (before left by)
             (if (null-string left)
                 (list before)
                 (if (uiop:string-prefix-p by left)
                     (list before (subseq left 1))
                     (csb (mkstr before (char left 0))
                          (subseq left 1) by)))))
    (csb "" string by)))
'(docs "To cut the string by the by char")
'(example ((cut-string-by "123:xyz" #\:)
           (cut-string-by "123:" #\:)
           (cut-string-by ":xyz" #\:)
           (cut-string-by "123xyz" #\:)))

(defun cut-string (string by)
  (if (listp by)
      (labels ((have-bys (s b-list hbs)
                 (if b-list
                     (let* ((b (car b-list))
                            (if-b (smember b s)))
                       (if if-b
                           (have-bys (subseq if-b 1) (cdr b-list)
                                     (push b hbs))
                           (have-bys s (cdr b-list) (push nil hbs))))
                     (nreverse hbs)))
               (cs (s have-b-list out)
                 (if have-b-list
                     (let ((b (car have-b-list)))
                       (if b
                           (destructuring-bind (left right)
                               (cut-string-by s (car have-b-list))
                             (cs right (cdr have-b-list) (push left out)))
                           (cs s (cdr have-b-list) out)))
                     (nreverse (push s out))))
               (cs-map (cs-out have-b-list out)
                 (if have-b-list
                     (if (car have-b-list)
                         (cs-map (cdr cs-out) (cdr have-b-list)
                                 (push (car cs-out) out))
                         (cs-map cs-out (cdr have-b-list)
                                 (push "" out)))
                     (if cs-out
                         (error "Why still have cuted peace???")
                         (nreverse out)))))
        (let* ((have-bys (have-bys string by nil))
               (cs-out (cs string have-bys nil)))
          (cons (car cs-out) (cs-map (cdr cs-out) have-bys nil))))
      (cut-string-by string by)))
'(docs "按照 by 中的字符来分割 string，除了第一个元素外，其余字符看作对应的头字符，会返回一个列表，包含 by 中元素数量 +1 的元素，如果不包含某个头字符则对应顺序的元素为 \"\" 空字符串。如果 by 是一个字符而不是列表，则退化为 cut-string-by 函数")
'(example ((cut-string "12:34:56:78@xx$yy" '(#\: #\: #\@ #\$))
           (cut-string "12:34:56:78"       '(#\: #\: #\@ #\$))
           (cut-string "12:34:56:78@xx"    '(#\: #\: #\@ #\$))
           (cut-string "12:34:56:78$yy"    '(#\: #\: #\@ #\$))
           (cut-string "12:34:56:78@xx$yy" #\@)
           (cut-string "12:34:56:78@xx$yy" #\+)
           (cut-string "12:34:56:78@xx$yy" '(#\+))))

(defun split-by (string &optional (by #\Tab) ignore-case)
  (let ((by-len (length (if (stringp by) by (format nil "~A" by)))))
    (labels ((sp (left-string out)
               (multiple-value-bind (if-s s-nth)
                   (smember by left-string ignore-case)
                 (if if-s
                     (sp (subseq if-s by-len)
                         (push (subseq left-string 0 s-nth) out))
                     (nreverse (push left-string out))))))
      (sp string nil))))
'(docs "从 string 中根据 by 字符分割 string 并返回 list")
'(example ((split-by "chr1\	10000\	20000")
           (split-by "chr1haha10000hahahaha20000" 'haha t)
           (split-by "123::456::789:1011::" "::")
           (split-by "123::456::789:1011::" 'a)))

(defmacro unite (list &optional (by #\Tab))
  `(format nil (mkstr "~{~A~^" ,by "~}") ,list))
'(docs "将列表中的每个元素之间用 by 分割，生成一个字符串")
'(keys (string unite split))
'(example ((unite '(1 2 3 4))
           (unite '(A B #\Space) "::")
           (unite '("chr1" 1234 2345 "chr1" 5678 6789))
           (unite '("abc" "def") nil)))

(defun maptree (function tree)
  (if (listp tree)
      (mapcar #'(lambda (x) (maptree function x)) tree)
      (funcall function tree)))
'(docs "mapcar 的 tree 版本")
'(example ((maptree #'1+
            (make-all-list (1 2 (3 (4 5) (6)) 7 8)))
           (maptree #'square
            (make-all-list (1 2 (3 (4 5) (6)) 7 8)))))

(defmacro ignore-redef (&body body)
  `(handler-bind ((warning #'(lambda (c) (muffle-warning c))))
     ,@body))
'(docs "忽略 body 中出现的重复定义等参数")


;;;IO
(defun load-list-by (read-fun input)
  (labels ((llb (in-stream out)
             (let* ((eof (gensym))
                    (now (funcall read-fun in-stream nil eof)))
               (if (eq now eof)
                   (nreverse out)
                   (llb in-stream (push now out))))))
    (if (streamp input)
        (llb input nil)
        (with-open-file (in input)
          (llb in nil)))))
'(docs "使用 read-fun 来 read 数据，并将所有结果输入列表返回，直到文件末尾")

(defun load-lines (input)
  (load-list-by #'read-line input))
'(docs "使用 read-lin 读取整个文件并将没一行作为列表一个元素，返回列表")

(defun save-list-by (list by file &key (direction :output)
                                    (if-exists :supersede)
                                    (if-does-not-exist :create)
                                    ignore-last)
  (with-open-file (out file :direction direction
                            :if-exists if-exists
                            :if-does-not-exist if-does-not-exist)
    (format out (mkstr "~{~A" (if ignore-last "~^" "") by "~}") list)))
'(docs "将 list 写入 file 并使用 by 分割")

(defun save-lines (list file &key (direction :output)
                               (if-exists :supersede)
                               (if-does-not-exist :create)
                               ignore-last)
  (save-list-by list #\Newline file :direction direction
                                    :if-exists if-exists
                                    :if-does-not-exist if-does-not-exist
                                    :ignore-last ignore-last))
'(docs "将列表中的每个元素作为一行写入 file 文件")

(defun get-number-splited (obj-string &optional (by #\.))
  (values (split-by obj-string by)))
'(docs "将任何形式的数值字符串变成 by 左右的两个整数")
'(example ((get-number-splited "1.2")
           (get-number-splited "1")
           (get-number-splited "3.2")))

(defun parse-number (obj)
  (if (typep obj 'string)
      (cond
        ((smember "/" obj)
         (let ((obj-numbers
                 (get-number-splited obj #\/)))
           (reduce #'/
                   (mapcar #'parse-number
                           obj-numbers))))
        ((smember "e" obj)
         (let ((obj-numbers
                 (get-number-splited obj #\e)))
           (float (* (parse-number (car obj-numbers))
                     (expt 10 (parse-integer
                               (second obj-numbers)))))))
        ((smember "." obj)
         (let ((obj-numbers
                 (get-number-splited obj #\.)))
           (let ((if-plus (if (smember "-" (car obj-numbers))
                              nil t)))
             (if if-plus
                 (+ (parse-integer (car obj-numbers))
                    (/ (parse-integer
                        (second obj-numbers))
                       (expt 10 (length
                                 (second obj-numbers))))
                    0.0)
                 (- (parse-integer (car obj-numbers))
                    (/ (parse-integer
                        (second obj-numbers))
                       (expt 10 (length
                                 (second obj-numbers))))
                    0.0)))))
        (t (parse-integer obj)))
      (if (typep obj 'number) obj)))
'(docs "将数字字符串转换为整数或有理数")
'(example ((parse-number "1")
           (parse-number 1)
           (parse-number "1.2")
           (parse-number "-1.2")
           (parse-number 1.2)
           (parse-number "123.456")
           (parse-number "2345e-2")
           (parse-number "-3.2345e-2")
           (parse-number "3/2")
           (parse-number "3/2/2")))

(defun right-or-wrong-char (&optional r-or-w)
  (code-char (if r-or-w 8730 215)))
'(docs "返回对或错的符号")
'(example ((format nil "~A" (right-or-wrong-char))
           (format nil "~A" (right-or-wrong-char t))
           (format nil "~A" (right-or-wrong-char nil))
           (format nil "~A" (right-or-wrong-char 1))))

(defun color-string (&optional string color)
  (let ((colors '(("black" 30) ("red"     31) ("green" 32) ("yellow" 33)
                  ("blue"  34) ("magenta" 35) ("cyan"  36) ("white"  37))))
    (if string
        (if color
            (let ((code (if (numberp color)
                            color
                            (second (assoc (format nil "~A" color) colors
                                           :test #'string-equal)))))
              (if code
                  (format nil "~A~A~A~A~A" #\Escape (mkstr #\[ code #\m)
                          string #\Escape "[0m")))
            string)
        colors)))
'(docs "给字符串在 ansi 标准终端中上色")
'(example ((color-string)
           (color-string "123" 'red)))

(defun connect-char (&optional location)
  (let ((code-map `(("lr" ,(code-char 9472))
                    ("zz" ,(code-char 9472))
                    ("ud" ,(code-char 9474))
                    ("ii" ,(code-char 9474))
                    ("lu" ,(code-char 9484))
                    ("ru" ,(code-char 9488))
                    ("ld" ,(code-char 9492))
                    ("rd" ,(code-char 9496))
                    ("lm" ,(code-char 9500))
                    ("rm" ,(code-char 9508))
                    ("um" ,(code-char 9516))
                    ("dm" ,(code-char 9524))
                    ("xx" ,(code-char 9532))
                    ("oo" #\Space))))
    (if location
        (if (listp location)
            (if (listp (car location))
                (format nil "~{~A~^~%~}" (mapcar #'connect-char location))
                (format nil "~{~A~}" (mapcar #'connect-char location)))
            (let ((char (second (assoc (mkstr location) code-map
                                       :test #'string-equal))))
              (format nil "~A" (if char char location))))
        (dolist (line code-map)
          (format t "<~A>: \"~A\"~%" (car line) (second line))))))
'(docs "获取连接字符的字符串")
'(example ((connect-char)
           (connect-char 'lu)
           (connect-char '(lm zz zz rm))
           (format t "~A~%~A~%~A~%~A"
            (connect-char '(lu zz zz zz zz zz ru))
            (connect-char '(ii oo lu zz ru oo ii))
            (connect-char '(ii oo ld zz rd oo ii))
            (connect-char '(ld zz zz zz zz zz rd)))
           (format t "~a"
            (connect-char
             '((lu zz zz zz um zz zz zz um zz zz zz ru)
               (ii oo 1  oo ii oo 2  oo ii oo 3  oo ii)
               (lm zz zz zz xx zz zz zz xx zz zz zz rm)
               (ii oo 4  oo ii oo 5  oo ii oo 6  oo ii)
               (lm zz zz zz xx zz zz zz xx zz zz zz rm)
               (ii oo 7  oo ii oo 8  oo ii oo 9  oo ii)
               (ld zz zz zz dm zz zz zz dm zz zz zz rd))))))

(defmacro quit-n (n &body body)
  `(progn
     ,@body
     (uiop:quit ,n)))
'(docs "运行一些命令，然后用 n 退出")

(defmacro quit-1 (&body body)
  `(progn
     ,@body
     (uiop:quit 1)))
'(docs "运行一些命令，然后用 1 退出")

(defmacro exit-1 (&body body)
  `(quit-1 ,@body))

(defmacro while (test &body body)
  (with-all-gensyms (wh)
    `(labels ((,wh ()
                (when ,test
                  ,@body
                  (,wh))))
       (,wh))))
'(docs "while")
'(example ((let ((x 0))
             (while (< (incf x) 10)
               (format t "~A: ~A~%" x (* x x)))
             (values (format nil "~A~%~A" 1 2) 3 4))))

(defun replace-string (string ori-thing new-thing)
  (let ((ori-len (length (mkstr ori-thing))))
    (labels ((rc (left out)
               (if (or (null left) (null-string left))
                   out
                   (if (uiop:string-prefix-p ori-thing left)
                       (rc (subseq left ori-len) (mkstr out new-thing))
                       (rc (subseq left 1) (mkstr out (char left 0)))))))
      (rc string ""))))
'(docs "替换字符串")
'(example ((replace-string "1234567654321" "34" "xyz")
           (replace-string "1234567654321" #\4 'xyz)
           (replace-string "1234567654321" "555" #\x)))
