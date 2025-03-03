(defun head->-p (string &optional (id ""))
  (uiop:string-prefix-p
   (format nil ">~A~A" id (if (hanky:null-string id) "" " "))
   (format nil "~A~A" string (if (hanky:null-string id) "" " "))))

(defun search-ids (seqs-stream ids)
  (let ((eof (gensym)))
    (labels ((si (&optional if-in)
               (let ((line (read-line seqs-stream nil eof)))
                 (cond
                   ((eql line eof))
                   ((head->-p line)
                    (if (some #'(lambda (id) (head->-p line id)) ids)
                        (progn (format t "~A~%" line)
                               (finish-output)
                               (si t))
                        (si nil)))
                   (t
                    (when if-in (format t "~A~%" line))
                    (si if-in))))))
      (si)
      (finish-output))))

(defun get-seqs-from-ids (seqs-file ids-file)
  (with-open-file (in seqs-file)
    (search-ids in (mapcar #'hanky:clean-string (hanky:load-lines ids-file)))))

(defun main ()
  (hanky:let-args-opts (args) (opts nil)
    (let ((seqs-file (second args))
          (ids-file  (third  args)))
      (if (and (probe-file seqs-file) (probe-file ids-file))
          (get-seqs-from-ids seqs-file ids-file)
          (format *error-output*
                  "~A [Usage]: get-seqs-from-ids [seqs.fasta] [ids.txt]"
                  (hanky:color-string "[Error]" 'red))))))

(main)
