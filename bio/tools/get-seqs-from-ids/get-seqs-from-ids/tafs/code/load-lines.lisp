(defun load-seqs (seqs-file)
  (let ((seqs-list
          (remove-if #'hanky:null-string
                     (remove-if #'(lambda (line)
                                    (uiop:string-prefix-p #\# line))
                                (hanky:load-lines seqs-file)))))
    (labels ((ls (left now out)
               (cond
                 ((null left)
                  (nreverse (push (nreverse now) out)))
                 ((uiop:string-prefix-p #\> (car left))
                  (ls (cdr left) (list (car left)) (push (nreverse now) out)))
                 (t
                  (ls (cdr left) (push (car left) now) out)))))
      (ls (cdr seqs-list) (list (car seqs-list)) nil))))

(defun get-seqs-from-ids (seqs-file ids-file)
  (let ((seqs (load-seqs seqs-file))
        (ids  (mapcar #'hanky:clean-string (hanky:load-lines ids-file))))
    (dolist (a-seq (remove-if-not
                    #'(lambda (seq)
                        (let ((seq-id (car seq)))
                          (some #'(lambda (id)
                                    (uiop:string-prefix-p
                                     (format nil ">~A " id)
                                     (format nil "~A " seq-id)))
                                ids)))
                    seqs))
      (format t "~{~A~%~}" a-seq))))

;;(main)
(defun main ()
  (handler-case
      (hanky:let-args-opts (all-args *posix-argv*) (opts nil)
        (let* ((args (cdr all-args))
               (seqs-file (first  args))
               (ids-file  (second args))
               (if-seqs-file (probe-file seqs-file))
               (if-ids-file  (probe-file ids-file)))
          (cond
            ((and if-seqs-file if-ids-file)
             (get-seqs-from-ids if-seqs-file if-ids-file))
            (t
             (format *error-output* "[Usage]: taf-get-seqs-from-ids [seqs.fasta] [ids.txt]~%")))))
    (condition (e)
      (format *error-output* "[Condition]: ~A~%[Usage]: taf-get-seqs-from-ids [seqs.fasta] [ids.txt]~%"
              e))))

(main)
