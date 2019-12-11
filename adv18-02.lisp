;; sbcl --noinform --load adv18-02.lisp [-v n]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is 7470
;; Correct answer for part 2 is kqzxdenujwcstybmgvyiofrwrd and kqzxdenujwcstybmgvyiofrard differ in one letter, giving kqzxdenujwcstybmgvyiofrrd

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-02.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     ,@body))

(defun read-input (filename)
  (vprint 1 "Reading input file ~a~%" filename)
  (let ((ids (make-array '(1) :fill-pointer 0 :adjustable t)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
	 while line
	 do (vector-push-extend line ids)))
    ids))

(defun run-length (s c)
  "Returns number of 'c' characters at the beginning of 's'"
  (let ((count 0))
    (do ((i 0 (1+ i)))
	((or (>= i (length s)) (not (equal (char s i) c))) count)
      (incf count))))

(defun main (args)

  ;; Parse command-line options
  (let ((opts '(("v" :required 0)
		("p" :required 0))))
    (multiple-value-bind (new-args vals) (getopt:getopt args opts)
      (dolist (arg vals)
	(cond ((string= "v" (car arg))
	       (setf *verbose* (parse-integer (cdr arg))))
	      ((string= "p" (car arg))
	       (setf *part* (parse-integer (cdr arg))))))
      (setf args new-args)))

  ;; The first of the remaining args, if any, replaces the default input file name.
  (if (> (length args) 1) (setf *input-file* (cadr args)))

  (format t "Reading input from ~a~%" *input-file*)
  (let* ((ids (read-input *input-file*))
	 (length-ids (length ids)))
    (vprint 1 "input read, ~a items~%" length-ids)

    (when (= *part* 1)
      ;; do the counts
      (let ((count2s 0)
	    (count3s 0))
	(loop for rawid across ids
	   do (let ((has2 0)
		    (has3 0)
		    (id (sort rawid #'char<)))
		;;	      (format t "doing ~a (~a)~%" rawid id)
		;; NB: have to count the 3's first, so they don't double-count as 2's
		(do ((i 0))
		    ((>= i (1- (length id))))
		  (let* ((sid (subseq id i))
			 (len (run-length sid (char sid 0))))
		    (vprint 2 (format t " ~a ~a ~a~%" len (char sid 0) sid))
		    (cond
		      ((= len 3)
		       (setf has3 1))
		      ((= len 2)
		       (setf has2 1)))
		    (incf i len)))
		(when (> has2 0)
		  (format t "~a has 2s~%" id)
		  (incf count2s))
		(when (> has3 0)
		  (format t "~a has 3s~%" id)
		  (incf count3s))))
	(format t "~a inputs. 2s ~a 3s ~a~%" (length ids) count2s count3s)
	(format t "checksum is ~a~%" (* count2s count3s))))

    (when (= *part* 2)
      (loop for main-index below length-ids
	 do (let ((first-id (elt ids main-index)))
	      (vprint 1 "outer [~a] ~a~%" main-index first-id)
	      (loop for second-id across (subseq ids (1+ main-index)) do
		   (let ((first-len (length first-id))
			 (second-len (length second-id)))
		     (when (= first-len second-len)
		       (let ((same-list '())
			     (different-list '()))
			 (loop for i below first-len
			    do (let ((first-char (elt first-id i))
				     (second-char (elt second-id i)))
				 (if (char= first-char second-char)
				     (push first-char same-list)
				     (push first-char different-list))))
			 (when (= 1 (length different-list))
			   (format t "~a and ~a differ in one letter, giving ~a~%" first-id second-id (nreverse (concatenate 'string same-list)))))))))))
	    
    0))
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
