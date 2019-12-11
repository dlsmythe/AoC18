;; sbcl --noinform --load adv18-05.lisp [-v n] [-p <1|2>] [inputfile]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is 11108
;; Correct answer for part 2 is 5094 (F/f)

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-05.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-input (fname)
  (let ((input nil)
	(char-count 0))
    (with-open-file (in fname)
      (do* ((ch (read-char in nil 'the-end) (read-char in nil 'the-end)))
	   ((not (characterp ch)))
	(unless (char= #\Newline ch)
	  (incf char-count)
	  (push ch input))))
    (values (concatenate 'string (nreverse input)) char-count)))

(defun react (input)
  (do ((input-len (length input))
       (i 0)
       (iteration 0 (1+ iteration)))
      ((= (1- input-len) i) input)
    (when (= 0 (mod iteration 1000))
      (vprint 1 "iteration ~a~%" iteration))
    (vprint 1 "~a ~a ~a~%" input-len i input)
    (let ((cur (elt input i))
	  (next (elt input (1+ i))))
      ;;	  (vprint 2 "~a ~a ~a~%" i cur next)
      (cond ((and (upper-case-p cur) (upper-case-p next))
	     (vprint 2 "both uppercase~%")
	     (incf i))
	    ((and (lower-case-p cur) (lower-case-p next))
	     (vprint 2 "both lowercase~%")
	     (incf i))
	    ((char= (char-upcase cur) (char-upcase next))
	     (vprint 2 "differ only by case~%")
	     (setf input (concatenate 'string (subseq input 0 i) (subseq input (+ 2 i))))
	     (decf input-len 2)
	     (if (> i 0)
		 (decf i)))
	    (t
	     (vprint 2 "nada~%")
	     (incf i))))))
  
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

  (multiple-value-bind (input input-len)
      (read-input *input-file*)
    (vprint 1 "input read, ~a items~%" input-len)

    (when (= *part* 1)
      (let ((result (react input)))
	(format t "Result: ~a~%Final length: ~a~%" result (length result))))

    (when (= *part* 2)
      (let ((min-len most-positive-fixnum)
	    (min-char #\0))
	(loop for c from (char-code #\a) upto (char-code #\z) do
	     (let* ((input-copy (copy-seq input))
		    (reacted-str (react (remove (char-upcase (code-char c)) (remove (code-char c) input-copy))))
		    (reacted-len  (length reacted-str)))
	       (when (< reacted-len min-len)
		   (setf min-len reacted-len)
		   (setf min-char c))
	       (format t "~a/~a ~a~%" (char-upcase (code-char c)) (code-char c) reacted-len)))
	(format t "Best to remove was ~a/~a~%" (code-char min-char) (char-upcase (code-char min-char)))))

    0))
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
