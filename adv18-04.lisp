;; sbcl --noinform --load adv18-04.lisp [-v n] [-p <1|2>] [inputfile]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is 19874 (guard 523 minute 38)
;; Correct answer for part 2 is 22687 (guard 463 at minute 49)

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-04.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defstruct guardmap (id 0) (shiftmap (make-array '(60) :initial-element 0)))

(defun make-guard (guards str)
  (let ((id (parse-integer (subseq str 1) :junk-allowed t))
	(ret nil))
    (block find-guard
      (loop for guard across guards do
	   (when (= id (guardmap-id guard))
	     (setf ret guard)
	     (return-from find-guard))))
    (when (null ret)
      (setf ret (make-guardmap :id id))
      (format t "Created guard #~a~%" (guardmap-id ret))
      (vector-push-extend ret guards))
    ret))

(defun process-events (events)
  (let ((guards (make-array '(1) :fill-pointer 0 :adjustable t))
	(cur-guard nil)
	(nod-off-time nil))
    (loop for event across events do
	 (progn
	   (format t "event:  [~a] ~a ~a~%"  (first event) (second event) (third event))
	   (cond ((string= (second event) "Guard")
		  (setf cur-guard (make-guard guards (third event))))
		 ((string= (second event) "falls")
		  (setf nod-off-time (parse-integer (subseq (first event) 14 16))))
		 ((string= (second event) "wakes")
		  (let ((wake-up-time (parse-integer (subseq (first event) 14 16))))
		    (format t "guard ~a += ~2d-~2d " (guardmap-id cur-guard) nod-off-time wake-up-time)
		    (loop for i from nod-off-time below wake-up-time do
			 (incf (elt (guardmap-shiftmap cur-guard) i)))
		    (loop for i below 60 do
			   (format t "~2d" (elt (guardmap-shiftmap cur-guard) i)))
		    (format t "~%")))
		 (t
		  (format t "Bad event: [~a] ~a ~a~%"  (first event) (second event) (third event))
		  (sb-ext:exit :code 1)))))
    guards))

(defun read-input (filename)
  (vprint 1 "Reading input file ~a~%" filename)
  (let ((ret (make-array '(1) :fill-pointer 0 :adjustable t)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
	 while line
	 do (multiple-value-bind (matched l)
		(cl-ppcre:scan-to-strings
		 "\\[([^]]+)\\] (\\S+) (.+)" (string-trim '(#\Newline) line))
	      ;;(format t "line ~A: p/w: ~A~%" line l)
	      (when (not matched)
		(format t "bad line: ~a~%" line)
		(sb-ext:exit :code 1))
	      (let ((timestamp (elt l 0))
		    (keyword (elt l 1))
		    (suffix (elt l 2)))
		(vector-push-extend (list timestamp keyword suffix) ret)))))
    (sort ret (lambda (s1 s2) (string< (first s1) (first s2))))
    (values ret (length ret))))

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

  (multiple-value-bind (eventv num-events)
      (read-input *input-file*)
    (vprint 1 "input read, ~a events~%" num-events)

    (loop for event across eventv do
	 (vprint 2 "[~a] ~a ~a~%" (first event) (second event) (third event)))

    (when (= *part* 1)
      (let ((guards (process-events eventv))
	    (max-guard-id 0)
	    (max-guard-time 0)
	    (max-guard-minute 0))
	(loop for guard across guards do
	     (let ((max-min 0)
		   (max-val 0)
		   (total-time 0))
	       (format t " guard ~4d: " (guardmap-id guard))
	       (loop for i below 60 do
		    (let ((val (elt (guardmap-shiftmap guard) i)))
		      (format t "~2d" val)
		      (when (> val max-val)
			(setf max-min i)
			(setf max-val val))
		      (incf total-time val)))
	       (format t "  ~a~%" total-time)
	       (when (> total-time max-guard-time)
		 (setf max-guard-id (guardmap-id guard))
		 (setf max-guard-time total-time)
		 (setf max-guard-minute max-min))))
	(format t "max guard ~a at minute ~a -- answer: ~a~%" max-guard-id max-guard-minute (* max-guard-id max-guard-minute))))

    (when (= *part* 2)
      (let ((guards (process-events eventv))
	    (max-guard-id 0)
	    (max-guard-time 0)
	    (max-guard-minute 0))
	(loop for guard across guards do
	     (let ((max-min 0)
		   (max-val 0))
	       (format t " guard ~4d: " (guardmap-id guard))
	       (loop for i below 60 do
		    (let ((val (elt (guardmap-shiftmap guard) i)))
		      (format t "~2d" val)
		      (when (> val max-val)
			(setf max-min i)
			(setf max-val val))))
	       (format t "  ~a@~a~%" max-val max-min)
	       (when (> max-val max-guard-time)
		 (setf max-guard-id (guardmap-id guard))
		 (setf max-guard-time max-val)
		 (setf max-guard-minute max-min))))
	(format t "max guard ~a at minute ~a -- answer: ~a~%" max-guard-id max-guard-minute (* max-guard-id max-guard-minute))))

    0))
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
