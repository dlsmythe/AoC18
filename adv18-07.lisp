;; sbcl --noinform --load adv18-07.lisp [-v n] [-p <1|2>] [inputfile]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is EUGJKYFQSCLTWXNIZMAPVORDBH
;; Correct answer for part 2 is 

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(load "mergesort.lisp")

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-07.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defstruct node (name nil) (dependencies nil))

(defparameter *node-list* nil)
(defparameter *resolved-list* nil)

(defun node-resolved (name)
  (find name *resolved-list* :test #'string=))

(defun add-resolved (name)
  (unless (node-resolved name)
    (push name *resolved-list*)))

(defun find-node (name)
  (find name *node-list* :test (lambda (x y) (string= x (node-name y)))))

(defun add-node (name)
  (let ((node (find-node name)))
    (unless node
      (setf node (make-node :name name))
      (push node *node-list*))
    node))

(defun has-unresolved-dependencies (name)
  (vprint 1 "unresolved-dependencies for ~a: " name)
  (let ((node (find-node name)))
    (dolist (d (node-dependencies node))
      (unless (node-resolved d)
	(vprint 1 "~a~%" d)
	(return-from has-unresolved-dependencies t))))
  (vprint 1 "~a~%" "None!")
  nil)

(defun add-dependency (preceder-name depender-name)
  (add-node preceder-name)
  (let ((depender-node (add-node depender-name)))
    (unless (find preceder-name (node-dependencies depender-node) :test #'string=)
      (vprint 1 "~a depends on ~a~%" depender-name preceder-name)
      (push preceder-name (node-dependencies depender-node)))))

(defun read-input (filename)
  (vprint 1 "Reading input file ~a~%" filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
       while line
       do (multiple-value-bind (matched l)
	      (cl-ppcre:scan-to-strings
	       "Step (\\S) must be finished before step (\\S) can begin." (string-trim '(#\Newline) line))
	    (when (not matched)
	      (format t "bad line: ~a~%" line)
	      (sb-ext:exit :code 1))
	    (add-dependency (elt l 0) (elt l 1))))))

(defun next-node ()
  (let ((nodes-with-no-unresolved-dependencies nil))
    (dolist (node *node-list*)
      (unless (node-resolved (node-name node))
	(block skip-it
	  (dolist (dep (node-dependencies node))
	    (unless (node-resolved dep)
	      (return-from skip-it)))
	  (push (node-name node) nodes-with-no-unresolved-dependencies))))
    (vprint 1 "New roots: ~a~%" (sort nodes-with-no-unresolved-dependencies #'string<))
    (car (sort nodes-with-no-unresolved-dependencies #'string<))))

(defun walk-nodes ()
  (do ((n-name (next-node) (next-node)))
      ((null n-name))
    (add-resolved n-name)
    (format t "~a" n-name))
  (format t "~%"))

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

  (read-input *input-file*)
  (vprint 1 "input read, ~a items~%" (length *node-list*))
  (dolist (n *node-list*)
    (vprint 1 "Node ~a dependencies:" (node-name n))
    (dolist (d (node-dependencies n))
      (vprint 1 " ~a" d))
    (vprint 1 "~%"))

  (when (= *part* 1)
      (walk-nodes))

  0)
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
