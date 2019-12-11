;; sbcl --noinform --load adv18-03.lisp [-v n] [-p <1|2>] [inputfile]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is 101565
;; Correct answer for part 2 is 656

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-03.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defstruct claim (id 0) (x 0) (y 0) (w 0) (h 0))

(defmacro nested-loop (syms dimensions &body body)
  "Iterates over a multidimensional range of indices.
   
   SYMS must be a list of symbols, with the first symbol
   corresponding to the outermost loop. 
   
   DIMENSIONS will be evaluated, and must be a list of 
   dimension sizes, of the same length as SYMS.

   Example:
    (nested-loop (i j) '(10 20) (format t '~a ~a~%' i j))

  "
  (unless syms (return-from nested-loop `(progn ,@body))) ; No symbols
  
  ;; Generate gensyms for dimension sizes
  (let* ((rank (length syms))
         (syms-rev (reverse syms)) ; Reverse, since starting with innermost
         (dims-rev (loop for i from 0 below rank collecting (gensym))) ; innermost dimension first
         (result `(progn ,@body))) ; Start with innermost expression
    ;; Wrap previous result inside a loop for each dimension
    (loop for sym in syms-rev for dim in dims-rev do
         (unless (symbolp sym) (error "~S is not a symbol. First argument to nested-loop must be a list of symbols" sym))
         (setf result
               `(loop for ,sym from 0 below ,dim do
                     ,result)))
    ;; Add checking of rank and dimension types, and get dimensions into gensym list
    (let ((dims (gensym)))
      `(let ((,dims ,dimensions))
         (unless (= (length ,dims) ,rank) (error "Incorrect number of dimensions: Expected ~a but got ~a" ,rank (length ,dims)))
         (dolist (dim ,dims)
           (unless (integerp dim) (error "Dimensions must be integers: ~S" dim)))
         (destructuring-bind ,(reverse dims-rev) ,dims ; Dimensions reversed so that innermost is last
           ,result)))))

(defun read-input (filename)
  (vprint 1 "Reading input file ~a~%" filename)
  (let ((ret (make-array '(1) :fill-pointer 0 :adjustable t))
	(max-width 0)
	(max-height 0))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
	 while line
	 do (multiple-value-bind (matched l)
		(cl-ppcre:scan-to-strings
		 "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" (string-trim '(#\Newline) line))
	      ;;(format t "line ~A: p/w: ~A~%" line l)
	      (when (not matched)
		(format t "bad line: ~a~%" line)
		(sb-ext:exit :code 1))
	      (let ((id (parse-integer (elt l 0)))
		    (x (parse-integer (elt l 1)))
		    (y (parse-integer (elt l 2)))
		    (w (parse-integer (elt l 3)))
		    (h (parse-integer (elt l 4))))
		(if (> (+ x w) max-width)
		    (setf max-width (+ x w)))
		(if (> (+ y h) max-height)
		    (setf max-height (+ y h)))
		(vector-push-extend (make-claim :id id :x x :y y :w w :h h) ret)))))
    (values ret (length ret) max-width max-height)))

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

  (multiple-value-bind (claims num-claims max-width max-height)
      (read-input *input-file*)
    (vprint 1 "input read, ~a items, fabric is ~ax~a~%" num-claims max-width max-height)

    (let ((fabric (make-array (list max-height max-width) :initial-element 0)))
      
;;      (when (= *part* 1)
	(let ((overlaps 0))
	  (loop for claim across claims do
	       (loop for row from (claim-y claim) below (+ (claim-y claim) (claim-h claim)) do
		    (loop for col from (claim-x claim) below (+ (claim-x claim) (claim-w claim)) do
			 (vprint 3 "setting <~a> [~a,~a] to ~a~%" (claim-id claim) row col (1+ (aref fabric row col)))
		       (if (= (incf (aref fabric row col)) 2)
			   (incf overlaps)))))
	  (format t "~a square inches of multiply-claimed fabric~%" overlaps))
      ;; (nested-loop (row col) (array-dimensions fabric)
      ;; 	     (let ((val  (aref fabric row col)))
      ;; 	       (format t "~a" (cond ((= 0 val) " ")
      ;; 				    ((> val 9) "X")
      ;; 				    (t val))))
      ;; 	     (when (= col (1- max-width))
      ;; 	       (format t "~%"))))
      
      (when (= *part* 2)
	(block all-claims
	  (loop for claim across claims do
	       (block this-claim
		 (loop for row from (claim-y claim) below (+ (claim-y claim) (claim-h claim)) do
		      (loop for col from (claim-x claim) below (+ (claim-x claim) (claim-w claim)) do
			   (if (> (aref fabric row col) 1)
			       (return-from this-claim 0))))
		 (format t "Claim ~a has no overlap~%" (claim-id claim))
		 (return-from all-claims 0)))))

      0)))
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
