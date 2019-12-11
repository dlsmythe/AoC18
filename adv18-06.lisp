;; sbcl --noinform --load adv18-06.lisp [-v n] [-p <1|2>] [inputfile]
;;  -p n  select puzzle part (1 or 2)
;;  -v n  set verbosity to level n
;;
;; Correct answer for part 1 is 3933
;; Correct answer for part 2 is 41145

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *part* 1)
(defparameter *verbose* 0)
(defparameter *input-file* "adv18-06.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-input (filename)
  (vprint 1 "Reading input file ~a~%" filename)
  (let ((ret (make-array '(1) :fill-pointer 0 :adjustable t))
	(element-id -1)
	(min-x most-positive-fixnum)
	(min-y most-positive-fixnum)
	(max-x 0)
	(max-y 0))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
	 while line
	 do (multiple-value-bind (matched l)
		(cl-ppcre:scan-to-strings
		 "(\\d+), (\\d+)" (string-trim '(#\Newline) line))
	      (when (not matched)
		(format t "bad line: ~a~%" line)
		(sb-ext:exit :code 1))
	      (let ((x (parse-integer (elt l 0)))
		    (y (parse-integer (elt l 1))))
		(cond ((< x min-x)
		       (setf min-x x))
		      ((< y min-y)
		       (setf min-y y))
		      ((> x max-x)
		       (setf max-x x))
		      ((> y max-y)
		       (setf max-y y)))
		(vector-push-extend (list (incf element-id) x y 0) ret))))) ; final 0 is for the area
    (values ret min-x min-y max-x max-y)))

(defun manhattan-distance (row1 col1 row2 col2)
  (+ (abs (- row1 row2)) (abs (- col1 col2))))

(defun id-with-min-manhattan-distance (input row col)
  (let ((min-id -1)
	(min-dist most-positive-fixnum))
    (loop for item across input do
	 (let ((dist (manhattan-distance (third item) (second item) row col)))
	   (when (< dist min-dist)
	     (setf min-id (first item))
	     (setf min-dist dist))))
    min-id))

(defun find-item (input id)
  (loop for item across input do
       (if (= id (first item))
	   (return-from find-item item)))
  (format t "OOOPS: bad item ~a~%" id)
  (sb-ext:exit :code 1))

;; Returns new current area of the given ID
(defun inc-area (input id)
  (let ((item (find-item input id)))
    (if (= most-positive-fixnum (fourth item))
	most-positive-fixnum
	(incf (fourth item)))))

(defun set-area (input id val)
  (let ((item (find-item input id)))
    (setf (fourth item) val)))

(defun calculate-areas (input grid w h)
  (let ((largest-id -1)
	(largest-area 0))
    (loop for row below h do
	 (loop for col below w do
	      (cond ((or (= 0 row) (= 0 col) (= (1- h) row) (= (1- w) col))
		     (set-area input (aref grid row col) most-positive-fixnum))
		    (t
		     (let ((new-area (inc-area input (aref grid row col))))
		       (when (and (/= most-positive-fixnum new-area) (> new-area largest-area))
			 (setf largest-id (aref grid row col))
			 (setf largest-area new-area)))))))
    (values largest-id largest-area)))
	 
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

  (multiple-value-bind (input min-x min-y max-x max-y) (read-input *input-file*)
    (vprint 1 "input read, ~a items~%" (length input))
    ;; make 2d array of ints of the calculated size, initialized to 0
    (let ((grid (make-array (list (1+ max-y) (1+ max-x)) :initial-element 0)))
      (vprint 1 "min-x ~a min-y ~a max-x ~a max-y ~a~%" min-x min-y max-x max-y)

      (when (= *part* 1)
	;; set each element to id of item with minimum manhattan distance
	(loop for row upto max-y do
	     (loop for col upto max-x do
		  (setf (aref grid row col) (id-with-min-manhattan-distance input row col))))
	;; walk the grid and determine area of each input element.
	;; - Mark those that touch the edge as infinite.
	;; - Keep track of the largest area seen so far (element-id and size)
	(multiple-value-bind (largest-id largest-area) (calculate-areas input grid (1+ max-x) (1+ max-y))
	  ;; Print largest size
	  (format t "Largest area is around item with id ~a having area ~a~%" largest-id largest-area)))

      (when (= *part* 2)
	;; for each grid space, calculate the sum of the manhattan distances to each input element
	(loop for row upto max-y do
	     (loop for col upto max-x do
		  (let ((sum 0))
		    (loop for item across input do
			 (incf sum (manhattan-distance (third item) (second item) row col)))
		    (setf (aref grid row col) sum))))
	;; find the size of the area that is less than 10000 units from all input elements
	(let ((area 0))
	  (loop for row upto max-y do
	       (loop for col upto max-x do
		    (when (< (aref grid row col) 10000)
		      (incf area))))
	  (format t "size of area < 10000 manhattan distance from all input items is ~a~%" area)))

      0)))
	  
(sb-ext:exit :code (main sb-ext:*posix-argv*))
