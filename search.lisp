; jump to chapter 19 - Examples involving Search

(setf (get 's 'neighbours) '(a d)
      (get 'a 'neighbours) '(s b d)
      (get 'b 'neighbours) '(a c e)
      (get 'c 'neighbours) '(b)
      (get 'd 'neighbours) '(s a e)
      (get 'e 'neighbours) '(d b f)
      (get 'f 'neighbours) '(e))

;(defun search (start finish &optional (queue (list (list start))))
;  (cond
;    ((null queue) nil)
;    ((eq finish (first (first queue)))
;     (reverse (first queue)))
;    (t (search start finish <new queue>))))

(defun extend-circular-problem (path)
  (mapcar #'(lambda (new-node) (cons new-node path)) (get (first path) 'neighbours)))

(defun extend (path)
;  (print '(start of extend))
;  (print (reverse path))
  (mapcar #'(lambda (new-node) (cons new-node path)) ;form new paths 
	  (remove-if #'(lambda (neighbour) (member neighbour path))
		     (get (first path) 'neighbours))))


(defun depth-first  (start finish &optional (queue (list (list start))))
  (print '(start of depth-first))
  (cond
    ((null queue) nil)
    ((eq finish (first (first queue)))
     (reverse (first queue)))
    (t (depth-first start finish (append (extend (first queue)) (rest queue))))))


(defun breadth-first  (start finish &optional (queue (list (list start))))
;  (print '(start of breadth-first))
  (cond
    ((null queue) nil)
    ((eq finish (first (first queue)))
     (reverse (first queue)))
    (t (breadth-first start finish (append (rest queue) (extend (first queue)) )))))

; change for git
