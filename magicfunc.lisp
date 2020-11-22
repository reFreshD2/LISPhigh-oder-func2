(setq data '(5 2 4 2 4 6))

(defun listOfLists (arg)
    (list arg)
)
(defun MAGIC1 (myList)
    (mapcar 'listOfLists myList)
)
(defun countOfElem (el myList countEl)
(cond
    ((NULL myList) countEl)
    ((eq el (CAR myList)) (countOfElem el (CDR myList) (+ countEl 1)))
    (T (countOfElem el (CDR myList) countEl))
)
)
(defun listCount (arg)
    (list arg (countOfElem arg data 0))
)
(defun MAGIC2 (myList)
    (mapcar 'listCount myList)
)
(defun myDiv (myList)
(cond
    ((NULL (CDR myList)) NIL)
    (T (cons (- (CAR myList) (CADR myList)) (myDiv (CDR myList))))
)
)
(defun MAGIC3 (myList)
    (funcall 'myDiv myList)
)
(defun mySum (myList)
(cond
    ((NULL (CDR myList)) NIL)
    (T (cons (+ (CAR myList) (CADR myList)) (mySum (CDR myList))))
)
)
(defun MAGIC4 (myList)
    (funcall 'mySum myList)
)

(print (MAGIC1 data))
(print (MAGIC2 data))
(print (MAGIC3 data))
(print (MAGIC4 data))