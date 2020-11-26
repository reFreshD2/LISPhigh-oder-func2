(setq data '(5 2 4 2 4 6))

(defun listOfLists (arg)
    (list arg)
)

(defun myMap (myFunc list)
(cond
    ((NULL list) NIL)
    (T (cons (funcall myFunc (CAR list)) (myMap myFunc (CDR list))))
)
)

(defun listCount (arg)
    (defun countOfElem (el myList countEl)
        (cond
        ((NULL myList) countEl)
        ((eq el (CAR myList)) (countOfElem el (CDR myList) (+ countEl 1)))
        (T (countOfElem el (CDR myList) countEl))
        )
    )
    (list arg (countOfElem arg data 0))
)

(defun myDiv (arg1 arg2)
    (- arg1 arg2)
)

(defun mySum (arg1 arg2)
    (+ arg1 arg2)
)

(defun mySibling (myFunc list)
(cond
    ((NULL (CDR list)) NIL)
    (T (cons (funcall myFunc (CAR list) (CADR list)) (mySibling myFunc (CDR list))))
)
)

(print (myMap 'listOfLists data))
(print (myMap 'listCount data))
(print (mySibling 'myDiv data))
(print (mySibling 'mySum data))