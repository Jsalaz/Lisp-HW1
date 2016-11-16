;;;  Jorge Salazar
;;;  CSC345: Paragidms 
;;;  Professor R. Wyatt
;;;  Homework 2

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-1-all, adds one to every element of the list
;; 1	Checks if L is a list
;; 2	Checks if L is at the end of the list
;; 3	Checks if every element of the list is a number
;; 4	If none of the previous conditions are true then concatenate
;;          one plus the first element of L and recursively call
;;	    add-1-all with the rest of L
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-1-all (L)
  (cond ((not (listp L)) nil) 		 				;; 1
        ((endp L) nil)			 				;; 2
	((not (every #'integerp L)) nil) 				;; 3
	(t (cons (1+ (first L))(add-1-all (rest L))))))			;; 4 
 

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-replace, switches element e1 with e2 if it in list L
;; 1	Checks if L is a list, wasnt necessary
;; 2	Checks if L is at the end of the list
;; 3	Equal checks if the first element of L is the same as e1
;; 	   If true the e2 replaces the first element and recursively calls
;;         my-replace with e1, e2 and the rest of L
;; 4	Checks if the first element of L is a list
;;	   If true it concatenates the recursive call of e1, e2 with 
;;         the first of L
;; 5	   with the recrusive call of e1, e2 with the rest of L
;; 6	If no condition is true then the function concatenates the first of L
;;	   and recursively calls e1, e2 with the rest of L
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-replace (e1 e2 L) 
  (cond ((not (listp L)) nil)   					;; 1
	((endp L) nil)						      	;; 2
	((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))  	;; 3
	((listp (first L)) (cons (my-replace e1 e2 (first L)) 	      	;; 4
				 (my-replace e1 e2 (rest L))))	      	;; 5
	(t (cons (first L) (my-replace e1 e2 (rest L))))))	      	;; 6


;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Book's Fibonacci function found on Lisp 3rd ed. page 73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci (n)
  (if (or (= n 0) (= n 1))           ;; Checks if n equals 0 or if n equals 1
      1                              ;;  if either is true then return 1
      (+ (fibonacci (- n 1))         ;;  else perform a double recursive call
         (fibonacci (- n 2)))))      ;;  with n-1 and n-2


;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fibonacci function tail recursive. This Tail Recursive Fiboncacci function 
;;; calls the fibonacci tail recursive auxillary function to perform the 
;;; calculation which eliminates the need for double recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci-TR (n)          ;; Calls the fibonacci-tr-aux function
  (fibonacci-TR-aux n 0 1))      ;; with the number of terms n and the initial
                                 ;; elements of 0 and 1

(defun fibonacci-TR-aux (terms n1 n2)
  (if (= terms 0)                ;; if the number of terms is 0 then it return
      n2                                             ;; n2 (which is one)       
      (fibonacci-TR-aux (- terms 1) n2 (+ n1 n2))))  ;; else fibonacci-TR-aux
                                                     ;; is recursively called 
                                                     ;; with terms-1 n2 and
                                                     ;; the sum of n1 and n2
;;;===========================================================================
;;; END 


;;;===========================================================================
;;;FOR USE IN HOMEWORK 1

;;;==================================================================================
;;; a macro to do FOR loops  ;; equivalent to Java for-loop control: for (int var=start, var<=stop; var+=update) body
(defmacro for ((var start stop update) &body body)
  (let ((gstop (gensym))                    ;; generate new symbols, GUARANTEED to be new; prevents capture
	(gupdate (gensym)))
    `(do ( (,gupdate ,update)               ;; needed so that the update expression is evaluated just once
	   (,var ,start (+ ,gupdate ,var))  ;; needed so that the stop expression is evaluated just once
	   (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; EXAMPLE
;; CL-USER> (for (i 1 6 1) (print i))     ;; equivalent to Java for-loop control: (for int i=1, i<=6; i++)
;;
;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
;; 6 
;; NIL

;;;==================================================================================
(defun comparefibonaccis()
  (for (i 10 35 5)                               ;; for (int i=10; i<=35, i+=5) ...
       (format t "TAIL REC FIBONACCI ~a~%" i)
       (time(fibonacci-TR i))
       (format t "FIBONACCI ~a~%" i)
       (time(fibonacci i))
       (format t "=======================================================~%")
       )
  )

;;;==================================================================================
;;; END 
