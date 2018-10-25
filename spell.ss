
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************
;; final final 2017-04-24 14:06:08
;;final ver 2017-04-24 02:49:32

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")
;;(load "dictionary.ss")
;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


 
(define (together tem1 tem2)
       (if (null? tem2) '()
                (cons (tem1 (car tem2)) (together tem1 (cdr tem2)))))


 ;; To compare two data tem1 and tem2 is same or not
 ;; if two data are same reture ture ,other false
        (define combine
          (lambda (tem1 tem2)
        (if (and tem1 tem2)ture
            false)))

;; 

(define generatesBitVal  ;;to check if hashlist and dic same return ture, if not teturn false
  (lambda (hashList dictionary)
    ;;combin two list into one list
    (define (comb tem1 tem2)
      (cond( (null? tem1) tem2)(else(cons (car tem1) (comb (cdr tem1) tem2)))))
    (cond((null? hashList)'())
         (else (comb (together (car hashList)dictionary) (generatesBitVal (cdr hashList)dictionary))
               )
         )
    )
  )


;; define  t == ture
(define ture
  #t)
;; define  f == false
(define false
  #f)




(define generatesCheck
  (lambda (list bitvector)
    ;;generates list either ture or false
    (define check
       (lambda (tem1 tem2 ) (cond ((null? tem1) false)  ;; tem1 is empy retuern fasle 
               ((null? tem2) false)  ;; tem2 is empy retuern fasle 
               ((= tem1 (car tem2)) ture)(else (check tem1 (cdr tem2))))))  ;;continue call this function
               
    (cond( (null? list)'())
    (else(cons (check (car list) bitvector ) (generatesCheck (cdr list) bitvector))
  )
    )
    )
  )
      

(define reduce
  (lambda (op l id)
    (if (null? l)
       id
       (op (car l) (reduce op (cdr l) id)
           )
    )
    )
  )



;;list function
(define (MakeList x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x)
              )
        )
  )


;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
     'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
      (cond( (null? w)
        5187)
       ( else(+ (reduce + (MakeList(ctv (car w))) 0) ( * 29 (key (cdr w))
                                                         )
                )
             )
    )
)
  )

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
        (lambda(tem)
       (modulo(key tem)size)
)
)
  )

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
       (lambda (tem)
       (floor (* size(-(* (key tem) A) (floor(* (key tem) A)))))
         )
    )
  )


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
     (lambda(word)
       (define generatesVal(lambda (list tem)(cond((null? list)'())(else(cons ((car list) tem) (generatesVal (cdr list) tem))))))
        (reduce combine (generatesCheck (generatesVal hashfunctionlist word) (generatesBitVal hashfunctionlist dict)) ture);;to check dictionary and hashlish data
    )
)
  )


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

;;  (checker-1 '())
;;  (checker-1 '())
;;  (checker-1 '())
;; (checker-1 '(b u b b l z))
 ;;(checker-2 '(b u b b l z))
 ;;(checker-3 '(b u b b l z))
;;(checker-1 '(a e r g k j g k y k i g k h k h k h))