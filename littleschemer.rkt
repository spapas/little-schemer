(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

; CHAPTER 2

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; CHAPTER 3 
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a ) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons old (cons new (cdr lat ))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons new (cons old (cdr lat ))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons new (cdr lat )))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a ) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))    


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons old (cons new (multiinsertR new old (cdr lat )))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons new (cons old (multiinsertL new old (cdr lat )))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old ) (cons new (multisubst new old (cdr lat ))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; CHAPTER 4
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o< n m) #f)
      ((o> n m) #f)
      (else #t))))

(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (o^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat) )
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((eq? n 1) (cdr lat))     
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat)) ))))

(define eqan?
  (lambda (a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))     
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; CHAPTER 5
(define rember*
  (lambda (a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
    (else
     (cons (rember* a (car l)) (rember* a (cdr l)))))))


(define insertR*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else
     (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else
     (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
    (else
     (cons (subst* new old (car l)) (subst* new old (cdr l)))))))


(define insertL*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
    (else
     (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else
     (or (member* a (car l)) (member a (cdr l)))))))

(define leftmost
  (lambda (l)
  (cond
    ((atom? (car l)) (car l))
    (else
     (leftmost (car l))))))

(define oequal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or  (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or  (null? l1) (null? l2)) #f)
      (else (and
             (oequal? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))))))

       
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

; CHAPTER 6
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+ ) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '* ) (ox (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else (o^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

; Same as value but use abstract functions (1st-sub-exp, 2nd-sub-exp and operator) to calculate the arguments
(define value-abstract
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+ ) (o+ (value-abstract (1st-sub-exp nexp)) (value-abstract (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '* ) (ox (value-abstract (1st-sub-exp nexp)) (value-abstract (2nd-sub-exp nexp))))
      (else (o^ (value-abstract (1st-sub-exp nexp)) (value-abstract (2nd-sub-exp nexp)))))))

; Notice that the functions below work for expressions of the form ((1 + 2) * (3 ^ 4)); with a little change
; we could change our form to (f.e) (* (+ 1 2) (^ 3 4) ). The value-abstract function would remanin the same
; only the functions below would need to change.

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))


; Fun with '( () () () ) = 3, '() = 0 etc number representation
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oo+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (oo+ n (zub1 m)))))))

; CHAPTER 7