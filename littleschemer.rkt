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
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset-mr
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset-mr (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

       
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

; Helper for function below
(define append-list
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) lat2)
      (else (cons (car lat1) (append-list (cdr lat1) lat2))))))

; Do the union using makeset (and append-list)
(define union-makeset
  (lambda (set1 set2)
    (makeset (append-list set1 set2))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Careful now: a rel is a list of pairs. a fun is a rel where the 1st members of each pairs is a set. i.e a fun is a list x -> y with each x being unique.
; Finally a fullfun is a fun where the 2nd members of each pairs is a set i.e a fullfun is a list x -> y with each x and y being unique
; So it moves from pair (x y) to rel ( (x y) (x z) (z d) ) to fun ( (x y) (e f) (d y) ) to fullfun ( ( x y) ( e f ) ( d z ) ) 
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))
  

(define revrel-rp
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))


(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

; Chapter 8
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad) )
; or use anonymous ((eq?-c 'salad) 'pepper)

; a carried rember-f
(define rember-fc
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-fc test?) a (cdr l))))))))

; Same as insertL just call the function (insertL-f test?)
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) old ) (cons new (cons old (cdr lat ))))
        (else (cons (car lat) ( (insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) old ) (cons old (cons new (cdr lat ))))
        (else (cons (car lat) ( (insertR-f test?) new old (cdr lat))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) old ) (seq new old (cdr lat)))
        (else (cons (car lat) ( (insert-g seq) new old (cdr lat))))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define seqrem
  (lambda (new old l) l))


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) o+)
      ((eq? x 'x) ox)
      (else o^))))

(define value-f
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value-f (1st-sub-exp nexp))
             (value-f (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a ) ( (multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ( (multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) ) ( multiremberT test? (cdr lat)))
      (else (cons (car lat) ( multiremberT test? (cdr lat)))))))

; multirember&co *is* diffucult.
; take a look at this: http://www.michaelharrison.ws/weblog/?p=34
; and this https://stackoverflow.com/questions/7004636/explain-the-continuation-example-on-p-137-of-the-little-schemer#7005024