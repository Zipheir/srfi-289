;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;;
;;;; SPDX-License-Identifier: MIT
(define-record-type <set>
  (%set hashmap)
  set?
  (hashmap %m set-m!))

(define (set comparator . els)
  (%set
   (hashmap-unfold null?
                   (lambda (x) (values (car x) (car x)))
                   cdr
                   els
                   comparator)))

(define (%set-unfold comparator stop? mapper successor seed)
  (unless (comparator? comparator)
    (error "Not a comparator" comparator))
  (%set
   (hashmap-unfold stop?
                   (lambda (x)
                     (let ((v (mapper x)))
                       (values v v)))
                   successor
                   seed
                   comparator)))

(define (set-unfold a1 a2 a3 a4 a5)
  (if (comparator? a1)
      (%set-unfold a1 a2 a3 a4 a5)
      (%set-unfold a5 a1 a2 a3 a4)))

(define (set-contains? set el)
  (hashmap-contains? (%m set) el))
(define (set-empty? set el)
  (hashmap-empty? (%m set)))
(define (set-disjoint? s1 s2)
  (hashmap-disjoint? (%m s1) (%m s2)))

(define (set-member set element default)
  (hashmap-ref/default (%m set)
                       element
                       default))
(define (set-element-comparator set)
  (hashmap-key-comparator (%m set)))

(define (set-adjoin! s . els)
  (let loop ((m (%m s))
             (els els))
    (if (null? els)
        (begin
          (set-m! s m)
          s)
        (loop (hashmap-adjoin! m (car els) (car els))
              (cdr els)))))

(define (set-adjoin s . els)
  (let loop ((m (%m s))
             (els els))
    (if (null? els)
        (%set m)
        (loop (hashmap-adjoin m (car els) (car els))
              (cdr els)))))

(define (set-replace s el)
  (%set (hashmap-replace (%m s) el el)))
(define (set-replace! s el)
  (set-m! s (hashmap-replace! (%m s) el el))
  s)

(define (set-delete-all s elements)
  (%set (hashmap-delete-all (%m s) elements)))

(define (set-delete-all! s elements)
  (set-m! s (hashmap-delete-all (%m s) elements))
  s)

(define (set-delete s . els)
  (set-delete-all s els))
(define (set-delete! s . els)
  (set-delete-all! s els))

(define (set-size s)
  (hashmap-size (%m s)))

(define (set-find predicate set failure)
  (hashmap-find (lambda (x y) (predicate x)) (%m set) failure))

(define (set-count predicate set failure)
  (hashmap-count (lambda (x y) (predicate x)) (%m set) failure))

(define (set-every? predicate set)
  (hashmap-every? (lambda (x y) (predicate x)) (%m set)))

(define (set-any? predicate set)
  (hashmap-any? (lambda (x y) (predicate x)) (%m set)))

(define (set-map comparator proc set)
  (%set
   (hashmap-map (lambda (key ignored)
                  (let ((new (proc key)))
                    (values new new)))
                comparator
                (%m set))))

(define (set-for-each proc set)
  (hashmap-for-each (lambda (x y) (proc x)) (%m set)))

(define (set-fold proc nil set)
  (hashmap-fold (lambda (k v nil) (proc k nil)) (%m set)))

(define (set-filter predicate set)
  (%set (hashmap-filter (lambda (x y) (predicate x)) (%m set))))
(define (set-filter! predicate set)
  (set-m! (hashmap-filter! (lambda (x y) (predicate x)) (%m set)) set)
  set)

(define (set-remove predicate set)
  (%set (hashmap-remove (lambda (x y) (predicate x)) (%m set))))
(define (set-remove! predicate set)
  (set-m! (hashmap-remove! (lambda (x y) (predicate x)) (%m set)) set)
  set)

(define (set-partition predicate set)
  (let-values (((m1 m2)
                (hashmap-partition (lambda (x y) (predicate x))
                                   (%m set))))
    (values (%set m1) (%set m2))))

(define (set-partition! predicate set)
  ;; SRFI 113 sample implementation mutates the argument and makes it
  ;; the first return value.
  (let-values (((m1 m2) (hashmap-partition!
                         (lambda (x y) (predicate x))
                         (%m set))))
    (set-m! set m1)
    (values set (%set m2))))

(define (set-copy set)
  (%set (hashmap-copy (%m set))))
(define (set->list set)
  (hashmap-keys (%m set)))
(define (list->set comparator lst)
  (%set-unfold comparator null? car cdr lst))
(define (list->set! set lst)
  (let loop ((m (%m set))
             (lst lst))
    (if (null? lst)
        (begin
          (set-m! set m)
          set)
        (loop (hashmap-adjoin! m (car lst) (car lst))
              (cdr lst)))))

(define anything-comparator
  (make-comparator (lambda (x) #t)
                   (lambda (x y) #t)
                   #f
                   #f))

(define (variadic-comparison C get hashmap~?)
  (lambda (s1 . rest)
    (let loop ((s1 s1) (rest rest))
      (if (null? rest)
          #t
          (let ((s2 (car rest)))
            (and (hashmap~? C (get s1) (get s2))
                 (loop s2 (cdr rest))))))))

(define set=? (variadic-comparison
                anything-comparator
                %m
                hashmap=?))
(define set<? (variadic-comparison
                anything-comparator
                %m
                hashmap<?))
(define set>? (variadic-comparison
                anything-comparator
                %m
                hashmap>?))
(define set>=? (variadic-comparison
                anything-comparator
                %m
                hashmap>=?))
(define set<=? (variadic-comparison
                anything-comparator
                %m
                hashmap<=?))

(define (variadic-set-operation op)
  (lambda (s1 . rest)
    (let loop ((acc (%m s1)) (rest rest))
      (if (null? rest)
          (%set acc)
          (let ((s (%m (car rest))))
            (loop (op acc s) (cdr rest)))))))

(define set-union (variadic-set-operation hashmap-union))
(define set-intersection (variadic-set-operation hashmap-intersection))
(define set-difference (variadic-set-operation hashmap-difference))

(define (set-xor s1 s2)
  (%m (hashmap-xor (%m s1) (%m s2))))
(define (set-xor! s1 s2)
  (set-m! s1 (hashmap-xor! (%m s1) (%m s2)))
  s1)

(define (linear-variadic-set-operation op!)
  (lambda (s1 . rest)
    (let loop ((acc (%m s1)) (rest rest))
      (if (null? rest)
          (begin
            (set-m! s1 acc)
            s1)
          (let ((s (%m (car rest))))
            (loop (op! acc s) (cdr rest)))))))

(define set-union! (linear-variadic-set-operation hashmap-union!))
(define set-intersection! (linear-variadic-set-operation hashmap-intersection!))
(define set-difference! (linear-variadic-set-operation hashmap-difference!))

(define set-comparator
  (make-comparator set?
                   set=?
                   #f
                   (lambda (x)
                     (let ((hash (comparator-hash-function
                                  (set-element-comparator x))))
                       (set-fold (lambda (el previous-hash)
                                   (+ (bitwise-xor (hash el) previous-hash) 1))
                                 0
                                 x)))))


;;;;;;;;;;;;;;;;;;;;
;;; Bags
;;;;;;;;;;;;;;;;;;;;

(define-record-type <bag>
  (%bag hashmap total)
  bag?
  (hashmap bag-m bag-set-m!)
  (total bag-size bag-set-size!))

(define (bag-unique-size bag)
  (hashmap-size (bag-m bag)))

(define (bag-element-count bag element)
  (hashmap-ref/default (bag-m bag) element 0))

(define (bag-for-each-unique proc bag)
  (hashmap-for-each proc (bag-m bag)))

(define (bag-fold proc nil bag)
  (hashmap-fold proc nil (bag-m bag)))

(define (%bag-unfold comparator stop? mapper successor seed)
  (unless (comparator? comparator)
    (error "Not a comparator" comparator))
  (let loop ((m (hashmap comparator))
             (count 0)
             (seed seed))
    (if (stop? seed)
        (%bag m count)
        (let* ((value (mapper seed))
               (m (hashmap-update/default!
                   m
                   value
                   (lambda (x) (+ x 1))
                   0)))
          (loop m (+ count 1) (successor seed))))))

(define (bag-unfold a1 a2 a3 a4 a5)
  (if (comparator? a1)
      (%bag-unfold a1 a2 a3 a4 a5)
      (%bag-unfold a5 a1 a2 a3 a4)))

(define (bag comparator . els)
  (%bag-unfold comparator null? car cdr els))

(define (bag-contains? bag el)
  (hashmap-contains? (bag-m bag) el))
(define (bag-empty? bag)
  (zero? (bag-size bag)))

(define (bag-disjoint? bag1 bag2)
  (let ((m1 (bag-m bag1))
        (m2 (bag-m bag2)))
    (hashmap-every? (lambda (element number)
                      (hashmap-ref m2 element
                                   (lambda () #f)
                                   (lambda (number2)
                                     (= number number2))))
                    m1)))

(define (bag-member bag element default)
  (call-with-current-continuation
   (lambda (return)
    (mapping-search (bag-m bag)
                    element
                    (lambda (insert ignore)
                      (return default))
                    (lambda (key count update remove)
                      (return key))))))

(define (bag-element-comparator set)
  (hashmap-key-comparator (bag-m set)))

(define (bag-adjoin! bag . els)
  (let loop ((m (bag-m bag))
             (count (bag-size bag))
             (els els))
    (if (null? els)
        (begin
          (bag-set-m! bag m)
          (bag-set-size! bag count)
          bag)
        (let* ((el (car els))
               (m (hashmap-update/default!
                   m
                   el
                   (lambda (x) (+ x 1))
                   0)))
          (loop m (+ count 1) (cdr els))))))

(define (bag-adjoin bag . els)
  (let loop ((m (bag-m bag))
             (count (bag-size bag))
             (els els))
    (if (null? els)
        (%bag m count)
        (let* ((el (car els))
               (m (hashmap-update/default
                   m
                   el
                   (lambda (x) (+ x 1))
                   0)))
          (loop m (+ count 1) (cdr els))))))

(define (bag-replace bag el)
  (%bag
   (bag-size bag)
   (hashmap-replace (bag-m bag)
                    el
                    (hashmap-ref/default
                     (bag-m bag)
                     el
                     0))))

(define (bag-replace! bag el)
  (let ((m (hashmap-replace! (bag-m bag)
                             el
                             (hashmap-ref/default (bag-m bag)
                                                  el
                                                  0))))
    (bag-set-m! bag m)
    bag))

(define (bag-delete-all! bag els)
  (let loop ((m (bag-m bag))
             (count count)
             (els els))
    (if (null? els)
        (begin
          (bag-set-m! bag m)
          (bag-set-size! bag count)
          bag)
        (hashmap-ref m
                     (car els)
                     (lambda ()
                       (loop m count (cdr els)))
                     (lambda (count)
                       (if (= count 1)
                           (loop (mapping-delete! m (car els))
                                 (- count 1)
                                 (cdr els))
                           (loop (mapping-update! m (car els)
                                                  (lambda (x) (- x 1)))
                                 (- count 1)
                                 (cdr els))))))))

(define (bag-delete-all bag els)
  (let loop ((m (bag-m bag))
             (count count)
             (els els))
    (if (null? els)
        (%bag m count)
        (hashmap-ref m (car els)
                     (lambda ()
                       (loop m count (cdr els)))
                     (lambda (count)
                       (if (= count 1)
                           (loop (mapping-delete m (car els))
                                 (- count 1)
                                 (cdr els))
                           (loop (mapping-update m (car els)
                                                 (lambda (x) (- x 1)))
                                 (- count 1)
                                 (cdr els))))))))

(define (bag-delete s . els)
  (bag-delete-all s els))
(define (bag-delete! s . els)
  (bag-delete-all! s els))

(define (bag-find predicate set failure)
  (hashmap-find (lambda (x y) (predicate x)) (bag-m set) failure))

(define (bag-count predicate set failure)
  (hashmap-count (lambda (x y) (predicate x)) (bag-m set) failure))

(define (bag-every? predicate bag)
  (hashmap-every? (lambda (x y) (predicate x)) (bag-m bag)))

(define (bag-any? predicate bag)
  (hashmap-any? (lambda (x y) (predicate x)) (bag-m bag)))

(define (bag-increment bag element count)
  (unless (and (integer? count) (not (negative? count)))
    (error "must be a non-negative integer" count))
  (let ((m (bag-m bag)))
    (%bag (hashmap-update/default m
                                  element
                                  (lambda (old) (+ old count))
                                  count)
          (+ (bag-size bag) count))))

(define (bag-increment! bag element count)
  (unless (and (integer? count) (not (negative? count)))
    (error "must be a non-negative integer" count))
  (let* ((m (bag-m bag)))
         (m (hashmap-update/default! m
                                     element
                                     (lambda (old)
                                       (+ old count))
                                     count)))
    (bag-set-m! bag m)
    (bag-set-count! bag m (+ count (bag-size bag)))
    bag)

(define (bag-decrement bag element count)
  (unless (and (integer? count) (not (negative? count)))
    (error "must be a non-negative integer" count))
  (let* ((m (bag-m bag))
         (old (hashmap-ref/default m element 0))
         (new (min (- old count) 0)))
    (if (zero? new)
        (%bag (hashmap-delete m element)
              (- (bag-size bag) old))
        (%bag (hashmap-set m element new)
              (- (bag-size bag) count)))))

(define (bag-decrement! bag element count)
  (unless (and (integer? count) (not (negative? count)))
    (error "must be a non-negative integer" count))
  (let* ((m (bag-m bag))
         (old (hashmap-ref/default m element 0))
         (new (min (- old count) 0)))
    (if (zero? new)
        (let ((m (hashmap-delete! m element)))
          (bag-set-size! bag (- (bag-size bag) old))
          (bag-set-m! bag m)
          bag)
        (let ((m (hashmap-set! m element)))
          (bag-set-size! bag (- (bag-size bag) count))
          (bag-set-m! bag m)
          bag))))

(define (bag-product n the-bag)
  (unless (and (integer? n) (positive? n))
    (error "not a non-negative integer" n))
  (bag-fold-unique (lambda (el count nil)
                     (bag-increment nil el (* count (- n 1))))
                   (bag (bag-element-comparator bag))
                   the-bag))

(define (bag-product! n the-bag)
  (let ((new-bag (bag-product n the-bag)))
    (bag-set-m! the-bag (bag-m the-bag))
    (bag-set-size! the-bag (bag-size the-bag))
    the-bag))

(define (bag-map comparator proc bag)
  (bag-fold-unique (lambda (el count bag)
                     (bag-increment el count bag))
                   (bag comparator)
                   bag))

(define (bag-for-each proc bag)
  (bag-for-each-unique (lambda (key count)
                         (do ((i 0 (+ i 1)))
                             ((= i key))
                           (proc key)))
                       bag))

(define (bag-fold proc nil bag)
  (bag-fold-unique (lambda (el count nil)
                     (do ((i 0 (+ i 1))
                          (nil nil (proc el nil)))
                         ((= i count))))
                   bag))

(define (bag-filter predicate the-bag)
  (bag-fold-unique (lambda (key count bag)
                     (if (predicate key)
                         (bag-increment bag key count)
                         bag))
                   (bag (bag-element-comparator bag))
                   the-bag))

(define (bag-filter! predicate bag)
  (define (kons key count nil)
    (if (predicate key)
        (let ((m (car nil))
              (total (cdr nil)))
          (cons (hashmap-set! m key count)
                (+ total count)))
        nil))
  (define nil (bag-fold-unique kons
                               (cons (hashmap (bag-element-comparator bag))
                                     0)
                               bag))
  (bag-set-m! bag (car nil))
  (bag-set-size! bag (cdr nil))
  bag)

(define (bag-remove predicate bag)
  (bag-filter (lambda (x) (not (predicate x))) bag))
(define (bag-remove! predicate bag)
  (bag-filter! (lambda (x) (not (predicate x))) bag))

(define (bag-partition predicate bag)
  (define (kons el count knil)
    (let ((b1 (car knil))
          (b2 (cdr knil)))
      (if (predicate el)
          (vector (bag-adjoin m1 el count) m2)
          (vector (m1 (bag-adjoin m2 el count))))))
  (define knil
    (bag-fold-unique kons
                     (vector (bag (bag-element-comparator bag))
                             (bag (bag-element-comparator bag)))
                     bag))
  (values (vector-ref knil 0)
          (vector-ref knil 1)))

(define (bag-partition! predicate bag)
  (let-values (((b1 b2) (bag-partition predicate bag)))
    (bag-set-m! bag (bag-m bag))
    (bag-set-size! bag (bag-size bag))
    (values bag b2)))

(define (bag-copy bag)
  (%bag (hashmap-copy (bag-m bag)) (bag-size bag)))

(define (bag->list bag)
  (bag-fold cons '() bag))
(define (list->bag comparator lst)
  (%bag-unfold comparator null? car cdr lst))
(define (list->bag! bag lst)
  (if (null? lst)
      bag
      (list->bag! (bag-adjoin! bag (car lst)) (cdr lst))))

(define exact-integer-comparator
  (make-comparator exact-integer? = #f #f))

(define bag=? (variadic-comparison
                exact-integer-comparator
                bag-m
                hashmap=?))
(define bag<? (variadic-comparison
                exact-integer-comparator
                bag-m
                hashmap<?))
(define bag>? (variadic-comparison
                exact-integer-comparator
                bag-m
                hashmap>?))
(define bag>=? (variadic-comparison
                exact-integer-comparator
                bag-m
                hashmap>=?))
(define bag<=? (variadic-comparison
                exact-integer-comparator
                bag-m
                hashmap<=?))

(define (variadic-bag-operation bag-op)
  (lambda (bag1 . rest)
    (let loop ((bag1 bag1) (rest rest))
      (if (null? rest)
          bag1
          (loop (op bag1 (car rest)) (cdr rest))))))

(define (bag-sum* bag1 bag2)
  (bag-fold-unique (lambda (el count next-bag)
                     (bag-increment el count next-bag))
                   bag2
                   bag1))
(define bag-sum (variadic-bag-operation bag-sum*))

(define (bag-sum*! bag1 bag2)
  (bag-fold-unique (lambda (el count next-bag)
                     (bag-increment! el count next-bag))
                   bag2
                   bag1))
(define bag-sum! (variadic-bag-operation bag-sum*!))

(define (hashmap-sum hashmap)
  (hashmap-fold (lambda (ignored count total) (+ count total))
                hashmap))

(define (bag-union* bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (hashmap-set hm el
                                    (max count (hashmap-ref/default hm el 0))))
                     (bag-m bag1)
                     bag2))
  (%bag new-hashmap (hashmap-sum new-hashmap)))
(define bag-union (variadic-bag-operation bag-union*))

(define (bag-union*! bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (hashmap-set! hm el
                                     (max count (hashmap-ref/default hm el 0))))
                     (bag-m bag1)
                     bag2))
  (bag-set-m! bag1 new-hashmap)
  (bag-set-total! bag1 (hashmap-sum new-hashmap))
  bag1)
(define bag-union! (variadic-bag-operation bag-union*!))

(define (bag-intersection* bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (hashmap-set hm el
                                    (min count (hashmap-ref/default hm el 0))))
                     (bag-m bag1)
                     bag2))
  (%bag new-hashmap (hashmap-sum new-hashmap)))
(define bag-intersection (variadic-bag-operation bag-intersection*))

(define (bag-intersection*! bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (hashmap-set! hm el
                                     (min count (hashmap-ref/default hm el 0))))
                     (bag-m bag1)
                     bag2))
  (bag-set-m! bag1 new-hashmap)
  (bag-set-total! bag1 (hashmap-sum new-hashmap))
  bag1)
(define bag-intersection! (variadic-bag-operation bag-intersection*!))

(define (bag-difference* bag1 bag2)
  (define (kons el second-count first-m)
    (let* ((current (hashmap-ref/default first-m el 0))
           (new (- current second-count)))
      (if (positive? new)
          (hashmap-set first-m el new)
          (hashmap-delete first-m el))))
  (define new-hashmap (bag-fold-unique kons (bag-m bag1) bag2))
  (%bag new-hashmap (hashmap-sum new-hashmap)))
(define bag-difference (variadic-bag-operation bag-difference*))

(define (bag-difference*! bag1 bag2)
  (define (kons el second-count first-m)
    (let* ((current (hashmap-ref/default first-m el 0))
           (new (- current second-count)))
      (if (positive? new)
          (hashmap-set! first-m el new)
          (hashmap-delete! first-m el))))
  (define new-hashmap (bag-fold-unique kons (bag-m bag1) bag2))
  (bag-set-m! bag1 new-hashmap)
  (bag-set-total! bag1 (hashmap-sum new-hashmap))
  bag1)
(define bag-difference! (variadic-bag-operation bag-difference*!))

(define (bag-xor bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (let* ((previous (hashmap-ref/default hm el 0))
                              (current (abs (- count previous))))
                         (if (zero? current)
                             (hashmap-delete hm el)
                             (hashmap-set hm el current))))
                 (bag-m bag2)
                 bag1))
  (%bag new-hashmap (hashmap-sum new-hashmap)))

(define (bag-xor! bag1 bag2)
  (define new-hashmap
    (bag-fold-unique (lambda (el count hm)
                       (let* ((previous (hashmap-ref/default hm el 0))
                              (current (abs (- count previous))))
                         (if (zero? current)
                             (hashmap-delete! hm el)
                             (hashmap-set! hm el current))))
                 (bag-m bag2)
                 bag1))
  (bag-set-m! bag1 new-hashmap)
  (bag-set-total! bag1 (hashmap-sum new-hashmap))
  bag1)

(define bag-comparator
  (make-comparator bag?
                   bag=?
                   #f
                   (lambda (x)
                     (let ((hash (comparator-hash-function
                                  (bag-element-comparator x))))
                       (bag-fold-unique
                        (lambda (el count nil)
                          (do ((i 0 (+ i 1))
                               (hash nil (+ (bitwise-xor el hash) 1)))
                              ((= i count) nil)))
                        0
                        x)))))

(define (bag->set bag)
  (%set (hashmap-map (bag-element-comparator bag)
                     (lambda (el ignored)
                       (values el el))
                     (bag-m bag))))

(define (set->bag set)
  (%bag (hashmap-map (bag-element-comparator bag)
                     (lambda (el ignored)
                       (values el 1))
                     (%m bag))
        (set-size set)))

(define (set->bag! bag set)
  (set-fold (lambda (el bag)
              (bag-adjoin! bag el))
            bag
            set))

(define (bag->alist bag)
  (hashmap->alist (bag-m bag)))

(define (alist->bag comparator alist)
  (let loop ((bag (bag comparator))
             (alist alist))
    (if (null? alist)
        bag
        (let ((pair (car alist)))
          (loop (bag-increment bag (car pair) (cdr pair))
                (cdr alist))))))
