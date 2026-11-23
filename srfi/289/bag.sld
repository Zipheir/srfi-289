;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT

(define-library (srfi 289 bag)
  (include-library-declarations "bag-exports.scm")
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) car+cdr fold)
          (srfi 128)
          (srfi 146 hash)
          (only (srfi 151) bitwise-xor)
          (srfi 289 common)
          (only (srfi 289 set) set-element-comparator
                               set-fold)
          )
  (begin
   ;;; A bag is represented by a hashmap which associates each element
   ;;; with its count.  The count is always positive.  Whenever an
   ;;; element's count would be reduced to zero, its association is
   ;;; removed from the hashmap.

   (define-record-type <bag>
     (%bag hashmap)
     bag?
     (hashmap bag-m bag-set-m!))

   (define (bag-unique-size bag)
     (hashmap-size (bag-m bag)))

   (define (bag-element-count bag element)
     (hashmap-ref/default (bag-m bag) element 0))

   (define (bag-for-each-unique proc bag)
     (hashmap-for-each proc (bag-m bag)))

   (define (bag-fold-unique proc nil bag)
     (hashmap-fold proc nil (bag-m bag)))

   (define (bag-unfold stop? mapper successor seed comparator)
     (unless (comparator? comparator)
       (error "Not a comparator" comparator))
     (let loop ((m (hashmap comparator)) (seed seed))
       (if (stop? seed)
           (%bag m)
           (let* ((value (mapper seed))
                  (m (hashmap-update/default
                      m
                      value
                      (lambda (x) (+ x 1))
                      0)))
             (loop m (successor seed))))))

   (define (bag comparator . els)
     (bag-unfold null? car cdr els comparator))

   (define (bag-contains? bag el)
     (hashmap-contains? (bag-m bag) el))

   (define (bag-empty? bag)
     (hashmap-empty? (bag-m bag)))

   (define (bag-disjoint? bag1 bag2)
     (hashmap-disjoint? (bag-m bag1) (bag-m bag2)))

   (define (bag-element-comparator bag)
     (hashmap-key-comparator (bag-m bag)))

   (define (bag-adjoin bag . els)
     (bag-adjoin-all bag els))

   (define (bag-adjoin! bag . els)
     (bag-adjoin-all! bag els))

   (define (bag-adjoin-all bag els)
     (%bag
      (fold (lambda (el hmap)
              (hashmap-update/default hmap
                                      el
                                      (lambda (x) (+ x 1))
                                      0))
            (bag-m bag)
            els)))

   (define (bag-adjoin-all! bag els)
     (bag-set-m!
      bag
      (fold (lambda (el hmap)
              (hashmap-update!/default hmap
                                       el
                                       (lambda (x) (+ x 1))
                                       0))
            (bag-m bag)
            els))
     bag)

   (define (bag-replace bag el)
     (%bag
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
     (define (delete1 el hmap)
       (hashmap-ref
        hmap
        el
        (lambda () hmap)
        (lambda (count)
          (if (= count 1)
              (hashmap-delete! hmap el)
              (hashmap-update! hmap el (lambda (c) (- c 1)))))))
     (let ((hmap (fold delete1 (bag-m bag) els)))
       (bag-set-m! bag hmap)
       bag))

   (define (bag-delete-all bag els)
     (define (delete1 el hmap)
       (hashmap-ref
        hmap
        el
        (lambda () hmap)
        (lambda (count)
          (if (= count 1)
              (hashmap-delete hmap el)
              (hashmap-update hmap el (lambda (c) (- c 1)))))))
     (%bag (fold delete1 (bag-m bag) els)))

   (define (bag-delete s . els)
     (bag-delete-all s els))

   (define (bag-delete! s . els)
     (bag-delete-all! s els))

   ;; (Ab)use bag-for-each to get a single arbitrary element.
   (define (bag-pick bag)
     (call-with-current-continuation
      (lambda (k) (bag-for-each k bag))))

   (define (bag-pop bag)
     (when (bag-empty? bag)
       (error "can't pop empty bag" bag))
     (let ((el (bag-pick bag)))
       (values (bag-delete bag el) el)))

   (define (bag-pop! bag)
     (when (bag-empty? bag)
       (error "can't pop empty bag" bag))
     (let ((el (bag-pick bag)))
       (values (bag-delete! bag el) el)))

   (define (bag-size bag)
     (hashmap-fold (lambda (_el c sum) (+ c sum))
                   0
                   (bag-m bag)))

   (define (bag-find predicate bag failure)
     (call-with-current-continuation
      (lambda (return)
        (bag-for-each (lambda (elt)
                        (when (predicate elt)
                          (return elt)))
                      bag)
        (failure))))

   (define (bag-count predicate bag)
     (hashmap-count (lambda (x y) (predicate x)) (bag-m bag)))

   (define (bag-every? predicate bag)
     (hashmap-every? (lambda (x y) (predicate x)) (bag-m bag)))

   (define (bag-any? predicate bag)
     (hashmap-any? (lambda (x y) (predicate x)) (bag-m bag)))

   (define (bag-increment bag element count)
     (unless (and (integer? count) (not (negative? count)))
       (error "must be a non-negative integer" count))
     (if (zero? count)
         bag
         (%bag (hashmap-update/default (bag-m bag)
                                       element
                                       (lambda (old) (+ old count))
                                       0))))

   (define (bag-increment! bag element count)
     (unless (and (integer? count) (not (negative? count)))
       (error "must be a non-negative integer" count))
     (if (zero? count)
         bag
         (let ((m (hashmap-update!/default (bag-m bag)
                                           element
                                           (lambda (old) (+ old count))
                                           0)))
           (bag-set-m! bag m)
           bag)))

   (define (bag-decrement bag element count)
     (unless (and (exact? count)
                  (integer? count)
                  (not (negative? count)))
       (error "must be a non-negative integer" count))
     (if (zero? count)
         bag
         (let* ((m (bag-m bag))
                (old (hashmap-ref/default m element 0))
                (new (max (- old count) 0)))
           (%bag (if (zero? new)
                     (hashmap-delete m element)
                     (hashmap-set m element new))))))

   (define (bag-decrement! bag element count)
     (unless (and (exact? count)
                  (integer? count)
                  (not (negative? count)))
       (error "must be a non-negative integer" count))
     (if (zero? count)
         bag
         (let* ((m (bag-m bag))
                (old (hashmap-ref/default m element 0))
                (new (max (- old count) 0)))
           (bag-set-m! bag
                       (if (zero? new)
                           (hashmap-delete! m element)
                           (hashmap-set! m element new)))
           bag)))

   (define (bag-product n the-bag)
     (unless (and (exact? n) (integer? n) (>= n 0))
       (error "invalid argument" n))
     (case n
       ((0) (%bag (hashmap (bag-element-comparator the-bag))))
       ((1) the-bag)
       (else
        (%bag
         (bag-fold-unique
          (lambda (el count hmap)
            (hashmap-adjoin hmap el (* count n)))
          (hashmap (bag-element-comparator the-bag))
          the-bag)))))

   (define (bag-product! n the-bag)
     (let ((new-bag (bag-product n the-bag)))
       (bag-set-m! the-bag (bag-m new-bag))
       the-bag))

   (define (bag-map proc comparator the-bag)
     (bag-fold (lambda (el bag) (bag-adjoin bag (proc el)))
               (bag comparator)
               the-bag))

   (define (bag-for-each proc bag)
     (bag-for-each-unique (lambda (key count)
                            (do ((i 0 (+ i 1)))
                                ((= i count))
                              (proc key)))
                          bag))

   (define (bag-fold proc nil bag)
     (bag-fold-unique (lambda (el count seed)
                        (do ((i 0 (+ i 1))
                             (seed seed (proc el seed)))
                            ((= i count) seed)))
                      nil
                      bag))

   (define (bag-filter predicate the-bag)
     (bag-fold-unique (lambda (key count bag)
                        (if (predicate key)
                            (bag-increment bag key count)
                            bag))
                      (bag (bag-element-comparator the-bag))
                      the-bag))

   (define (bag-filter! predicate bag)
     (define (filter! key count hmap)
       (if (predicate key)
           (hashmap-set! hmap key count)
           hmap))
     (bag-set-m! bag
                 (bag-fold-unique filter!
                                  (hashmap (bag-element-comparator bag))
                                  bag))
     bag)

   (define (bag-remove predicate bag)
     (bag-filter (lambda (x) (not (predicate x))) bag))

   (define (bag-remove! predicate bag)
     (bag-filter! (lambda (x) (not (predicate x))) bag))

   (define (bag-partition predicate the-bag)
     (define (split el count bags)
       (let ((b1 (car bags))
             (b2 (cdr bags)))
         (if (predicate el)
             (cons (bag-increment b1 el count) b2)
             (cons b1 (bag-increment b2 el count)))))
     (car+cdr
      (bag-fold-unique split
                       (cons (bag (bag-element-comparator the-bag))
                             (bag (bag-element-comparator the-bag)))
                       the-bag)))

   (define (bag-partition! predicate bag)
     (let-values (((b1 b2) (bag-partition predicate bag)))
       (bag-set-m! bag (bag-m b1))
       (values bag b2)))

   (define (bag-copy bag)
     (%bag (hashmap-copy (bag-m bag))))

   (define (bag->list bag)
     (bag-fold cons '() bag))

   (define (list->bag comparator lst)
     (bag-unfold null? car cdr lst comparator))

   (define exact-integer-comparator
     (make-comparator exact-integer? = #f #f))

   (define bag=? (variadic-comparison
                   exact-integer-comparator
                   bag-m
                   hashmap=?))

   (define (compare-bags pred bags)
     (cond ((or (null? bags) (null? (cdr bags))))
           (else
            (and (pred (car bags) (cadr bags))
                 (compare-bags pred (cdr bags))))))

   ;; Not very efficient, but then, bag<? doesn't seem like it
   ;; will see much use.
   (define (bag<?-2 bag1 bag2)
     (and (bag<=?-2 bag1 bag2)
          (< (bag-size bag1) (bag-size bag2))))

   (define bag<?
     (case-lambda
       ((bag1) #t)
       (bags
        (unless (pair? bags)
          (error "invalid number of arguments"))
        (compare-bags bag<?-2 bags))))

   (define (bag<=?-2 bag1 bag2)
     (let ((hmap2 (bag-m bag2)))
       (hashmap-every? (lambda (e c1)
                         (hashmap-ref hmap2
                                      e
                                      (lambda () #f)
                                      (lambda (c2)
                                        (<= c1 c2))))
                       (bag-m bag1))))

   (define bag<=?
     (case-lambda
       ((bag1) #t)
       (bags
        (unless (pair? bags)
          (error "invalid number of arguments"))
        (compare-bags bag<=?-2 bags))))

   (define (bag>=?-2 bag1 bag2)
     (bag<=?-2 bag2 bag1))

   (define bag>=?
     (case-lambda
       ((bag1) #t)
       (bags
        (unless (pair? bags)
          (error "invalid number of arguments"))
        (compare-bags bag>=?-2 bags))))

   ;; Not very efficient, but then, bag>? doesn't seem like it
   ;; will see much use.
   (define (bag>?-2 bag1 bag2)
     (and (bag>=?-2 bag1 bag2)
          (> (bag-size bag1) (bag-size bag2))))

   (define bag>?
     (case-lambda
       ((bag1) #t)
       (bags
        (unless (pair? bags)
          (error "invalid number of arguments"))
        (compare-bags bag>?-2 bags))))

   (define (variadic-bag-operation op)
     (lambda (bag1 . rest)
       (let loop ((bag1 bag1) (rest rest))
         (if (null? rest)
             bag1
             (loop (op bag1 (car rest)) (cdr rest))))))

   (define (bag-sum2 bag1 bag2)
     (bag-fold-unique (lambda (el count next-bag)
                        (bag-increment next-bag el count))
                      bag2
                      bag1))

   (define bag-sum (variadic-bag-operation bag-sum2))

   (define (bag-sum2! bag1 bag2)
     (bag-fold-unique (lambda (el count next-bag)
                        (bag-increment! next-bag el count))
                      bag2
                      bag1))

   (define bag-sum! (variadic-bag-operation bag-sum2!))

   (define (hashmap-sum hashmap)
     (hashmap-fold (lambda (ignored count total) (+ count total))
                   hashmap))

   (define (bag-union2 bag1 bag2)
     (%bag
      (bag-fold-unique
       (lambda (el count hm)
         (hashmap-set hm
                      el
                      (max count (hashmap-ref/default hm el 0))))
       (bag-m bag1)
       bag2)))

   (define bag-union (variadic-bag-operation bag-union2))

   (define (bag-union2! bag1 bag2)
     (bag-set-m!
      bag1
      (bag-fold-unique
       (lambda (el count hm)
         (hashmap-set! hm
                       el
                       (max count (hashmap-ref/default hm el 0))))
       (bag-m bag1)
       bag2))
     bag1)

   (define bag-union! (variadic-bag-operation bag-union2!))

   (define (bag-intersection2 bag1 bag2)
     (let ((hmap2 (bag-m bag2)))
       (%bag
        (bag-fold-unique
         (lambda (el c1 hm)
           (hashmap-ref hmap2
                        el
                        (lambda () hm)
                        (lambda (c2)
                          (hashmap-adjoin hm el (min c1 c2)))))
         (hashmap (bag-element-comparator bag1))
         bag1))))

   (define bag-intersection (variadic-bag-operation bag-intersection2))

   (define (bag-intersection2! bag1 bag2)
     (let ((hmap2 (bag-m bag2)))
       (bag-set-m!
        bag1
        (bag-fold-unique
         (lambda (el c1 hm)
           (hashmap-ref hmap2
                        el
                        (lambda () hm)
                        (lambda (c2)
                          (hashmap-adjoin! hm el (min c1 c2)))))
         (hashmap (bag-element-comparator bag1))
         bag1)))
     bag1)

   (define bag-intersection! (variadic-bag-operation bag-intersection2!))

   (define (bag-difference2 bag1 bag2)
     (define (kons el second-count first-m)
       (let* ((current (hashmap-ref/default first-m el 0))
              (new (- current second-count)))
         (if (positive? new)
             (hashmap-set first-m el new)
             (hashmap-delete first-m el))))
     (%bag (bag-fold-unique kons (bag-m bag1) bag2)))

   (define bag-difference (variadic-bag-operation bag-difference2))

   (define (bag-difference2! bag1 bag2)
     (define (kons el second-count first-m)
       (let* ((current (hashmap-ref/default first-m el 0))
              (new (- current second-count)))
         (if (positive? new)
             (hashmap-set! first-m el new)
             (hashmap-delete! first-m el))))
     (bag-set-m! bag1 (bag-fold-unique kons (bag-m bag1) bag2))
     bag1)

   (define bag-difference! (variadic-bag-operation bag-difference2!))

   (define (bag-xor bag1 bag2)
     (%bag
      (bag-fold-unique
       (lambda (el count hm)
         (let* ((previous (hashmap-ref/default hm el 0))
                (current (abs (- count previous))))
           (if (zero? current)
               (hashmap-delete hm el)
               (hashmap-set hm el current))))
       (bag-m bag2)
       bag1)))

   (define (bag-xor! bag1 bag2)
     (bag-set-m!
      bag1
      (bag-fold-unique
       (lambda (el count hm)
         (let* ((previous (hashmap-ref/default hm el 0))
                (current (abs (- count previous))))
           (if (zero? current)
               (hashmap-delete! hm el)
               (hashmap-set! hm el current))))
       (bag-m bag2)
       bag1))
     bag1)

   (define (make-bag-comparator elt-comp)
     (make-comparator (lambda (x)
                        (and (bag? x)
                             (eqv? (bag-element-comparator x)
                                   elt-comp)))
                      bag=?
                      #f
                      (and (comparator-hashable? elt-comp)
                      (lambda (x)
                        (let ((hash (comparator-hash-function
                                     (bag-element-comparator x))))
                          (bag-fold-unique
                           (lambda (el count nil)
                             (do ((i 0 (+ i 1))
                                  (hash nil (+ (bitwise-xor el hash) 1)))
                                 ((= i count) hash)))
                           0
                           x))))))

   (define (bag->set bag)
     (%set (hashmap-map (lambda (el ignored)
                          (values el el))
                        (bag-element-comparator bag)
                        (bag-m bag))))

   (define (set->bag set)
     (%bag (hashmap-map (lambda (el ignored)
                          (values el 1))
                        (set-element-comparator set)
                        (%m set))))

   (define (bag->alist bag)
     (hashmap->alist (bag-m bag)))

   (define (alist->bag comparator alist)
     (%bag
      (fold (lambda (p hmap)
              (let ((c (cdr p)))
                (unless (and (integer? c)
                             (exact? c)
                             (positive? c))
                  (error "invalid element count" c))
                  (hashmap-set hmap (car p) c)))
            (hashmap comparator)
            alist)))

   ))
