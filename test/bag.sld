;;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT
(define-library (test bag)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (prefix (srfi 1) s1:)
	  (prefix (srfi 128) s128:)
	  (prefix (srfi 132) s132:)
	  (srfi 289 bag)
	  (srfi 289 set)
	  )
  (begin
   (define intc (s128:make-comparator integer? = #f s128:number-hash))
   (define strc (s128:make-comparator string?
                                      string=?
                                      #f
                                      s128:string-hash))
   (define strc (s128:make-comparator string?
                                      string=?
                                      #f
                                      s128:string-hash))

   ;;; "Two bags, please."
   ;;; For a generous fee,
   ;;; Make your world
   ;;; What you want it to be.
   ;;;
   ;;;  --Curtis Mayfield, "Pusherman"

   (define (run-tests)
     (test-begin "Bags")

     (test-group "Constructors"
       (test-group "bag"
         (test-assert "empty bag"
           (bag-empty? (bag intc)))

         (test-assert "nonempty bag"
           (s1:lset= equal?
                     '((1 . 1) (2 . 1) (3 . 1))
                     (bag->alist (bag intc 1 2 3))))

         (test-assert "duplicate elements"
           (s1:lset= equal?
                     '((1 . 1) (2 . 2) (3 . 3))
                     (bag->alist (bag intc 1 2 2 3 3 3)))))

       (test-group "bag-unfold"
         (test-assert "empty bag"
           (bag-empty? (bag-unfold (lambda (i) #t)
                                   values
                                   (lambda (i) (+ i 1))
                                   0
                                   intc)))
         (test-assert "nonempty bag"
           (bag=? (bag intc 0 1 2 3 4)
                  (bag-unfold (lambda (i) (> i 4))
                              values
                              (lambda (i) (+ i 1))
                              0
                              intc)))
         (test-assert "duplicate elements"
           (bag=? (bag intc 0 1 2 2 1)
                  (bag-unfold (lambda (i) (> i 2))
                              abs
                              (lambda (i) (+ i 1))
                              -2
                              intc)))
         (test-assert "multiplicate elements"
           (let ((v #(0 1 2)))
             (bag=? (bag intc 0 0 0 1 1 1 2 2 2)
                    (bag-unfold (lambda (i) (> i 8))
                                (lambda (i)
                                  (vector-ref v
                                              (truncate-remainder i 3)))
                                (lambda (i) (+ i 1))
                                0
                                intc))))))

     (test-group "Predicates"
       (test-assert "bag?" (bag? (bag intc)))

       (test-group "bag-contains?"
         (test-assert "true"
           (bag-contains? (bag intc 1 3 3) 3))
         (test-assert "false"
           (not (bag-contains? (bag intc 1 3 3) 4))))

       (test-group "bag-empty?"
         (test-assert "bag-empty? true" (bag-empty? (bag intc)))
         (test-assert "bag-empty? false" (not (bag-empty? (bag intc 1)))))

       (test-group "bag-disjoint?"
         (test-assert "bag-disjoint? true"
           (bag-disjoint? (bag intc 1 3 5 5) (bag intc 2 2 4 6)))
         (test-assert "bag-disjoint? false"
           (not (bag-disjoint? (bag intc 1 1 2) (bag intc 2 3 3))))))

     (test-group "Accessors"
       ;; This might possibly fail on Gauche, which doesn't always
       ;; preserve hashmap comparator equivalence.
       (test-eqv "result is eqv? to bag's comparator"
         intc
         (bag-element-comparator (bag intc))))

     (test-group "Updaters"
       (test-group "bag-adjoin"
         (test-assert "new elements"
           (bag=? (bag intc 1 2 3 4 5)
                  (bag-adjoin (bag intc 1 3 5) 2 4)))
         (test-assert "existing elements (increase count)"
           (bag=? (bag intc 1 3 3 5 5)
                  (bag-adjoin (bag intc 1 3 5) 3 5)))
         (test-assert "some new elements, some existing"
           (bag=? (bag intc 1 3 3 4 5 5 6)
                  (bag-adjoin (bag intc 1 3 5) 3 4 5 6))))

       (test-group "bag-adjoin!"
         (test-assert "new elements"
           (bag=? (bag intc 1 2 3 4 5)
                  (bag-adjoin! (bag intc 1 3 5) 2 4)))
         (test-assert "existing elements (increase count)"
           (bag=? (bag intc 1 3 3 5 5)
                  (bag-adjoin! (bag intc 1 3 5) 3 5)))
         (test-assert "some new elements, some existing"
           (bag=? (bag intc 1 3 3 4 5 5 6)
                  (bag-adjoin! (bag intc 1 3 5) 3 4 5 6))))

       (test-group "bag-adjoin-all"
         (test-assert "new elements"
           (bag=? (bag intc 1 2 3 4 5)
                  (bag-adjoin-all (bag intc 1 3 5) '(2 4))))
         (test-assert "existing elements (increase count)"
           (bag=? (bag intc 1 3 3 5 5)
                  (bag-adjoin-all (bag intc 1 3 5) '(3 5))))
         (test-assert "some new elements, some existing"
           (bag=? (bag intc 1 3 3 4 5 5 6)
                  (bag-adjoin-all (bag intc 1 3 5) '(3 4 5 6)))))

       (test-group "bag-adjoin-all!"
         (test-assert "new elements"
           (bag=? (bag intc 1 2 3 4 5)
                  (bag-adjoin-all! (bag intc 1 3 5) '(2 4))))
         (test-assert
           "existing elements (increase count)"
           (bag=? (bag intc 1 3 3 5 5)
                  (bag-adjoin-all! (bag intc 1 3 5) '(3 5))))
         (test-assert "some new elements, some existing"
           (bag=? (bag intc 1 3 3 4 5 5 6)
                  (bag-adjoin-all! (bag intc 1 3 5) '(3 4 5 6)))))

       (test-group "bag-delete"
         (test-assert "elements present"
           (bag=? (bag intc 1 2 3 5)
                  (bag-delete (bag intc 1 2 2 3 4 5) 2 4)))
         (test-assert "elements absent"
           (bag=? (bag intc 1 2 3)
                  (bag-delete (bag intc 1 2 3) 4 5)))
         (test-assert "some elements present, some absent"
           (bag=? (bag intc 1 2 3 4 4)
                  (bag-delete (bag intc 1 2 3 4 4 4) 4 5))))

       (test-group "bag-delete!"
         (test-assert "elements present"
           (bag=? (bag intc 1 2 3 5)
                  (bag-delete! (bag intc 1 2 2 3 4 5) 2 4)))
         (test-assert "elements absent"
           (bag=? (bag intc 1 2 3)
                  (bag-delete! (bag intc 1 2 3) 4 5)))
         (test-assert "some elements present, some absent"
           (bag=? (bag intc 1 2 3 4 4)
                  (bag-delete! (bag intc 1 2 3 4 4 4) 4 5))))

       (test-group "bag-delete-all"
         (test-assert "elements present"
           (bag=? (bag intc 1 2 3 5)
                  (bag-delete-all (bag intc 1 2 2 3 4 5) '(2 4))))
         (test-assert "elements absent"
           (bag=? (bag intc 1 2 3)
                  (bag-delete-all (bag intc 1 2 3) '(4 5))))
         (test-assert "some elements present, some absent"
           (bag=? (bag intc 1 2 3 4 4)
                  (bag-delete-all (bag intc 1 2 3 4 4 4) '(4 5)))))

       (test-group "bag-delete-all!"
         (test-assert "elements present"
           (bag=? (bag intc 1 2 3 5)
                  (bag-delete-all! (bag intc 1 2 2 3 4 5) '(2 4))))
         (test-assert "elements absent"
           (bag=? (bag intc 1 2 3)
                  (bag-delete-all! (bag intc 1 2 3) '(4 5))))
         (test-assert "some elements present, some absent"
           (bag=? (bag intc 1 2 3 4 4)
                  (bag-delete-all! (bag intc 1 2 3 4 4 4) '(4 5)))))

       (test-assert "bag-pop"
         (let*-values (((els) '(1 1 2 3 3 4))
                       ((b x) (bag-pop (list->bag intc els))))
           (and (memv x els)
                (= (bag-element-count b x)
                   (- (s1:count (lambda (y) (eqv? x y)) els) 1)))))

       (test-assert "bag-pop!"
         (let*-values (((els) '(1 1 2 3 3 4))
                       ((b x) (bag-pop! (list->bag intc els))))
           (and (memv x els)
                (= (bag-element-count b x)
                   (- (s1:count (lambda (y) (eqv? x y)) els) 1))))))

     (test-group "bag-size"
       (test-eqv "empty bag"
         0
         (bag-size (bag intc)))
       (test-eqv "nonempty bag"
         4
         (bag-size (bag intc 1 1 1 1))))

     (test-group "Searching"
       (test-group "bag-find"
         (test-assert "success"
           (memv (bag-find even? (bag intc 1 2 3 4) (lambda () #f))
                 '(2 4)))
         (test-eqv "failure"
           'z
           (bag-find negative? (bag intc 1 2 3 4) (lambda () 'z))))

       (test-group "bag-count"
         (test-eqv "bag-count (1)"
           2
           (bag-count even? (bag intc 1 2 3 4)))
         (test-eqv "bag-count (2)"
           0
           (bag-count negative? (bag intc 1 2 3 4))))

       (test-group "bag-any?"
         (test-assert "true"
           (bag-any? even? (bag intc 1 2 3 4)))
         (test-assert "false"
           (not (bag-any? negative? (bag intc 1 2 3 4)))))

       (test-group "bag-every?"
         (test-assert "true"
           (bag-every? integer? (bag intc 1 2 3 4)))
         (test-assert "false"
           (not (bag-every? even? (bag intc 1 2 3 4))))))

     (test-group "Mapping & folding"
       (test-group "bag-map"
         (test-assert "bag-map (1)"
           (bag=? (bag intc -1 -2 -2 -3)
                  (bag-map - intc (bag intc 1 2 2 3))))
         (test-assert "bag-map (2)"
           (bag=? (bag strc "a" "aa" "aa" "aaa")
                  (bag-map (lambda (k) (make-string k #\a))
                           strc
                           (bag intc 1 2 2 3))))
         (test-assert "bag-map (3)"
           (bag=? (bag intc 0 1 1 2 2)
                  (bag-map abs intc (bag intc 0 -1 -2 1 2)))))

       (test-group "bag-for-each"
         (test-eqv "sum"
           10
           (let ((sum 0))
             (bag-for-each (lambda (k)
                             (set! sum (+ k sum)))
                           (bag intc 1 2 2 2 3))
             sum))
         (test-assert "convert to list"
           (let ((ks '()))
             (bag-for-each (lambda (k)
                             (set! ks (cons k ks)))
                           (bag intc 1 2 3 4 4))
             (s1:lset= eqv? ks '(1 2 3 4 4)))))

       (test-group "bag-fold"
         (test-eqv "sum"
           10
           (bag-fold + 0 (bag intc 1 2 2 2 3)))
         (test-assert "convert to list"
           (s1:lset= eqv?
                     (bag-fold cons '() (bag intc 1 2 3 4 4))
                     '(1 2 3 4 4))))

       (test-group "bag-filter"
         (test-assert "preserve some"
           (bag=? (bag intc 1 3 5 5)
                  (bag-filter odd? (bag intc 1 2 3 4 5 5))))
         (test-assert "preserve none"
           (bag-empty? (bag-filter negative? (bag intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (bag=? (bag strc "a" "b" "b" "c")
                  (bag-filter (lambda (s) #t)
                              (bag strc "a" "b" "c" "b")))))

       (test-group "bag-filter!"
         (test-assert "preserve some"
           (bag=? (bag intc 1 3 5 5)
                  (bag-filter! odd? (bag intc 1 2 3 4 5 5))))
         (test-assert "preserve none"
           (bag-empty? (bag-filter! negative? (bag intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (bag=? (bag strc "a" "b" "b" "c")
                  (bag-filter! (lambda (s) #t)
                               (bag strc "a" "b" "c" "b")))))

       (test-group "bag-remove"
         (test-assert "preserve some"
           (bag=? (bag intc 2 4)
                  (bag-remove odd? (bag intc 1 2 3 4 5 5))))
         (test-assert "preserve none"
           (bag-empty? (bag-remove positive? (bag intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (bag=? (bag strc "a" "b" "c" "b")
                  (bag-remove (lambda (s) #f)
                              (bag strc "a" "b" "c" "b")))))

       (test-group "bag-remove!"
         (test-assert "preserve some"
           (bag=? (bag intc 2 4)
                  (bag-remove! odd? (bag intc 1 2 3 4 5 5))))
         (test-assert "preserve none"
           (bag-empty? (bag-remove! positive? (bag intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (bag=? (bag strc "a" "b" "c" "b")
                  (bag-remove! (lambda (s) #f)
                               (bag strc "a" "b" "c" "b")))))

       (test-group "bag-partition"
         (test-assert "empty bag argument"
           (let-values (((p1 p2) (bag-partition even? (bag intc))))
             (and (bag-empty? p1) (bag-empty? p2))))
         (test-assert "all in first bag"
           (let*-values (((b) (bag intc 1 2 3 4 5))
                         ((p1 p2) (bag-partition (lambda (k) #t) b)))
             (and (bag=? b p1) (bag-empty? p2))))
         (test-assert "all in second bag"
           (let*-values (((b) (bag intc 1 2 3 4 5))
                         ((p1 p2) (bag-partition (lambda (k) #f) b)))
             (and (bag-empty? p1) (bag=? b p2))))
         (test-assert "both bags populated"
           (let*-values (((b) (bag intc 1 2 2 3 4 5 5))
                         ((p1 p2) (bag-partition even? b)))
             (and (bag=? p1 (bag intc 2 2 4))
                  (bag=? p2 (bag intc 1 3 5 5))))))

       (test-group "bag-partition!"
         (test-assert "empty bag argument"
           (let-values (((p1 p2) (bag-partition! even? (bag intc))))
             (and (bag-empty? p1) (bag-empty? p2))))
         (test-assert "all in first bag"
           (let-values (((p1 p2)
                         (bag-partition! (lambda (k) #t)
                                         (bag intc 1 2 3))))
             (and (bag=? (bag intc 1 2 3) p1) (bag-empty? p2))))
         (test-assert "all in second bag"
           (let-values (((p1 p2)
                         (bag-partition! (lambda (k) #f)
                                         (bag intc 1 2 3))))
             (and (bag-empty? p1) (bag=? (bag intc 1 2 3) p2))))
         (test-assert "both bags populated"
           (let-values (((p1 p2)
                         (bag-partition! even?
                                         (bag intc 1 2 2 3 4 5 5))))
             (and (bag=? p1 (bag intc 2 2 4))
                  (bag=? p2 (bag intc 1 3 5 5))))))
         )

     (test-group "bag-copy"
       (test-assert "result is bag=?"
         (let ((s (bag intc 1 2 3 4 5)))
           (bag=? s (bag-copy s))))
       (test-assert "result is not eqv?"
         (let ((s (bag intc 1 2 3 4 5)))
           (not (eqv? s (bag-copy s))))))

     (test-group "Conversion"
       (test-group "bag->list"
         (test-assert "null"
           (null? (bag->list (bag intc))))
         (test-assert "nonempty"
           (equal? '(1 2 2 3 3 3 4)
                   (s132:list-sort <
                                   (bag->list
                                    (bag intc 1 2 2 3 3 3 4))))))

       (test-group "list->bag"
         (test-assert "empty"
           (bag-empty? (list->bag intc '())))
         (test-assert "nonempty"
           (bag=? (bag intc 1 2 3 4)
                  (list->bag intc '(1 2 3 4))))))

     (test-group "Comparison"
       (test-group "bag=?"
         (test-assert "3 bags, true"
           (bag=? (bag intc 1 2 2 3)
                  (bag intc 2 1 2 3)
                  (bag intc 2 3 2 1)))
         (test-assert "3 bags, false (different elements)"
           (not
            (bag=? (bag intc 1 2 2 3)
                   (bag intc 3 1 2 2)
                   (bag intc 0 2 3 2 1))))
         (test-assert "3 bags, false (different element counts)"
           (not
            (bag=? (bag intc 1 2 2 3)
                   (bag intc 3 1 2 2)
                   (bag intc 3 2 1)))))

       (test-group "bag<?"
         (test-assert "2 sets, true (1)"
           (bag<? (bag intc) (bag intc 1)))
         (test-assert "2 sets, true (2)"
           (bag<? (bag intc 1) (bag intc 1 2)))
         (test-assert "2 sets, true (3)"
           (bag<? (bag intc 1) (bag intc 1 1 1)))
         (test-assert "2 sets, false (1)"
           (not (bag<? (bag intc 1) (bag intc))))
         (test-assert "2 sets, false (2)"
           (not (bag<? (bag intc 1 2) (bag intc 1))))
         (test-assert "2 sets, false (3)"
           (not (bag<? (bag intc 1 1) (bag intc 1 2))))
         (test-assert "2 sets, false (4)"
           (not (bag<? (bag intc 1 1) (bag intc 1 1))))
         (test-assert "3 sets, true (1)"
           (bag<? (bag intc 1) (bag intc 1 1) (bag intc 1 1 2)))
         (test-assert "3 sets, true (2)"
           (bag<? (bag intc 1 2) (bag intc 1 1 2 2) (bag intc 1 1 2 2 2)))
         (test-assert "3 sets, false (1)"
           (not (bag<? (bag intc 1) (bag intc 1 1) (bag intc 1 2 2))))
         (test-assert "3 sets, false (2)"
           (not (bag<? (bag intc 1 1) (bag intc 1 2) (bag intc 1 1 2 2)))))

       (test-group "bag<=?"
         (test-assert "2 sets, true (1)"
           (bag<=? (bag intc) (bag intc 1)))
         (test-assert "2 sets, true (2)"
           (bag<=? (bag intc 1) (bag intc 1)))
         (test-assert "2 sets, true (3)"
           (bag<=? (bag intc 1 1) (bag intc 1 1)))
         (test-assert "2 sets, true (4)"
           (bag<=? (bag intc 1) (bag intc 1 1 1)))
         (test-assert "2 sets, false (1)"
           (not (bag<=? (bag intc 1) (bag intc))))
         (test-assert "2 sets, false (2)"
           (not (bag<=? (bag intc 1 2) (bag intc 1))))
         (test-assert "2 sets, false (3)"
           (not (bag<=? (bag intc 1 1) (bag intc 1 2))))
         (test-assert "3 sets, true (1)"
           (bag<=? (bag intc 1) (bag intc 1 1) (bag intc 1 1 2)))
         (test-assert "3 sets, true (2)"
           (bag<=?
            (bag intc 1 2) (bag intc 1 1 2 2) (bag intc 1 1 2 2 2)))
         (test-assert "3 sets, true (3)"
           (bag<=? (bag intc 1) (bag intc 1 1 2) (bag intc 1 1 2)))
         (test-assert "3 sets, false (1)"
           (not (bag<=? (bag intc 1) (bag intc 1 1) (bag intc 1 2 2))))
         (test-assert "3 sets, false (2)"
           (not
            (bag<=? (bag intc 1 1) (bag intc 1 2) (bag intc 1 1 2 2)))))

       (test-group "bag>?"
         (test-assert "2 sets, true (1)"
           (bag>? (bag intc 1) (bag intc)))
         (test-assert "2 sets, true (2)"
           (bag>? (bag intc 1 2) (bag intc 1)))
         (test-assert "2 sets, true (3)"
           (bag>? (bag intc 1 1 1) (bag intc 1)))
         (test-assert "2 sets, false (1)"
           (not (bag>? (bag intc) (bag intc 1))))
         (test-assert "2 sets, false (2)"
           (not (bag>? (bag intc 1) (bag intc 1 2))))
         (test-assert "2 sets, false (3)"
           (not (bag>? (bag intc 1 2) (bag intc 1 1))))
         (test-assert "2 sets, false (4)"
           (not (bag>? (bag intc 1 1) (bag intc 1 1))))
         (test-assert "3 sets, true (1)"
           (bag>? (bag intc 1 1 2) (bag intc 1 1) (bag intc 1)))
         (test-assert "3 sets, true (2)"
           (bag>? (bag intc 1 1 2 2 2) (bag intc 1 1 2 2) (bag intc 1 2)))
         (test-assert "3 sets, false (1)"
           (not (bag>? (bag intc 1 2 2) (bag intc 1 1) (bag intc 1))))
         (test-assert "3 sets, false (2)"
           (not (bag>? (bag intc 1 1 2 2) (bag intc 1 2) (bag intc 1 1)))))

       (test-group "bag>=?"
         (test-assert "2 sets, true (1)"
           (bag>=? (bag intc 1) (bag intc)))
         (test-assert "2 sets, true (2)"
           (bag>=? (bag intc 1) (bag intc 1)))
         (test-assert "2 sets, true (3)"
           (bag>=? (bag intc 1 1) (bag intc 1 1)))
         (test-assert "2 sets, true (4)"
           (bag>=? (bag intc 1 1 1) (bag intc 1)))
         (test-assert "2 sets, false (1)"
           (not (bag>=? (bag intc) (bag intc 1))))
         (test-assert "2 sets, false (2)"
           (not (bag>=? (bag intc 1) (bag intc 1 2))))
         (test-assert "2 sets, false (3)"
           (not (bag>=? (bag intc 1 2) (bag intc 1 1))))
         (test-assert "3 sets, true (1)"
           (bag>=? (bag intc 1 1 2) (bag intc 1 1) (bag intc 1)))
         (test-assert "3 sets, true (2)"
           (bag>=?
            (bag intc 1 1 2 2 2) (bag intc 1 1 2 2) (bag intc 1 2)))
         (test-assert "3 sets, true (3)"
           (bag>=? (bag intc 1 1 2) (bag intc 1 1 2) (bag intc 1)))
         (test-assert "3 sets, false (1)"
           (not (bag>=? (bag intc 1 2 2) (bag intc 1 1) (bag intc 1))))
         (test-assert "3 sets, false (2)"
           (not
            (bag>=? (bag intc 1 1 2 2) (bag intc 1 2) (bag intc 1 1))))))

     (test-group "Set theory (functional)"
       (test-group "bag-union"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3) (bag-union (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-union (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag=? (bag intc 1 2 3)
                  (bag-union (bag intc) (bag intc 1 2 3))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 1 2 3 4)
                  (bag-union (bag intc 1 3) (bag intc 2 4))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 1 1 2 2 3 3)
                  (bag-union (bag intc 1 1 2 3) (bag intc 1 2 2 3 3))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 1 2 3 4)
                  (bag-union (bag intc 1 3) (bag intc 2) (bag intc 4))))
         (test-assert "3 bags (2)"
           (bag=? (bag intc 1 2 3 3 4 4 4 5 5)
                  (bag-union (bag intc 1 3 3 5 5)
                             (bag intc)
                             (bag intc 2 3 3 4 4 4 5)))))

       (test-group "bag-intersection"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-intersection (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag-empty? (bag-intersection (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag-empty? (bag-intersection (bag intc) (bag intc 1 2 3))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 2)
                  (bag-intersection (bag intc 1 2 3) (bag intc 2 4 6))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 2 2 3)
                  (bag-intersection (bag intc 1 1 2 2 3)
                                    (bag intc 2 2 3 4 4 6))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 1 2)
                  (bag-intersection (bag intc 1 2 3)
                                    (bag intc 1 2 4)
                                    (bag intc 5 1 2 3))))
         (test-assert "3 bags (2)"
           (bag-empty?
            (bag-intersection (bag intc 1 2 3)
                              (bag intc)
                              (bag intc 1 4 6))))
         (test-assert "3 bags (3)"
           (bag=? (bag intc 1 1 2 3 5 5 5)
                  (bag-intersection (bag intc 1 1 2 3 5 5 5)
                                    (bag intc 1 1 2 3 4 4 5 5 5)
                                    (bag intc 5 5 5 1 1 1 2 2 3)))))

       (test-group "bag-difference"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-difference (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-difference (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag=? (bag intc 2)
                  (bag-difference (bag intc 1 2 3) (bag intc 1 3 5))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 1 3 5)
                  (bag-difference (bag intc 1 3 5) (bag intc 2 4 6))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 3 3 5)
                  (bag-difference (bag intc 1 3 3 3 5)
                                  (bag intc 1 1 2 3 4))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 2)
                  (bag-difference (bag intc 1 2 3)
                                  (bag intc 1 4)
                                  (bag intc 3 7))))
         (test-assert "3 bags (2)"
           (bag=? (bag intc 1 2)
                  (bag-difference (bag intc 1 2 3)
                                  (bag intc)
                                  (bag intc 3 7))))
         (test-assert "3 bags (3)"
           (bag=? (bag intc 1 2)
                  (bag-difference (bag intc 1 2 2 2 3 3 3)
                                  (bag intc 2 3 3 4)
                                  (bag intc 3 2 7)))))

       (test-group "bag-xor"
         (test-assert "bag-xor (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-xor (bag intc) (bag intc 1 2 3))))
         (test-assert "bag-xor (2)"
           (bag=? (bag intc 1 2 3)
                  (bag-xor (bag intc 1 2 3) (bag intc))))
         (test-assert "bag-xor (3)"
           (bag=? (bag intc 5 7 6)
                  (bag-xor (bag intc 1 4 5 7) (bag intc 1 4 6))))
         (test-assert "bag-xor (4)"
           (bag=? (bag intc 4 4 5 7 6)
                  (bag-xor (bag intc 1 4 4 4 5 7)
                           (bag intc 1 4 5 5 6)))))
         )

     (test-group "Set theory (linear update)"
       (test-group "bag-union!"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3) (bag-union! (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-union! (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag=? (bag intc 1 2 3)
                  (bag-union! (bag intc) (bag intc 1 2 3))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 1 2 3 4)
                  (bag-union! (bag intc 1 3) (bag intc 2 4))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 1 1 2 2 3 3)
                  (bag-union! (bag intc 1 1 2 3) (bag intc 1 2 2 3 3))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 1 2 3 4)
                  (bag-union! (bag intc 1 3) (bag intc 2) (bag intc 4))))
         (test-assert "3 bags (2)"
           (bag=? (bag intc 1 2 3 3 4 4 4 5 5)
                  (bag-union! (bag intc 1 3 3 5 5)
                              (bag intc)
                              (bag intc 2 3 3 4 4 4 5)))))

       (test-group "bag-intersection!"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-intersection! (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag-empty? (bag-intersection! (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag-empty? (bag-intersection! (bag intc) (bag intc 1 2 3))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 2)
                  (bag-intersection! (bag intc 1 2 3) (bag intc 2 4 6))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 2 2 3)
                  (bag-intersection! (bag intc 1 1 2 2 3)
                                     (bag intc 2 2 3 4 4 6))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 1 2)
                  (bag-intersection! (bag intc 1 2 3)
                                     (bag intc 1 2 4)
                                     (bag intc 5 1 2 3))))
         (test-assert "3 bags (2)"
           (bag-empty?
            (bag-intersection! (bag intc 1 2 3)
                               (bag intc)
                               (bag intc 1 4 6))))
         (test-assert "3 bags (3)"
           (bag=? (bag intc 1 1 2 3 5 5 5)
                  (bag-intersection! (bag intc 1 1 2 3 5 5 5)
                                     (bag intc 1 1 2 3 4 4 5 5 5)
                                     (bag intc 5 5 5 1 1 1 2 2 3)))))

       (test-group "bag-difference!"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-difference! (bag intc 1 2 3))))
         (test-assert "2 bags (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-difference! (bag intc 1 2 3) (bag intc))))
         (test-assert "2 bags (2)"
           (bag=? (bag intc 2)
                  (bag-difference! (bag intc 1 2 3) (bag intc 1 3 5))))
         (test-assert "2 bags (3)"
           (bag=? (bag intc 1 3 5)
                  (bag-difference! (bag intc 1 3 5) (bag intc 2 4 6))))
         (test-assert "2 bags (4)"
           (bag=? (bag intc 3 3 5)
                  (bag-difference! (bag intc 1 3 3 3 5)
                                   (bag intc 1 1 2 3 4))))
         (test-assert "3 bags (1)"
           (bag=? (bag intc 2)
                  (bag-difference! (bag intc 1 2 3)
                                   (bag intc 1 4)
                                   (bag intc 3 7))))
         (test-assert "3 bags (2)"
           (bag=? (bag intc 1 2)
                  (bag-difference! (bag intc 1 2 3)
                                   (bag intc)
                                   (bag intc 3 7))))
         (test-assert "3 bags (3)"
           (bag=? (bag intc 1 2)
                  (bag-difference! (bag intc 1 2 2 2 3 3 3)
                                   (bag intc 2 3 3 4)
                                   (bag intc 3 2 7)))))

       (test-group "bag-xor!"
         (test-assert "bag-xor! (1)"
           (bag=? (bag intc 1 2 3)
                  (bag-xor! (bag intc) (bag intc 1 2 3))))
         (test-assert "bag-xor! (2)"
           (bag=? (bag intc 1 2 3)
                  (bag-xor! (bag intc 1 2 3) (bag intc))))
         (test-assert "bag-xor! (3)"
           (bag=? (bag intc 5 7 6)
                  (bag-xor! (bag intc 1 4 5 7) (bag intc 1 4 6))))
         (test-assert "bag-xor! (4)"
           (bag=? (bag intc 4 4 5 7 6)
                  (bag-xor! (bag intc 1 4 4 4 5 7)
                            (bag intc 1 4 5 5 6))))))

     (test-group "bag-comparator"
       (test-assert "is a SRFI 128 comparator"
         (s128:comparator? bag-comparator))
       (test-assert "type-test predicate is true on bags"
         ((s128:comparator-type-test-predicate bag-comparator)
          (bag intc 1 2 3))))

     (test-group "Bag sum & product"
       (test-group "bag-sum"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-sum (bag intc 1 2 3))))
         (test-assert "2 disjoint bags"
           (bag=? (bag intc 1 2 3 4)
                  (bag-sum (bag intc 1 3) (bag intc 2 4))))
         (test-assert "2 bags with common elements"
           (bag=? (bag intc 1 1 2 3 4 4 4)
                  (bag-sum (bag intc 1 2 4 4)
                           (bag intc 1 3 4))))
         (test-assert "3 disjoint bags"
           (bag=? (bag intc 1 2 3 4 8 9)
                  (bag-sum (bag intc 1 3)
                           (bag intc 2 4)
                           (bag intc 8 9))))
         (test-assert "3 bags with common elements"
           (bag=? (bag intc 1 1 1 2 3 3 4 4 4 5)
                  (bag-sum (bag intc 1 2 4 4)
                           (bag intc 1 3 4)
                           (bag intc 5 3 1)))))

       (test-group "bag-sum!"
         (test-assert "1 bag"
           (bag=? (bag intc 1 2 3)
                  (bag-sum! (bag intc 1 2 3))))
         (test-assert "2 disjoint bags"
           (bag=? (bag intc 1 2 3 4)
                  (bag-sum! (bag intc 1 3) (bag intc 2 4))))
         (test-assert "2 bags with common elements"
           (bag=? (bag intc 1 1 2 3 4 4 4)
                  (bag-sum! (bag intc 1 2 4 4)
                           (bag intc 1 3 4))))
         (test-assert "3 disjoint bags"
           (bag=? (bag intc 1 2 3 4 8 9)
                  (bag-sum! (bag intc 1 3)
                           (bag intc 2 4)
                           (bag intc 8 9))))
         (test-assert "3 bags with common elements"
           (bag=? (bag intc 1 1 1 2 3 3 4 4 4 5)
                  (bag-sum! (bag intc 1 2 4 4)
                           (bag intc 1 3 4)
                           (bag intc 5 3 1)))))

       (test-group "bag-product"
         (test-assert "k = 0 returns empty bag"
           (bag-empty? (bag-product 0 (bag intc 1 2 3))))
         (test-assert "with k = 1, identity"
           (bag=? (bag intc 1 2 3)
                  (bag-product 1 (bag intc 1 2 3))))
         (test-assert "double element count"
           (bag=? (bag intc 1 1 2 2 3 3)
                  (bag-product 2 (bag intc 1 2 3)))))

       (test-group "bag-product!"
         (test-assert "k = 0 returns empty bag"
           (bag-empty? (bag-product! 0 (bag intc 1 2 3))))
         (test-assert "with k = 1, identity"
           (bag=? (bag intc 1 2 3)
                  (bag-product! 1 (bag intc 1 2 3))))
         (test-assert "double element count"
           (bag=? (bag intc 1 1 2 2 3 3)
                  (bag-product! 2 (bag intc 1 2 3))))))

     (test-group "Bag 'unique' procedures"
       (test-group "bag-unique-size"
         (test-eqv "empty bag"
           0
           (bag-unique-size (bag intc)))
         (test-eqv "nonempty bag"
           4
           (bag-unique-size (bag intc 1 1 2 2 2 3 3 4 4 4))))

       (test-assert "bag-for-each-unique"
         (let ((size 0) (elts '()))
           (bag-for-each-unique
            (lambda (e c)
              (set! size (+ size c))
              (set! elts (cons e elts)))
            (bag intc 1 1 2 2 2 3 3 4 4 4))
           (and (= size 10)
                (s1:lset= eqv? elts '(1 2 3 4)))))

       (test-assert "bag-fold-unique"
         (s1:lset= equal?
                   '((1 . 2) (2 . 3) (3 . 2) (4 . 3))
                   (bag-fold-unique
                    (lambda (e c ps)
                      (cons (cons e c) ps))
                    '()
                    (bag intc 1 1 2 2 2 3 3 4 4 4))))
       )

     (test-group "Bag increment & decrement"
       (test-group "bag-increment"
         (test-assert "with k = 0, identity (1)"
           (bag-empty? (bag-increment (bag intc) 1 0)))
         (test-assert "with k = 0, identity (2)"
           (bag=? (bag intc 1)
                  (bag-increment (bag intc 1) 1 0)))
         (test-assert "add new elements"
           (bag=? (bag intc 1 2 3 3)
                  (bag-increment (bag intc 1 2) 3 2)))
         (test-assert "add more of an existing element"
           (bag=? (bag intc 1 2 2 2)
                  (bag-increment (bag intc 1 2) 2 2))))

       (test-group "bag-increment!"
         (test-assert "with k = 0, identity (1)"
           (bag-empty? (bag-increment! (bag intc) 1 0)))
         (test-assert "with k = 0, identity (2)"
           (bag=? (bag intc 1)
                  (bag-increment! (bag intc 1) 1 0)))
         (test-assert "add new elements"
           (bag=? (bag intc 1 2 3 3)
                  (bag-increment! (bag intc 1 2) 3 2)))
         (test-assert "add more of an existing element"
           (bag=? (bag intc 1 2 2 2)
                  (bag-increment! (bag intc 1 2) 2 2))))

       (test-group "bag-decrement"
         (test-assert "with k = 0, identity (1)"
           (bag-empty? (bag-decrement (bag intc) 1 0)))
         (test-assert "with k = 0, identity (2)"
           (bag=? (bag intc 1)
                  (bag-decrement (bag intc 1) 1 0)))
         (test-assert "with non-element, identity"
           (bag=? (bag intc 1 2)
                  (bag-decrement (bag intc 1 2) 3 2)))
         (test-assert "remove some of an existing element"
           (bag=? (bag intc 1 2)
                  (bag-decrement (bag intc 1 2 2 2) 2 2)))
         (test-assert "resulting count is floored to 0"
           (bag=? (bag intc 1 2)
                  (bag-decrement (bag intc 1 2 3) 3 100))))

       (test-group "bag-decrement!"
         (test-assert "with k = 0, identity (1)"
           (bag-empty? (bag-decrement! (bag intc) 1 0)))
         (test-assert "with k = 0, identity (2)"
           (bag=? (bag intc 1)
                  (bag-decrement! (bag intc 1) 1 0)))
         (test-assert "with non-element, identity"
           (bag=? (bag intc 1 2)
                  (bag-decrement! (bag intc 1 2) 3 2)))
         (test-assert "remove some of an existing element"
           (bag=? (bag intc 1 2)
                  (bag-decrement! (bag intc 1 2 2 2) 2 2)))
         (test-assert "resulting count is floored to 0"
           (bag=? (bag intc 1 2)
                  (bag-decrement! (bag intc 1 2 3) 3 100)))))

     (test-group "bag-element-count"
       (test-eqv "bag-element-count (1)"
         0
         (bag-element-count (bag intc) 1))
       (test-eqv "bag-element-count (2)"
         1
         (bag-element-count (bag intc 1 2 3 3) 1))
       (test-eqv "bag-element-count (3)"
         2
         (bag-element-count (bag intc 1 2 3 3) 3)))

     (test-group "Extra bag conversions"
       (test-group "bag->set"
         (test-assert "empty bag"
           (set-empty? (bag->set (bag intc))))
         (test-eqv "set comparator is eqv? to original"
           intc
           (set-element-comparator
            (bag->set (bag intc))))
         (test-assert "nonempty bag"
           (set=? (set intc 1 2 3 4)
                  (bag->set (bag intc 1 1 2 3 3 3 4 4 4 4)))))

       (test-group "set->bag"
         (test-assert "empty set"
           (bag-empty? (set->bag (set intc))))
         (test-eqv "bag comparator is eqv? to original"
           intc
           (bag-element-comparator
            (set->bag (set intc))))
         (test-assert "nonempty set"
           (bag=? (bag intc 1 2 3 4)
                  (set->bag (set intc 3 2 1 4)))))

       (test-group "set->bag!"
         (test-assert "add empty set"
           (bag-empty? (set->bag! (bag intc) (set intc))))
         (test-eqv "bag comparator is eqv? to original"
           intc
           (bag-element-comparator
            (set->bag! (bag intc) (set intc))))
         (test-assert "add nonempty set to empty bag"
           (bag=? (bag intc 1 2 3 4)
                  (set->bag! (bag intc) (set intc 3 2 1 4))))
         (test-assert "add nonempty set to empty bag"
           (bag=? (bag intc 1 2 3 4 5 5 6)
                  (set->bag! (bag intc 5 5 6) (set intc 3 2 1 4)))))

       (test-group "bag->alist"
         (test-eqv "empty bag"
           '()
           (bag->alist (bag intc)))
         (test-assert "nonempty bag"
           (s1:lset= equal?
                     '((1 . 2) (2 . 1) (3 . 3))
                     (bag->alist (bag intc 1 2 3 3 1 3)))))

       (test-group "alist->bag"
         (test-assert "empty alist"
           (bag-empty? (alist->bag intc '())))
         (test-assert "nonempty alist"
           (bag=? (bag intc 1 1 2 3 3 3)
                  (alist->bag intc
                              '((1 . 2) (2 . 1) (3 . 3)))))
         (test-eqv "bag comparator is eqv? to argument"
           intc
           (bag-element-comparator
            (alist->bag intc '())))))

     (test-end))))
