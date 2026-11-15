;;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT
(define-library (test set)
  (export run-tests)
  (import (scheme base)
          (prefix (srfi 1) s1:)
          (prefix (srfi 128) s128:)
	  (srfi 64)
	  (srfi NNN set)
	  )
  (begin
   (define intc (s128:make-comparator integer? = #f s128:number-hash))
   (define strc (s128:make-comparator string?
                                      string=?
                                      #f
                                      s128:string-hash))

   (define (run-tests)
     (test-begin "Sets")

     (test-group "Constructors"
       (test-group "set"
         (test-assert "empty set"
           (set-empty? (set intc)))

         (test-assert "nonempty set"
           (s1:lset= eqv?
                     '(1 2 3 4 5)
                     (set->list (set intc 1 2 3 4 5))))

         (test-assert "duplicate elements ignored"
           (s1:lset= eqv?
                     '(1 2 3 4 5)
                     (set->list (set intc 1 2 3 3 4 4 5)))))

       (test-group "set-unfold"
         (test-assert "empty set"
           (set-empty? (set-unfold (lambda (i) #t)
                                   values
                                   (lambda (i) (+ i 1))
                                   0
                                   intc)))

         (test-assert "nonempty set"
           (s1:lset= eqv?
                     '(0 1 2 3 4)
                     (set->list
                      (set-unfold (lambda (i) (> i 4))
                                  values
                                  (lambda (i) (+ i 1))
                                  0
                                  intc))))

         (test-assert "duplicate elements ignored"
           (s1:lset= eqv?
                     '(0 1 2)
                     (set->list
                      (set-unfold (lambda (i) (> i 2))
                                  abs
                                  (lambda (i) (+ i 1))
                                  -2
                                  intc)))))
       )

     (test-group "Predicates"
       (test-assert "set?" (set? (set intc)))

       (test-group "set-contains?"
         (test-assert "true"
           (set-contains? (set intc 1 3 2) 3))
         (test-assert "false"
           (not (set-contains? (set intc 1 3 2) 4))))

       (test-group "set-empty?"
         (test-assert "empty set" (set-empty? (set intc)))
         (test-assert "nonempty set" (not (set-empty? (set intc 1)))))

       (test-group "set-disjoint?"
         (test-assert "disjoint sets"
           (set-disjoint? (set intc 1 3 5) (set intc 2 4 6)))
         (test-assert "overlapping sets"
           (not (set-disjoint? (set intc 1 2) (set intc 2 3)))))
       )

     (test-group "Accessors"
       (test-group "set-member"
         (test-assert "returned element is eqv? to original"
           (let ((e1 (string #\a)))
             (set-member (set strc e1) "a" 'z)))
         (test-eqv "element absent"
           'z
           (set-member (set intc 2 4 5 6) 1 'z)))

       ;; This might possibly fail on Gauche, which doesn't always
       ;; preserve hashmap comparator equivalence.
       (test-eqv
         "result is eqv? to set's comparator"
         intc
         (set-element-comparator (set intc))))

     (test-group "Updaters"
       (test-group "set-adjoin"
         (test-assert "new elements"
           (set=? (set intc 1 2 3 4 5)
                  (set-adjoin (set intc 1 3 5) 2 4)))
         (test-assert "existing elements"
           (set=? (set intc 1 3 5)
                  (set-adjoin (set intc 1 3 5) 3 5)))
         (test-assert "some new elements, some existing"
           (set=? (set intc 1 3 4 5 6)
                  (set-adjoin (set intc 1 3 5) 3 4 5 6))))

       (test-group "set-adjoin!"
         (test-assert "new elements"
           (set=? (set intc 1 2 3 4 5)
                  (set-adjoin! (set intc 1 3 5) 2 4)))
         (test-assert "existing elements"
           (set=? (set intc 1 3 5)
                  (set-adjoin! (set intc 1 3 5) 3 5)))
         (test-assert
           "some new elements, some existing"
           (set=? (set intc 1 3 4 5 6)
                  (set-adjoin! (set intc 1 3 5) 3 4 5 6))))

       (test-group "set-adjoin-all"
         (test-assert "new elements"
           (set=? (set intc 1 2 3 4 5)
                  (set-adjoin-all (set intc 1 3 5) '(2 4))))
         (test-assert "existing elements"
           (set=? (set intc 1 3 5)
                  (set-adjoin-all (set intc 1 3 5) '(3 5))))
         (test-assert
           "some new elements, some existing"
           (set=? (set intc 1 3 4 5 6)
                  (set-adjoin-all (set intc 1 3 5) '(3 4 5 6)))))

       (test-group "set-adjoin-all!"
         (test-assert "new elements"
           (set=? (set intc 1 2 3 4 5)
                  (set-adjoin-all! (set intc 1 3 5) '(2 4))))
         (test-assert "existing elements"
           (set=? (set intc 1 3 5)
                  (set-adjoin-all! (set intc 1 3 5) '(3 5))))
         (test-assert
           "some new elements, some existing"
           (set=? (set intc 1 3 4 5 6)
                  (set-adjoin-all! (set intc 1 3 5) '(3 4 5 6)))))

       ;; These certainly show the weirdness of set-replace.  set-member
       ;; is the only other procedure that cares whether elements are
       ;; eqv? as well as equal (in the sense of the set's comparator).
       (test-group "set-replace"
         (test-assert "new element is eqv? to argument"
           (let ((e1 (string #\a)) (e2 (string #\a)))
             (eqv? e2
                   (set-member (set-replace (set strc e1) e2) "a" #f))))
         (test-assert
           "element absent"
           (let ((es (list (string #\a))))
             (s1:lset= eqv?
                       es
                       (set->list
                        (set-replace (list->set strc es) "b"))))))

       (test-group "set-replace!"
         (test-assert
           "new element is eqv? to argument"
           (let ((e1 (string #\a)) (e2 (string #\a)))
             (eqv? e2
                   (set-member (set-replace! (set strc e1) e2)
                               "a"
                               #f))))
         (test-assert "element absent"
           (let ((es (list (string #\a))))
             (s1:lset= eqv?
                       es
                       (set->list
                        (set-replace! (list->set strc es) "b"))))))

       (test-group "set-delete"
         (test-assert "elements present"
           (set=? (set intc 1 3 5)
                  (set-delete (set intc 1 2 3 4 5) 2 4)))
         (test-assert "elements absent"
           (set=? (set intc 1 2 3)
                  (set-delete (set intc 1 2 3) 4 5)))
         (test-assert "some elements present, some absent"
           (set=? (set intc 1 2 3)
                  (set-delete (set intc 1 2 3 4) 4 5))))

       (test-group "set-delete!"
         (test-assert "elements present"
           (set=? (set intc 1 3 5)
                  (set-delete! (set intc 1 2 3 4 5) 2 4)))
         (test-assert "elements absent"
           (set=? (set intc 1 2 3)
                  (set-delete! (set intc 1 2 3) 4 5)))
         (test-assert "some elements present, some absent"
           (set=? (set intc 1 2 3)
                  (set-delete! (set intc 1 2 3 4) 4 5))))

       (test-group "set-delete-all"
         (test-assert "elements present"
           (set=? (set intc 1 3 5)
                  (set-delete-all (set intc 1 2 3 4 5) '(2 4))))
         (test-assert "elements absent"
           (set=? (set intc 1 2 3)
                  (set-delete-all (set intc 1 2 3) '(4 5))))
         (test-assert "some elements present, some absent"
           (set=? (set intc 1 2 3)
                  (set-delete-all (set intc 1 2 3 4) '(4 5)))))

       (test-group "set-delete-all!"
         (test-assert "elements present"
           (set=? (set intc 1 3 5)
                  (set-delete-all! (set intc 1 2 3 4 5) '(2 4))))
         (test-assert "elements absent"
           (set=? (set intc 1 2 3)
                  (set-delete-all! (set intc 1 2 3) '(4 5))))
         (test-assert "some elements present, some absent"
           (set=? (set intc 1 2 3)
                  (set-delete-all! (set intc 1 2 3 4) '(4 5)))))

       (test-group "set-search!"
         (test-assert "insert"
           (let-values (((s x)
                         (set-search! (set intc 1 2 3)
                                      4
                                      (lambda (insert _ig)
                                        (insert 'z))
                                      (lambda (_el update _rem)
                                        (update 4 #t)))))
             (and (set=? s (set intc 1 2 3 4))
                  (eqv? x 'z))))
         (test-assert "ignore"
           (let-values (((s x)
                         (set-search! (set intc 1 2 3)
                                      4
                                      (lambda (_ins ignore)
                                        (ignore 'z))
                                      (lambda (_el _up remove)
                                        (remove #t)))))
             (and (set=? s (set intc 1 2 3))
                  (eqv? x 'z))))
         (test-assert "replace"
           (let-values (((s x)
                         (set-search! (set intc 1 2 3 4)
                                      4
                                      (lambda (_ins ignore)
                                        (ignore #t))
                                      (lambda (_el update _rem)
                                        (update 5 'z)))))
             (and (set=? s (set intc 1 2 3 5))
                  (eqv? x 'z))))
         (test-assert "remove"
           (let-values (((s x)
                         (set-search! (set intc 1 2 3 4)
                                      4
                                      (lambda (_ins ignore)
                                        (ignore #t))
                                      (lambda (_el _up remove)
                                        (remove 'z)))))
             (and (set=? s (set intc 1 2 3))
                  (eqv? x 'z)))))

       (test-assert "set-pop"
         (let*-values (((els) (list 1 2 3 4))
                       ((s x) (set-pop (list->set intc els))))
           (and (memv x els)
                (not (set-contains? s x)))))

       (test-assert "set-pop!"
         (let*-values (((els) (list 1 2 3 4))
                       ((s x) (set-pop! (list->set intc els))))
           (and (memv x els)
                (not (set-contains? s x)))))
       )

     (test-group "set-size"
       (test-eqv "empty set"
         0
         (set-size (set intc)))
       (test-eqv "nonempty set"
         4
         (set-size (set intc 1 2 3 4))))

     (test-group "Searching"
       (test-group "set-find"
         (test-assert "success"
           (memv (set-find even? (set intc 1 2 3 4) (lambda () #f))
                 '(2 4)))
         (test-eqv "failure"
           'z
           (set-find negative? (set intc 1 2 3 4) (lambda () 'z))))

       (test-group "set-count"
         (test-eqv "1"
           2
           (set-count even? (set intc 1 2 3 4)))
         (test-eqv "2"
           0
           (set-count negative? (set intc 1 2 3 4))))

       (test-group "set-any?"
         (test-assert "true"
           (set-any? even? (set intc 1 2 3 4)))
         (test-assert "false"
           (not (set-any? negative? (set intc 1 2 3 4)))))

       (test-group "set-every?"
         (test-assert "true"
           (set-every? integer? (set intc 1 2 3 4)))
         (test-assert "false"
           (not (set-every? even? (set intc 1 2 3 4)))))
       )

     (test-group "Mapping & folding"
       (test-group "set-map"
         (test-assert "isomorphic set (1)"
           (set=? (set intc -1 -2 -3)
                  (set-map - intc (set intc 1 2 3))))
         (test-assert "isomorphic set (2)"
           (set=? (set strc "a" "aa" "aaa")
                  (set-map (lambda (k) (make-string k #\a))
                           strc
                           (set intc 1 2 3))))
         (test-assert "smaller set"
           (set=? (set intc 0 1 2)
                  (set-map abs intc (set intc 0 -1 -2 1 2)))))

       (test-group "set-for-each"
         (test-eqv "sum"
           10
           (let ((sum 0))
             (set-for-each (lambda (k)
                             (set! sum (+ k sum)))
                           (set intc 1 2 3 4))
             sum))
         (test-assert "convert to list"
           (let ((ks '()))
             (set-for-each (lambda (k)
                             (set! ks (cons k ks)))
                           (set intc 1 2 3 4))
             (s1:lset= eqv? ks '(1 2 3 4)))))

       (test-group "set-fold"
         (test-eqv "sum"
           10
           (set-fold + 0 (set intc 1 2 3 4)))
         (test-assert "convert to list"
           (s1:lset= eqv?
                     (set-fold cons '() (set intc 1 2 3 4))
                     '(1 2 3 4))))

       (test-group "set-filter"
         (test-assert "preserve some"
           (set=? (set intc 1 3 5)
                  (set-filter odd? (set intc 1 2 3 4 5))))
         (test-assert "preserve none"
           (set-empty? (set-filter negative? (set intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (set=? (set strc "a" "b" "c")
                  (set-filter (lambda (s) #t) (set strc "a" "c" "b")))))

       (test-group "set-filter!"
         (test-assert "preserve some"
           (set=? (set intc 1 3 5)
                  (set-filter! odd? (set intc 1 2 3 4 5))))
         (test-assert "preserve none"
           (set-empty? (set-filter! negative? (set intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (set=? (set strc "a" "b" "c")
                  (set-filter! (lambda (s) #t)
                               (set strc "a" "c" "b")))))

       (test-group "set-remove"
         (test-assert "preserve some"
           (set=? (set intc 2 4)
                  (set-remove odd? (set intc 1 2 3 4 5))))
         (test-assert "preserve none"
           (set-empty? (set-remove positive? (set intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (set=? (set strc "a" "b" "c")
                  (set-remove (lambda (s) #f)
                              (set strc "a" "c" "b")))))

       (test-group "set-remove!"
         (test-assert "preserve some"
           (set=? (set intc 2 4)
                  (set-remove! odd? (set intc 1 2 3 4 5))))
         (test-assert "preserve none"
           (set-empty? (set-remove! positive? (set intc 1 2 3 4 5))))
         (test-assert "preserve all"
           (set=? (set strc "a" "b" "c")
                  (set-remove! (lambda (s) #f)
                               (set strc "a" "c" "b")))))

       (test-group "set-partition"
         (test-assert "empty set argument"
           (let-values (((p1 p2) (set-partition even? (set intc))))
             (and (set-empty? p1) (set-empty? p2))))
         (test-assert "all in first set"
           (let*-values (((s) (set intc 1 2 3 4 5))
                         ((p1 p2) (set-partition (lambda (k) #t) s)))
             (and (set=? s p1) (set-empty? p2))))
         (test-assert "all in second set"
           (let*-values (((s) (set intc 1 2 3 4 5))
                         ((p1 p2) (set-partition (lambda (k) #f) s)))
             (and (set-empty? p1) (set=? s p2))))
         (test-assert "both sets populated"
           (let*-values (((s) (set intc 1 2 3 4 5))
                         ((p1 p2) (set-partition even? s)))
             (and (set=? p1 (set intc 2 4))
                  (set=? p2 (set intc 1 3 5))))))

       (test-group "set-partition!"
         (test-assert "empty set argument"
           (let-values (((p1 p2) (set-partition! even? (set intc))))
             (and (set-empty? p1) (set-empty? p2))))
         (test-assert "all in first set"
           (let-values (((p1 p2)
                         (set-partition! (lambda (k) #t)
                                         (set intc 1 2 3))))
             (and (set=? (set intc 1 2 3) p1) (set-empty? p2))))
         (test-assert "all in second set"
           (let-values (((p1 p2)
                         (set-partition! (lambda (k) #f)
                                         (set intc 1 2 3))))
             (and (set-empty? p1) (set=? (set intc 1 2 3) p2))))
         (test-assert "both sets populated"
           (let-values (((p1 p2)
                         (set-partition! even? (set intc 1 2 3 4 5))))
             (and (set=? p1 (set intc 2 4))
                  (set=? p2 (set intc 1 3 5)))))))

     (test-group "set-copy"
       (test-assert "result is set=?"
         (let ((s (set intc 1 2 3 4 5)))
           (set=? s (set-copy s))))
       (test-assert "result is not eqv?"
         (let ((s (set intc 1 2 3 4 5)))
           (not (eqv? s (set-copy s))))))

     (test-group "Conversion"
       (test-group "set->list"
         (test-assert "null"
           (null? (set->list (set intc))))
         (test-assert "nonempty"
           (s1:lset= eqv?
                     '(1 2 3 4)
                     (set->list (set intc 1 2 3 4)))))

       (test-group "list->set"
         (test-assert "list->set: empty"
           (set-empty? (list->set intc '())))
         (test-assert "list->set: nonempty"
           (set=? (set intc 1 2 3 4)
                  (list->set intc '(1 2 3 4)))))

       (test-group "set->list!"
         (test-assert "no elements added"
           (set=? (set intc 1 2 3 4)
                  (list->set! (set intc 1 2 3 4) '())))
         (test-assert "elements added"
           (set=? (set intc 1 2 3 4 5 6)
                  (list->set! (set intc 1 2 3 4) '(5 6))))))

     (test-group "Comparison"
       (test-group "set=?"
         (test-assert "3 sets, true"
           (set=? (set intc 1 2 3) (set intc 2 1 3) (set intc 3 2 1)))
         (test-assert "3 sets, false"
           (not
            (set=? (set intc 1 2 3)
                   (set intc 3 1 2)
                   (set intc 0 2 1)))))

       (test-group "set<?"
         (test-assert "2 sets, true (1)"
           (set<? (set intc) (set intc 1)))
         (test-assert "2 sets, true (2)"
           (set<? (set intc 1) (set intc 1 2 3)))
         (test-assert "3 sets, true"
           (set<? (set intc 1) (set intc 1 2 3) (set intc 1 5 2 3)))
         (test-assert "2 sets, false (1)"
           (not (set<? (set intc 1) (set intc))))
         (test-assert "2 sets, false (2)"
           (not (set<? (set intc 1 2 3) (set intc 1 2 3))))
         (test-assert "2 sets, false (3)"
           (not (set<? (set intc 1 2 3) (set intc 1 2 4))))
         (test-assert "3 sets, false (1)"
           (not
            (set<? (set intc 1 2 3)
                   (set intc 1 2 3 4)
                   (set intc 1 2 4))))
         (test-assert "3 sets, false (2)"
           (not
            (set<? (set intc 1 2 3)
                   (set intc 1 2 3)
                   (set intc 1 2 3 4))))
         (test-assert "3 sets, false (3)"
           (not (set<? (set intc 1 2 3)
                       (set intc 1 2 3 4)
                       (set intc 1 2 3 4)))))

       (test-group "set<=?"
         (test-assert "2 sets, true (1)"
           (set<=? (set intc) (set intc 1)))
         (test-assert "2 sets, true (2)"
           (set<=? (set intc 1) (set intc 1 2 3)))
         (test-assert "2 sets, true (3)"
           (set<=? (set intc 3 1 2) (set intc 1 2 3)))
         (test-assert "3 sets, true (1)"
           (set<=? (set intc 1) (set intc 1) (set intc 1)))
         (test-assert "3 sets, true (2)"
           (set<=? (set intc 1) (set intc 1 2 3) (set intc 1 5 2 3)))
         (test-assert "3 sets, true (3)"
           (set<=? (set intc 1 2 3)
                   (set intc 2 3 1)
                   (set intc 1 5 2 3)))
         (test-assert "2 sets, false (1)"
           (not (set<=? (set intc 1) (set intc))))
         (test-assert "2 sets, false (2)"
           (not (set<=? (set intc 1 2 3) (set intc 1 2 4))))
         (test-assert "3 sets, false"
           (not
            (set<=? (set intc 1 2 3)
                    (set intc 1 2 3 4)
                    (set intc 1 2 4)))))

       (test-group "set>?"
         (test-assert "2 sets, true (1)"
           (set>? (set intc 1) (set intc)))
         (test-assert "2 sets, true (1)"
           (set>? (set intc 1 3 2) (set intc 1)))
         (test-assert "3 sets, true"
           (set>? (set intc 1 2 3 4) (set intc 3 1 2) (set intc 1)))
         (test-assert "2 sets, false (1)"
           (not (set>? (set intc) (set intc 1))))
         (test-assert "2 sets, false (2)"
           (not (set>? (set intc 1 2 3) (set intc 2 1 3))))
         (test-assert "2 sets, false (3)"
           (not (set>? (set intc 1 2 4) (set intc 1 2 3))))
         (test-assert "3 sets, false (1)"
           (not
            (set>? (set intc 1 2 3 4)
                   (set intc 1 2 4)
                   (set intc 1 2 3))))
         (test-assert "3 sets, false (2)"
           (not
            (set>? (set intc 3 1 2) (set intc 3 2 1) (set intc 1 2))))
         (test-assert "3 sets, false (3)"
           (not
            (set>? (set intc 3 1 2 4)
                   (set intc 3 2 1)
                   (set intc 1 2 3)))))

       (test-group "set>=?"
         (test-assert "2 sets, true (1)"
           (set>=? (set intc 1) (set intc)))
         (test-assert "2 sets, true (2)"
           (set>=? (set intc 1 3 2) (set intc 1)))
         (test-assert "2 sets, true (3)"
           (set>=? (set intc 1 3 2) (set intc 1 2 3)))
         (test-assert "3 sets, true (1)"
           (set>=? (set intc 1 2 3 4) (set intc 3 1 2) (set intc 1)))
         (test-assert "3 sets, true (2)"
           (set>=? (set intc 1 2 3 5)
                   (set intc 3 1 2)
                   (set intc 1 2 3)))
         (test-assert "3 sets, true (3)"
           (set>=? (set intc 1) (set intc 1) (set intc 1)))
         (test-assert "2 sets, false (1)"
           (not (set>=? (set intc) (set intc 1))))
         (test-assert "2 sets, false (2)"
           (not (set>=? (set intc 1 2 3) (set intc 1 2 4))))
         (test-assert "3 sets, false (1)"
           (not
            (set>=? (set intc 1 2 3 4)
                    (set intc 1 2 4)
                    (set intc 1 2 3))))
         (test-assert "3 sets, false (2)"
           (not
            (set>=? (set intc 4 1 2)
                    (set intc 3 2 1)
                    (set intc 1 2))))))

     (test-group "Set theory (functional)"
       (test-group "set-union"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-union (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set=? (set intc 1 2 3)
                  (set-union (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set=? (set intc 1 2 3)
                  (set-union (set intc) (set intc 1 2 3))))
         (test-assert "2 sets (3)"
           (set=? (set intc 1 2 3 4)
                  (set-union (set intc 1 3) (set intc 2 4))))
         (test-assert "3 sets (1)"
           (set=? (set intc 1 2 3 4)
                  (set-union (set intc 1 3) (set intc 2) (set intc 4))))
         (test-assert "3 sets (2)"
           (set=? (set intc 1 2 3 4 5)
                  (set-union (set intc 1 3 5)
                             (set intc)
                             (set intc 2 4)))))

       (test-group "set-intersection"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-intersection (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set-empty? (set-intersection (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set-empty? (set-intersection (set intc) (set intc 1 2 3))))
         (test-assert "2 sets (3)"
           (set=? (set intc 2)
                  (set-intersection (set intc 1 2 3) (set intc 2 4 6))))
         (test-assert "3 sets (1)"
           (set=? (set intc 1 2)
                  (set-intersection (set intc 1 2 3)
                                    (set intc 1 2 4)
                                    (set intc 5 1 2 3))))
         (test-assert "3 sets (2)"
           (set-empty?
            (set-intersection (set intc 1 2 3)
                              (set intc)
                              (set intc 1 4 6)))))

       (test-group "set-difference"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-difference (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set=? (set intc 1 2 3)
                  (set-difference (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set=? (set intc 2)
                  (set-difference (set intc 1 2 3) (set intc 1 3 5))))
         (test-assert "2 sets (3)"
           (set=? (set intc 1 3 5)
                  (set-difference (set intc 1 3 5) (set intc 2 4 6))))
         (test-assert "3 sets (1)"
           (set=? (set intc 2)
                  (set-difference (set intc 1 2 3)
                                  (set intc 1 4)
                                  (set intc 3 7))))
         (test-assert "3 sets (2)"
           (set=? (set intc 1 2)
                  (set-difference (set intc 1 2 3)
                                  (set intc)
                                  (set intc 3 7)))))

       (test-group "set-xor"
         (test-assert "set-xor (1)"
           (set=? (set intc 1 2 3)
                  (set-xor (set intc) (set intc 1 2 3))))
         (test-assert "set-xor (2)"
           (set=? (set intc 1 2 3)
                  (set-xor (set intc 1 2 3) (set intc))))
         (test-assert "set-xor (3)"
           (set=? (set intc 5 7 6)
                  (set-xor (set intc 1 4 5 7) (set intc 1 4 6)))))
       )

     (test-group "Set theory (linear update)"
       (test-group "set-union!"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-union! (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set=? (set intc 1 2 3)
                  (set-union! (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set=? (set intc 1 2 3)
                  (set-union! (set intc) (set intc 1 2 3))))
         (test-assert "2 sets (3)"
           (set=? (set intc 1 2 3 4)
                  (set-union! (set intc 1 3) (set intc 2 4))))
         (test-assert "3 sets (1)"
           (set=? (set intc 1 2 3 4)
                  (set-union! (set intc 1 3) (set intc 2) (set intc 4))))
         (test-assert "3 sets (2)"
           (set=? (set intc 1 2 3 4 5)
                  (set-union! (set intc 1 3 5)
                              (set intc)
                              (set intc 2 4)))))

       (test-group "set-intersection!"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-intersection! (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set-empty? (set-intersection! (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set-empty? (set-intersection! (set intc) (set intc 1 2 3))))
         (test-assert "2 sets (3)"
           (set=? (set intc 2)
                  (set-intersection! (set intc 1 2 3) (set intc 2 4 6))))
         (test-assert "3 sets (1)"
           (set=? (set intc 1 2)
                  (set-intersection! (set intc 1 2 3)
                                     (set intc 1 2 4)
                                     (set intc 5 1 2 3))))
         (test-assert "3 sets (2)"
           (set-empty?
            (set-intersection! (set intc 1 2 3)
                               (set intc)
                               (set intc 1 4 6)))))

       (test-group "set-difference!"
         (test-assert "1 set"
           (set=? (set intc 1 2 3)
                  (set-difference! (set intc 1 2 3))))
         (test-assert "2 sets (1)"
           (set=? (set intc 1 2 3)
                  (set-difference! (set intc 1 2 3) (set intc))))
         (test-assert "2 sets (2)"
           (set=? (set intc 2)
                  (set-difference! (set intc 1 2 3) (set intc 1 3 5))))
         (test-assert "2 sets (3)"
           (set=? (set intc 1 3 5)
                  (set-difference! (set intc 1 3 5) (set intc 2 4 6))))
         (test-assert "3 sets (1)"
           (set=? (set intc 2)
                  (set-difference! (set intc 1 2 3)
                                   (set intc 1 4)
                                   (set intc 3 7))))
         (test-assert "3 sets (2)"
           (set=? (set intc 1 2)
                  (set-difference! (set intc 1 2 3)
                                   (set intc)
                                   (set intc 3 7)))))

       (test-group "set-xor!"
         (test-assert "1"
           (set=? (set intc 1 2 3)
                  (set-xor! (set intc) (set intc 1 2 3))))
         (test-assert "2"
           (set=? (set intc 1 2 3)
                  (set-xor! (set intc 1 2 3) (set intc))))
         (test-assert "3"
           (set=? (set intc 5 7 6)
                  (set-xor! (set intc 1 4 5 7) (set intc 1 4 6)))))
       )

     (test-group "set-comparator"
       (test-assert "is a SRFI 128 comparator"
         (s128:comparator? set-comparator))
       (test-assert "type-test predicate is true on sets"
         ((s128:comparator-type-test-predicate set-comparator)
          (set intc 1 2 3)))
       (test-assert "is hashable"
         (s128:comparator-hashable? set-comparator)))

     (test-end))))
