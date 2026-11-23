;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT

(define-library (srfi 289 set)
  (include-library-declarations "set-exports.scm")
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 128)
          (srfi 146 hash)
          (only (srfi 151) bitwise-xor)
          (srfi 289 common)
          )
  (begin
   (define (set comparator . els)
     (%set
      (hashmap-unfold null?
                      (lambda (x) (values (car x) (car x)))
                      cdr
                      els
                      comparator)))

   (define (set-unfold stop? mapper successor seed comparator)
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

   (define (set-contains? set el)
     (hashmap-contains? (%m set) el))

   (define (set-empty? set)
     (hashmap-empty? (%m set)))

   (define (set-disjoint? s1 s2)
     (hashmap-disjoint? (%m s1) (%m s2)))

   (define (set-element-comparator set)
     (hashmap-key-comparator (%m set)))

   (define (set-adjoin s . els)
     (set-adjoin-all s els))

   (define (set-adjoin! s . els)
     (set-adjoin-all! s els))

   (define (set-adjoin-all! s els)
     (let loop ((m (%m s))
                (els els))
       (if (null? els)
           (begin
             (set-m! s m)
             s)
           (loop (hashmap-adjoin! m (car els) (car els))
                 (cdr els)))))

   (define (set-adjoin-all s els)
     (let loop ((m (%m s))
                (els els))
       (if (null? els)
           (%set m)
           (loop (hashmap-adjoin m (car els) (car els))
                 (cdr els)))))

   (define (set-delete-all s elements)
     (%set (hashmap-delete-all (%m s) elements)))

   (define (set-delete-all! s elements)
     (set-m! s (hashmap-delete-all (%m s) elements))
     s)

   (define (set-delete s . els)
     (set-delete-all s els))

   (define (set-delete! s . els)
     (set-delete-all! s els))

   (define (set-pop set)
     (when (set-empty? set)
       (error "can't pop empty set" set))
     (let-values (((hmap el _junk) (hashmap-pop (%m set))))
       (values (%set hmap) el)))

   (define (set-pop! set)
     (when (set-empty? set)
       (error "can't pop empty set" set))
     (let-values (((hmap el _junk) (hashmap-pop! (%m set))))
       (set-m! set hmap)
       (values set el)))

   (define (set-size s)
     (hashmap-size (%m s)))

   (define (set-find predicate set failure)
     (call-with-current-continuation
      (lambda (return)
        (set-for-each (lambda (elt)
                        (when (predicate elt)
                          (return elt)))
                      set)
        (failure))))

   (define (set-count predicate set)
     (hashmap-count (lambda (x y) (predicate x)) (%m set)))

   (define (set-every? predicate set)
     (hashmap-every? (lambda (x y) (predicate x)) (%m set)))

   (define (set-any? predicate set)
     (hashmap-any? (lambda (x y) (predicate x)) (%m set)))

   (define (set-map proc comparator set)
     (%set
      (hashmap-map (lambda (key ignored)
                     (let ((new (proc key)))
                       (values new new)))
                   comparator
                   (%m set))))

   (define (set-for-each proc set)
     (hashmap-for-each (lambda (x y) (proc x)) (%m set)))

   (define (set-fold proc nil set)
     (hashmap-fold (lambda (k v nil) (proc k nil)) nil (%m set)))

   (define (set-filter predicate set)
     (%set (hashmap-filter (lambda (x y) (predicate x)) (%m set))))

   ;; FIXME: hashmap-filter! incorrectly returns <undef> in current
   ;; Gauche.
   (define (set-filter! predicate set)
     (set-m! set (hashmap-filter (lambda (x y) (predicate x)) (%m set)))
     set)

   (define (set-remove predicate set)
     (%set (hashmap-remove (lambda (x y) (predicate x)) (%m set))))

   ;; FIXME: hashmap-remove! incorrectly returns <undef> in current
   ;; Gauche.
   (define (set-remove! predicate set)
     (set-m! set (hashmap-remove (lambda (x y) (predicate x)) (%m set)))
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
     (set-unfold null? car cdr lst comparator))

   (define anything-comparator
     (make-comparator (lambda (x) #t)
                      (lambda (x y) #t)
                      #f
                      #f))

   (define set=?
     (variadic-comparison anything-comparator %m hashmap=?))

   (define set<?
     (variadic-comparison anything-comparator %m hashmap<?))

   (define set>?
     (variadic-comparison anything-comparator %m hashmap>?))

   (define set>=?
     (variadic-comparison anything-comparator %m hashmap>=?))

   (define set<=?
     (variadic-comparison anything-comparator %m hashmap<=?))

   (define (variadic-set-operation op)
     (lambda (s1 . rest)
       (let loop ((acc (%m s1)) (rest rest))
         (if (null? rest)
             (%set acc)
             (let ((s (%m (car rest))))
               (loop (op acc s) (cdr rest)))))))

   (define set-union (variadic-set-operation hashmap-union))

   (define set-intersection
     (variadic-set-operation hashmap-intersection))

   (define set-difference (variadic-set-operation hashmap-difference))

   (define (set-xor s1 s2)
     (%set (hashmap-xor (%m s1) (%m s2))))
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

   (define set-intersection!
     (linear-variadic-set-operation hashmap-intersection!))

   (define set-difference!
     (linear-variadic-set-operation hashmap-difference!))

   (define (make-set-comparator elt-comp)
     (make-comparator
      (lambda (x)
        (and (set? x)
             (eqv? (set-element-comparator x) elt-comp)))
      set=?
      #f
      (and (comparator-hashable? elt-comp)
      (lambda (x)
        (let ((hash (comparator-hash-function
                     (set-element-comparator x))))
          (set-fold (lambda (el previous-hash)
                      (+ (bitwise-xor (hash el) previous-hash) 1))
                    0
                    x))))))
   ))
