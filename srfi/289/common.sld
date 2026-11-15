;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT

(define-library (srfi 289 common)
  (import (scheme base)
          (srfi 146 hash))
  (export variadic-comparison
          %set
          set?
          %m
          set-m!
          )
  (begin
   ;; Used by bag->set as well as the set library.
   (define-record-type <set>
     (%set hashmap)
     set?
     (hashmap %m set-m!))

   (define (variadic-comparison C get hashmap~?)
     (lambda (s1 . rest)
       (let loop ((s1 s1) (rest rest))
         (if (null? rest)
             #t
             (let ((s2 (car rest)))
               (and (hashmap~? C (get s1) (get s2))
                    (loop s2 (cdr rest))))))))
   ))