;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT

(define-library (srfi 289)
  (include-library-declarations "289/set-exports.scm")
  (include-library-declarations "289/bag-exports.scm")
  (import (scheme base)
          (srfi 289 set)
          (srfi 289 bag)))
