;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN)
  (include-library-declarations "NNN/set-exports.scm")
  (include-library-declarations "NNN/bag-exports.scm")
  (import (scheme base)
          (srfi NNN set)
          (srfi NNN bag)))
