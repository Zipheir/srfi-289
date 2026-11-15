;;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT
(import (scheme base)
        (srfi 64)
        (prefix (test runner) tr:)
        (prefix (test set) sets:)
        (prefix (test bag) bags:)
        )

(test-runner-factory tr:custom-test-runner-factory)

(test-begin "Sets & bags")
(sets:run-tests)
(bags:run-tests)
(test-end)
