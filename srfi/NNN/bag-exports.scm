;;;; SPDX-FileCopyrightText: 2026 Peter McGoron & Wolfgang Corcoran-Mathe
;;;;
;;;; SPDX-License-Identifier: MIT
(export bag bag-unfold
        bag? bag-contains? bag-empty? bag-disjoint?
        bag-member bag-element-comparator
        bag-adjoin bag-adjoin!
        bag-adjoin-all bag-adjoin-all!
        bag-replace bag-replace!
        bag-delete bag-delete! bag-delete-all bag-delete-all!
        bag-search!
        bag-pop bag-pop!
        bag-size bag-find bag-count bag-any? bag-every?
        bag-map bag-for-each bag-fold
        bag-filter bag-remove bag-partition
        bag-filter! bag-remove! bag-partition!
        bag-copy bag->list list->bag list->bag!
        bag=? bag<? bag>? bag<=? bag>=?
        bag-union bag-intersection bag-difference bag-xor
        bag-union! bag-intersection! bag-difference! bag-xor!
        bag-comparator
        bag-sum bag-sum! bag-product bag-product!
        bag-unique-size bag-element-count bag-for-each-unique
        bag-fold-unique
        bag-increment bag-decrement
        bag-increment! bag-decrement! bag->set set->bag set->bag!
        bag->alist alist->bag)
