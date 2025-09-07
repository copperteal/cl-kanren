;;; Copyright (C) 2025 J. David Taylor
;;;
;;; This file is part of CL-KANREN.
;;;
;;; CL-KANREN is free software: you can redistribute it and/or modify it under
;;; the terms of the GNU General Public License as published by the Free
;;; Software Foundation, version 3.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; this program. If not, see <https://www.gnu.org/licenses/>.

(in-package #:cl-kanren)

;;; Each variable's hash is used for insertion and lookup in a binary tree.
;;; Prima facie, it is strange to use a random number as a hash.  However,
;;; retrieving a variable's hash is completely deterministic.  ANSI Common Lisp
;;; mandates that `RANDOM' sample an approximately uniform distribution and that
;;; `MOST-POSITIVE-FIXNUM' be at least 2^15 - 1 (though it tends to be much
;;; higher).
;;;
;;; I considered using `SXHASH', but testing showed that on some implementations
;;; (namely sbcl), (= (sxhash (vector 'a)) (sxhash (vector 'b))) evaluates to
;;; `T'.  Furthermore, ANSI Common Lisp is /very/ lenient in this matter.

(defstruct var
  (hash (random most-positive-fixnum)
        :type fixnum
        :read-only t)
  ;; `NAME' is used for debugging
  (name nil
        :type symbol
        :read-only t))

(defconstant +non-hash+ -1)

(define-condition var-collision-condition ()
  ((hash :reader get-hash
         :initarg :hash
         :type fixnum))
  (:report (lambda (condition stream)
             (format stream
                     "More than one VAR has hash ~A"
                     (get-hash condition)))))
