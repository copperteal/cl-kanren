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

(defun reify-name (n)
  "Convert `N' to a reified variable name"
  (declare (type fixnum n)
           (optimize safety))
  (intern
   (concatenate 'string
                "_"
                (write-to-string n))))

(defun reify-sub (sub var)
  "Extend `SUB' into a substitution that will reify `VAR'"
  (declare (type sub* sub)
           (optimize safety))
  (let ((value (sub-walk sub var)))
    (typecase value
      (var
       (sub-extend-blind sub
                         value
                         (reify-name (sub-size sub))))
      (cons
       (reify-sub (reify-sub sub
                             (car value))
                  (cdr value)))
      (vector
       (reduce #'(lambda (sub element)
                   (declare (type sub* sub)
                            (optimize safety))
                   (reify-sub sub element))
               value
               :initial-value sub))
      (t
       sub))))

(defun reify (var)
  "Reify `VAR'"
  (declare (type var var)
           (optimize safety))
  #'(lambda (sub)
      (declare (type sub* sub)
               (optimize safety))
      (let* ((value (sub-walk* sub var))
             (sub (reify-sub empty-sub value)))
        (declare (type sub* sub)
                 (optimize safety))
        (sub-walk* sub value))))
