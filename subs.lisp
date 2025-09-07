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

;;; A `SUBS' (substitution stream) is
;;; - `NIL',
;;; - a `CONS' with `CAR' a `SUB' and `CDR' a `SUBS', or
;;; - a `THUNK' returning a `SUBS'.
;;;
;;; If a `SUBS' is a `THUNK' it is (unfortunately) possible that no finite
;;; number of applications will return a list.  Note that is it possible to have
;;; an infinite stream and a goal that fails on every substitution it contains.
;;; For example, if the goal `FAIL' is applied to an infinite stream of
;;; `EMPTY-SUB', it will fail on every element.  Now, suppose we have an
;;; arbitrary infinite stream s and a random goal g.  There's no way of knowing
;;; a priori whether g will succeed for some element of s.  If indeed g fails on
;;; every element of s, then regardless of the algorithm for mixing the outputs
;;; of g, no substitutions will ever be returned.

(deftype subs ()
  ;; something is better than nothing
  '(or list function))

;;; `SUBS-WEAVE' is inspired by miniKanren's `APPEND-INF'.
(defun subs-weave (&rest subs-list)
  "Entwine `SUBS-LIST' into a single `subs'

`SUBS-LIST' is a list of `SUBS'."
  (declare (type list subs-list)
           (optimize safety))
  (cond
    ((null subs-list)
     nil)
    ((singlep subs-list)
     (return-from subs-weave (car subs-list)))
    (t
     (let ((subs (car subs-list)))
       (declare (type subs subs)
                (optimize safety))
       (etypecase subs
         (null
          (apply #'subs-weave (cdr subs-list)))
         (cons
          (cons (car subs)
                (thunk
                  (apply #'subs-weave
                         (append (cdr subs-list)
                                 (list (cdr subs)))))))
         (function
          (thunk
            (apply #'subs-weave
                   (append (cdr subs-list)
                           (list (funcall subs)))))))))))

(defun subs-take (subs n)
  "Return the first `N' `SUB's in `SUBS'

If `N' is 0, return the empty list.
If `N' is negative, coerce `SUBS' to a list."
  (declare (type subs subs)
           (type fixnum n)
           (optimize safety))
  (do ((list nil))
      ((or (zerop n)
           (null subs))
       (nreverse list))
    (declare (type list list)
             (optimize safety))
    (etypecase subs
      (cons
       (push (car subs) list)
       (setq subs (cdr subs))
       (decf n))
      (function
       (setq subs (funcall subs))))))

(deftype goal ()
  '(function (sub*) subs))

;;; `MAPSUBS' was inspired by miniKanren's `APPEND-MAP-INF'.
(defun mapsubs (goal subs)
  "Apply `GOAL' to the elements of `SUBS'

Note that each application of `GOAL' to an element of `SUBS' yields a distict
substitution stream.  `MAPSUBS' intertwines these streams into its output."
  (declare (type goal goal)
           (type subs subs)
           (optimize safety))
  (labels ((%mapsubs (subs subs-list subs-cache)
             (declare (type subs subs)
                      ;; `SUBS-LIST' and `SUBS-CACHE' are a lists of `SUBS'
                      (type list subs-list subs-cache)
                      (optimize safety))
             (etypecase subs-list
               (null ; increment row
                (etypecase subs
                  (null ; `SUBS' and `SUBS-LIST' are empty
                   (apply #'subs-weave (reverse subs-cache)))
                  (cons
                   (thunk
                     (%mapsubs (cdr subs)
                               (cons (funcall goal (car subs))
                                     (reverse subs-cache))
                               nil)))
                  (function
                   (thunk
                     (%mapsubs (funcall subs)
                               (reverse subs-cache)
                               nil)))))
               (cons ; increment column
                (let ((car-subs-list (car subs-list)))
                  (declare (type subs car-subs-list)
                           (optimize safety))
                  (etypecase car-subs-list
                    (null ; `CAR-SUBS-LIST' was empty
                     (%mapsubs subs
                               (cdr subs-list)
                               subs-cache))
                    (cons
                     (cons (car car-subs-list)
                           (thunk
                             (%mapsubs subs
                                       (cdr subs-list)
                                       (cons (cdr car-subs-list) subs-cache)))))
                    (function
                     (thunk
                       (%mapsubs subs
                                 (cdr subs-list)
                                 (cons (funcall car-subs-list) subs-cache))))))))))
    (%mapsubs subs nil nil)))
