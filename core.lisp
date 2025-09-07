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

(defmacro fresh ((&rest vars) &body goals)
  "Introduce fresh variables `VARS'"
  (unless (and (every #'symbolp vars)
               (every #'listp goals))
    (error "FRESH"))
  (etypecase vars
    (cons
     `(let (,@(mapcar #'(lambda (var)
                          (declare (type symbol var)
                                   (optimize safety))
                          `(,var (make-var :name ',var)))
                      vars))
        (declare (type var ,@vars)
                 (ignorable ,@vars)
                 (optimize safety))
        (conj ,@goals)))
    (null
     `(conj ,@goals))))

(defgoal == (sub u v)
  "Succeed if and only if unification is valid"
  (handler-case (list (unify sub u v))
    (unification-error ())))

(defvar *max-reruns* 0
  "How many times to rerun a query

If `*MAX-RERUNS*' is positive and a hash collision is detected while processing
a query, the query will be rerun (at most `*MAX-RERUNS*' times) and will return
when it succeeds without any hash collisions.  If a hash collision is detected
on every such run, a `SIMPLE-ERROR' will be signaled.")

(defun run-goal (n goal)
  (declare (type fixnum n)
           (type goal goal)
           (optimize safety))
  (do ((extra-runs *max-reruns* (1- extra-runs)))
      ((minusp extra-runs)
       (error "Hash collisions blocked all ~A attempts."
              *max-reruns*))
    (declare (type fixnum extra-runs)
             (optimize safety))
    (handler-case (return (subs-take (funcall goal empty-sub) n))
      (var-collision-condition ()))))

(defmacro run (n query &body goals)
  "Run `QUERY' for the first `N' successful `GOAL's"
  (unless (or (symbolp query)
              (and (listp query)
                   (every #'symbolp query)))
    (error "RUN"))
  (if (symbolp query)
      `(let ((,query (make-var :name ',query)))
         (declare (type var ,query)
                  (optimize safety))
         (mapcar (reify ,query)
                 (run-goal ,n (conj ,@goals))))
      (let ((new-query (gensym)))
        `(run ,n ,new-query
           (fresh (,@query)
             (== `(,,@query) ,new-query) ,@goals)))))

(defmacro run* (query &body goals)
  "Run `QUERY' for all successful `GOAL's"
  `(run -1 ,query ,@goals))

(defmacro conde (&body goals)
  "The disjunction of clause conjunctions

The 'e' in `CONDE' stands for \"every\"."
  (unless (every #'listp goals)
    (error "CONDE"))
  `(disj ,@(mapcar #'(lambda (clause)
                       `(conj ,@clause))
                   goals)))
