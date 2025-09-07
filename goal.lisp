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

;;; A goal is a function that maps a substitution to a substitution stream.
;;; A relation is a function that returns a goal.
;;;
;;; `DEFGOAL' and `DEFREL' both define relations.
;;; * `DEFGOAL' gives access to the input `SUB' and allows construction of
;;;   arbitrary goals.
;;; * `DEFREL' does not give access to the input `SUB' and only allows thunks of
;;;   goal conjunctions.
;;;
;;; We say a goal succeeds on a sub when it returns a non-empty `SUBS'.
;;; Otherwise, we say the goal fails.

(defmacro defgoal (name (sub &rest args) &body body)
  "Define the relation `NAME' with lambda list `ARGS'

`SUB' must be a symbol and may be used in `BODY' to refer to the resulting
goal's input argument."
  (declare (type symbol name sub)
           (optimize safety))
  `(defun ,name (,@args)
     #'(lambda (,sub)
         (declare (type sub* ,sub)
                  (optimize safety))
         ,@body)))

(defmacro defrel (name (&rest args) &body goals)
  "Define `NAME' as the conjunction of `GOALS'"
  (declare (type symbol name)
           (optimize safety))
  (let ((sub (gensym)))
    `(defun ,name (,@args)
       #'(lambda (,sub)
           (declare (type sub* ,sub)
                    (optimize safety))
           (thunk
             (funcall (conj ,@goals) ,sub))))))

(defvar succeed
  #'(lambda (sub)
      (declare (type sub* sub)
               (optimize safety))
      (list sub))
  "`SUCCEED' is the goal that always succeeds")

(defvar fail
  #'(lambda (sub)
      (declare (type sub* sub)
               (ignore sub)
               (optimize safety)))
  "`FAIL' is the goal that always fails")

(define-condition unification-error (error)
  ((sub :reader get-sub
        :initarg :sub
        :type sub*)
   (value-1 :reader get-value-1
            :initarg :value-1)
   (value-2 :reader get-value-2
            :initarg :value-2)
   (reason :reader get-reason
           :initarg :reason
           :type string))
  (:report (lambda (condition stream)
             (format stream
                     "~A and ~A cannot be unified in SUB ~A: ~A"
                     (get-value-1 condition)
                     (get-value-2 condition)
                     (get-sub condition)
                     (get-reason condition)))))

(defun %unify-stack-check (sub vs1 vs2)
  ;; this is called iff we no longer care about `V1' or `V2'
  (declare (type sub* sub)
           (type list vs1 vs2)
           (optimize safety))
  (cond
    ((and vs1 vs2) ; non-trivial unification stacks
     (%unify sub
             (car vs1) (car vs2)
             (cdr vs1) (cdr vs2)))
    ((or vs1 vs2) ; unification stack mismatch
     (error 'unification-error
            :reason "They contain mismatched trees."))
    (t ; return the unified `SUB' when done
     sub)))

(defun %unify (sub v1 v2 vs1 vs2)
  (declare (type sub* sub)
           (type list vs1 vs2)
           (optimize safety))
  (let ((v1 (sub-walk sub v1))
        (v2 (sub-walk sub v2)))
    (cond
      ((eql v1 v2)
       (%unify-stack-check sub vs1 vs2))
      ((var-p v1)
       (%unify (sub-extend sub v1 v2)
               (car vs1) (car vs2)
               (cdr vs1) (cdr vs2)))
      ((var-p v2)
       (%unify (sub-extend sub v2 v1)
               (car vs2) (car vs1)
               (cdr vs2) (cdr vs1)))
      ((and (consp v1)
            (consp v2))
       (%unify sub
               (car v1) (car v2)
               (cons (cdr v1) vs1) (cons (cdr v2) vs2)))
      ((and (vectorp v1)
            (vectorp v2))
       (when (/= (length v1) (length v2))
         (error 'unification-error
                :reason (format nil
                                "~A and ~A differ in length."
                                v1 v2)))
       (map nil
            #'(lambda (v1 v2)
                (setq sub (unify sub v1 v2)))
            v1 v2)
       (%unify-stack-check sub vs1 vs2))
      (t
       (error 'unification-error
              :reason (format nil
                              "~A and ~A are a mismatched pair."
                              v1 v2))))))

(defun unify (sub value-1 value-2)
  "Extend `SUB' so that `VALUE-1' and `VALUE-2' are recursively identified

If that's not possible, signal a `UNIFICATION-ERROR'"
  (declare (type sub* sub)
           (optimize safety))
  (handler-case (%unify sub
                        value-1 value-2
                        nil nil)
    ;; We want the final report to contain the original reason and the
    ;; original arguments.
    (unification-error (condition)
      (error 'unification-error
             :sub sub
             :value-1 value-1
             :value-2 value-2
             :reason (get-reason condition)))
    (sub-occurs-error (condition)
      (error 'unification-error
             :sub sub
             :value-1 value-1
             :value-2 value-2
             :reason (format nil "~A" condition)))
    (sub-var-associated-error (condition)
      (error 'unification-error
             :sub sub
             :value-1 value-1
             :value-2 value-2
             :reason (format nil "~A" condition)))))

;;; Suppose that `DISJ' were defined as
;;;
;;; (defmacro disj (&rest goals)
;;;   ((null goals)
;;;    'fail)
;;;   ((singlep goals)
;;;    (car goals))
;;;   (t
;;;    `(disj2 ,(car goals)
;;;            (disj ,@(cdr goals)))))
;;;
;;; for some function `DISJ2' that evenly mixes its two arguments outputs.  Then
;;; for goals `G1', `G2', and `G3', (disj g1 g2 g3) will sample the output of
;;; `G1' twice as often as the output of `G2'.  The same reasoning holds for
;;; `CONJ'.  It is for this reason that `DISJ' and `CONJ' are implemented
;;; directly.

(defmacro disj (&rest goals)
  "Form the logical disjunction of `GOALS'"
  (cond
    ((null goals)
     'fail)
    ((singlep goals)
     (car goals))
    (t
     (let ((sub (gensym)))
       `#'(lambda (,sub)
            (declare (type sub* ,sub)
                     (optimize safety))
            (subs-weave ,@(mapcar #'(lambda (goal)
                                      ;; `GOAL' is an expression that evaluates
                                      ;; to a goal.
                                      (list 'funcall goal sub))
                                  goals)))))))

(defmacro conj (&body goals)
  "Form the logical conjunction of `GOALS'"
  (cond
    ((null goals)
     'succeed)
    ((singlep goals)
     (car goals))
    (t
     (let ((sub (gensym)))
       `#'(lambda (,sub)
            (declare (type sub* ,sub)
                     (optimize safety))
            (reduce #'(lambda (subs goal)
                        (declare (type subs subs)
                                 (type goal goal)
                                 (optimize safety))
                        (mapsubs goal subs))
                    (list ,@goals)
                    :initial-value (list ,sub)))))))
