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

;;; A substitution is an acyclic directed graph such that:
;;; 1. each node is a value,
;;; 2. each edge's source node is a variable,
;;; 3. each node is the source of at most one edge, and
;;; 4. there are no paths from a variable to a value in which it occurs.
;;;
;;; A variable occurs in a value if:
;;; 1. it is equal to that value, or
;;; 2. that value is a container and the variable occurs in at least one of its
;;;    elements.
;;;
;;; An association is an edge in some substitution.
;;;
;;; Substitutions must be immutably extensible.  In particular, given a
;;; substitution S, one must be able to form the substitution S' by adjoining an
;;; edge to S without modifying S.
;;;
;;; Substitutions are encoded as alists in The Reasoned Schemer's miniKanren
;;; implementation.  The pairs encode edges and the stack structure allows
;;; immutable extension.
;;;
;;; A component of a substitution is an equivalence class in its direct limit.
;;; That is, two nodes are equivalent if each has a directed path to a common
;;; node.  Each component has a unique terminal value.
;;;
;;; A variable is fresh in a substitution if its component contains only
;;; variables.  `FRESH' introduces variables with no associations.  All values
;;; in a component are said to be fused.
;;;
;;; Here is a general description of the geometry of a substitution.  Each value
;;; initiates at most one association and terminates any number of associations.
;;; If a value initiates an association, then it must be a variable.  Note that
;;; any component contains at most one non-variable value, which is terminal.
;;;
;;; In `CL-KANREN', a substitution is implemented as an immutably extensible
;;; binary tree -- a `SUB'.  Each node represents an association with `KEY' and
;;; `VALUE' slots, in addition to the obligatory `LEFT' and `RIGHT' children
;;; slots.  Each variable has a (probabilistically) unique hash that is used for
;;; lookup.  The keys are drawn from an (approximately) uniform distribution.
;;; As such, large `SUB' trees will tend to be balanced.  The implementation as
;;; a tree allows looking up an association for a variable in a substitution in
;;; logarithmic rather than linear time.

(defconstant empty-sub
  'empty-sub
  "The empty substitution")

(deftype sub* ()
  '(or (eql empty-sub) sub))

(defstruct sub
  (key 0
       :type fixnum
       :read-only t)
  (value nil
         :read-only t)
  ;; `VAR' is used to detect collisions
  (var (make-var :hash +non-hash+
                 :name 'non-var)
       :type var
       :read-only t )
  ;; `%SIZE' is used for reification
  (%size 1
         :type fixnum)
  (left empty-sub
        :type sub*
        :read-only t)
  (right empty-sub
         :type sub*
         :read-only t))

(defun empty-sub-p (sub)
  "Is `SUB' empty?"
  (declare (type sub* sub)
           (optimize safety))
  (eq sub empty-sub))

(defun sub-size (sub)
  "Get the number of associations in `SUB'"
  (declare (type sub* sub)
           (optimize safety))
  (cond
    ((empty-sub-p sub)
     0)
    ((sub-p sub)
     (sub-%size sub))
    (t
     (error "SUB-SIZE"))))

(define-condition sub-var-error (error)
  ((sub :reader get-sub
        :initarg :sub
        :type sub*)
   (var :reader get-var
        :initarg :var
        :type var)))

(define-condition sub-var-associated-error (sub-var-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "VAR ~A is associated in SUB ~A"
                     (get-var condition)
                     (get-sub condition)))))

(defun sub-extend-blind (sub var value)
  "Adjoin an association to a sub without validity checking

Signal `SUB-VAR-ASSOCIATED-ERROR' if `VAR' is already associated in `SUB'.

Signal `VAR-COLLISION-CONDITION' if a hash collision is detected."
  (declare (type sub* sub)
           (type var var)
           (optimize safety))
  (let ((vh (var-hash var)))
    (declare (type fixnum vh)
             (optimize safety))
    ;; handle `EMPTY-SUB'
    (when (empty-sub-p sub)
      (return-from sub-extend-blind
        (make-sub :key vh
                  :value value
                  :var var)))
    (let ((top-sub sub)
          (sk (sub-key sub))
          (rope nil))
      (declare (type sub* top-sub)
               (type fixnum sk)
               ;; `ROPE' is a stack that holds a `SUB', then the symbol `LEFT'
               ;; or `RIGHT', then repeats the pattern or terminates.
               (type list rope)
               (optimize safety))
      ;; climb the tree, attaching your rope
      (loop
       (cond
         ((< vh sk) ; descend left
          (push 'left rope)
          (push sub rope)
          (setq sub (sub-left sub)))
         ((> vh sk) ; descend right
          (push 'right rope)
          (push sub rope)
          (setq sub (sub-right sub)))
         (t ; (= vh sk)
          (when (eq var (sub-var sub))
            ;; var is already associated in sub
            (error 'sub-var-associated-error
                   :sub top-sub
                   :var var))
          ;; found a hash collision
          (signal 'var-collision-condition
                  :hash vh)
          ;; if the signal is not handled, quietly ignore the collision
          (return-from sub-extend-blind
            #|TODO: Should the next item be `TOP-SUB'?|# sub)))
       ;; exit loop upon finding a leaf
       (when (empty-sub-p sub)
         (return)) ; from the loop
       (setq sk (sub-key sub)))
      ;; make a new leaf
      (setq sub (make-sub :key vh
                          :value value
                          :var var))
      ;; climb back down, detaching your rope
      (loop
       ;; exit loop when done
       (unless rope
         (return sub))
       (let ((prev (pop rope)))
         (declare (type sub* prev)
                  (optimize safety))
         (ecase (pop rope)
           (right ; ascend left
            (setq sub (make-sub :key (sub-key prev)
                                :value (sub-value prev)
                                :var (sub-var prev)
                                :%size (1+ (sub-size prev))
                                :left (sub-left prev)
                                :right sub)))
           (left ; ascend right
            (setq sub (make-sub :key (sub-key prev)
                                :value (sub-value prev)
                                :var (sub-var prev)
                                :%size (1+ (sub-size prev))
                                :left sub
                                :right (sub-right prev))))))))))

(define-condition sub-var-unassociated-error (sub-var-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "VAR ~A is unassociated in SUB ~A"
                     (get-var condition)
                     (get-sub condition)))))

(defun sub-get (sub var)
  "Get the value `SUB' associates to `VAR'

Signal `SUB-VAR-UNASSOCIATED-ERROR' if `VAR' is unassociated.

Signal `VAR-COLLISION-CONDITION' if a hash collision is detected."
  (declare (type sub* sub)
           (type var var)
           (optimize safety))
  (let ((top-sub sub)
        (vh (var-hash var))
        (sk +non-hash+))
    (declare (type sub* top-sub)
             (type fixnum vh sk)
             (optimize safety))
    (loop
     ;; didn't find an association for `VAR'
     (when (empty-sub-p sub)
       (signal 'sub-var-unassociated-error
               :sub top-sub
               :var var))
     (setq sk (sub-key sub))
     (cond
       ((> vh sk) ; descend left
        (setq sub (sub-right sub)))
       ((< vh sk) ; descend right
        (setq sub (sub-left sub)))
       (t ; (= vh sk)
        (unless (eq var (sub-var sub))
          ;; found a hash collision
          (signal 'var-collision-condition
                  :hash vh))
        (return (sub-value sub)))))))

(defun sub-walk (sub var)
  "Get the terminal value of `VAR' in `SUB'

If `VAR' is not a variable or is unassociated in `SUB', return it."
  (declare (type sub* sub)
           (optimize safety))
  (if (var-p var)
      (sub-walk sub
                ;; if `VAR' is unassociated, return it
                (handler-case (sub-get sub var)
                  (sub-var-unassociated-error (condition)
                    (return-from sub-walk (get-var condition)))))
      var))

(defun sub-walk* (sub var)
  "Get the terminal value of `VAR' in `SUB', recursively replace all of its
variables with their terminal values, and return the result."
  (declare (type sub* sub)
           (optimize safety))
  (let ((value (sub-walk sub var)))
    (typecase value
      (var
       value)
      (cons
       (cons
        (sub-walk* sub (car value))
        (sub-walk* sub (cdr value))))
      (vector
       (map 'vector
            #'(lambda (element)
                (sub-walk* sub element))
            value))
      (t value))))

(defun sub-occursp (sub var value)
  "Determine if `VAR' occurs in `VALUE'"
  (declare (type sub* sub)
           (type var var)
           (optimize safety))
  (labels ((helper (value values)
             (declare (type list values)
                      (optimize safety))
             ;; `HELPER' uses proper tail calls and an internal stack of
             ;; `VALUES' to avoid stack overflow when traversing trees of `CONS'
             ;; cells.
             (let ((value (sub-walk sub value)))
               (typecase value
                 (var
                  (or (eql value var)
                      (when values
                        (helper (car values)
                                (cdr values)))))
                 (vector
                  (or (some #'(lambda (element)
                                (sub-occursp sub var element))
                            value)
                      (when values
                        (helper (car values)
                                (cdr values)))))
                 (cons
                  (helper (car value)
                          (cons (cdr value) values)))
                 ;; at this point, we don't care about `VALUE'
                 (t
                  (when values
                    (helper (car values)
                            (cdr values))))))))
    (helper value nil)))

(define-condition sub-occurs-error (sub-var-error)
  ((value :reader get-value
          :initarg :value))
  (:report (lambda (condition stream)
             (format stream
                     "VAR ~A occurs in VALUE ~A in SUB ~A"
                     (get-var condition)
                     (get-value condition)
                     (get-sub condition)))))

(defun sub-extend (sub var value)
  "Adjoin an association to a sub

Signal `SUB-OCCURS-ERROR' if `VAR' occurs in `VALUE'.

Signal `SUB-VAR-ASSOCIATED-ERROR' if `VAR' is already associated in `SUB'.

Signal `VAR-COLLISION-CONDITION' if a hash collision is detected."
  (declare (type sub* sub)
           (type var var)
           (optimize safety))
  (if (sub-occursp sub var value)
      (error 'sub-occurs-error
             :sub sub
             :var var
             :value value)
      (sub-extend-blind sub var value)))
