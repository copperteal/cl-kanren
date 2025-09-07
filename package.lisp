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

(defpackage #:cl-kanren
  (:nicknames #:kanren)
  (:use #:cl)
  (:export
   #:fresh #:== #:run #:run* #:conde #:*total-runs*
   #:defgoal #:defrel #:succeed #:fail #:disj #:conj
   #:empty-sub #:empty-sub-p #:sub #:sub* #:sub-extend-blind #:sub-get #:sub-walk #:sub-walk*
   #:sub-occursp #:sub-extend
   #:subs-weave #:subs-take #:mapsubs
   #:var #:make-var #:var-p))
