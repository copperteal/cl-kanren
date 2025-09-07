;;;; -*- mode: Lisp -*-
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

(defsystem "cl-kanren"
  :description "An implementation of miniKanren in Common Lisp"
  :version "0.0.0"
  :author "J. David Taylor"
  :mailto "jdavidtaylor1@outlook.com"
  :license "GPL-3.0-only"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "var")
               (:file "sub")
               (:file "subs")
               (:file "goal")
               (:file "reify")
               (:file "core")))
