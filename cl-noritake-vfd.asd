;;;; -*- Mode: Lisp -*-

;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2013 Hans Hübner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :cl-user)

(defpackage :cl-noritake-vfd.system
  (:use :cl :asdf))

(in-package :cl-noritake-vfd.system)

(defsystem :cl-noritake-vfd
  :name "cl-noritake-vfd"
  :author "Hans Huebner <hans.huebner@gmail.com>"
  :version "0.0.1"
  :maintainer "Hans Huebner <hans.huebner@gmail.com>"
  :licence "BSD"
  :description "Interface to Noritake VFD"

  :depends-on (:alexandria :flexi-streams :trivial-shell)
  :serial t
  :components ((:file "vfd")
               (:file "examples")))
