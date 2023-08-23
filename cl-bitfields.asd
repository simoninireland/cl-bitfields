;; cl-bitfields.asd: ASDF system definition for cl-bitfields
;;
;; Copyright (C) 2023 Simon Dobson
;;
;; This file is part of cl-bitfields, Common Lisp DSL macros for bitfields
;;
;; cl-bitfields is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-bitfields is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-bitfields. If not, see <http://www.gnu.org/licenses/gpl.html>.

(defsystem "cl-bitfields"
  :description "Macros for manipulating bitfields in Common Lisp"
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria")
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "conditions")
	       (:file "bitfields"))
  :in-order-to ((test-op (test-op "cl-bitfields/test"))))

(defsystem "cl-bitfields/test"
  :depends-on ("cl-bitfields" "fiveam")
  :pathname "test/"
  :components ((:file "package")
	       (:file "test-bitfields"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
