;; conditions.lisp: Conditions signalled by bitfield operations
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

(in-package :cl-bitfields)

;; ---------- Information lost condition ----------

(define-condition information-lost ()
  ((var :initarg :variables
	:reader information-lost-from))
  (:documentation "Signalled when information in a variable is not written out in a bitfield pattern."))


(defun informtion-lost-warning (c)
  "Display a warning about loss of information."
  (print (format nil "Information lost in bitfield from variables ~A" (informtion-lost-from c))))
