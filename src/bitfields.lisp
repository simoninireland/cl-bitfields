;; bitfields.lisp -- Bitfield manipulation
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

;; ---------- Helper functions ----------

(defun extract-bits (b w n)
  "Extract W bits starting at B from N, where the least-significant bit is numbered 0"
  (logand (ash n (- (- (1+ b) w)))
	  (- (ash 1 w) 1)))


;; ---------- Code generators ----------

;; TODO Widths less than zero, or non-integer, are always errors.

(defun extract-symbols (l)
  "Extract a list of symbols appearing in L.

This identifies variables (symbols) to be matched against bits.
Elements 0, 1, and - are ignored. A list should contain a symbol name
and a bit width. Any other elements cause an error."
  (if (null l)
      '()
      (let ((syms (extract-symbols (cdr l)))
	    (s (car l)))
	(cond ((member s '(0 1 -))
	       syms)
	      ((symbolp s)
	       (if (member s syms)
		   syms
		   (cons s syms)))
	      ((listp s)
	       (let ((q (car s)))
		 (cond ((not (symbolp q))
			(error "(~S ...) cannot appear in a bitmap pattern" q))
		       ((member q syms)
			syms)
		       (t
			(cons q syms)))))
	      (t
	       (error "~S cannot appear in a bitmap pattern" s))))))


(defun bits-in-pattern (pat)
  "Count the number of bits PAT is trying to match.

If the number of bits can be computed at compile-time (i.e., all the
widths specifiers are constants), return the total number of bits as a
number. Otherwise, return an expression that computes the number at
run-time.

PAT is assumed to be side-effect-free, which means it should first be
passed through `relabel-pattern-width-specifiers` to factor-out
calculated widths into new variable bindings."
  (labels ((count-bits (pat)
	     (let ((p (car pat)))
	       (cond ((null p)
		      (list 0 '()))

		     ;; width specifier
		     ((listp p)
		      (let ((w (cadr p))
			    (rest (count-bits (cdr pat))))
			(if (numberp w)
			    (list (+ (car rest) w) (cadr rest))        ; add to known width
			    (list (car rest) (cons w (cadr rest))))))  ; add to sequence calculation

		     ;; single-bit match
		     (t
		      (let ((rest (count-bits (cdr pat))))
			(list (1+ (car rest)) (cadr rest))))))))
    (let ((bits (count-bits pat)))
      (if (null (cadr bits))
	  ;; no expressions, return the number of bits
	  (car bits)

	  ;; we have expressions, return the number-of-bits calculation
	  (if (equal (car bits) 0)
	      (if (equal (length (cadr bits)) 1)
		  (cadr bits)                      ; a single calculation, return directly
		  `(+  ,@(cadr bits)))             ; a sequence, add them together
	      `(+ ,(car bits) ,@(cadr bits)))))))  ; generally, add the known and sequence


(defun compress-pattern (pat)
  "Compress PAT for more efficient compilation.

This combines adjacent instances of the same variable into a width
specifier, which extracts several bits in one go and so involves less
bit-twiddling.

PAT is assumed to be side-effect-free, which means it should first be
passed through `relabel-pattern-width-specifiers` to factor-out
calculated widths into new variable bindings."
  (if (null pat)
      '()
      (let* ((p (car pat))
	     (r (compress-pattern (cdr pat)))
	     (q (car r)))
	(cond ((member p '(0 1 -))
	       ;; literal bits left unchanged
	       (cons p r))
	      ((listp p)
	       (cond ((equal (cadr p) 0)
		      ;; (x 0) -> deleted
		      r)
		     ((listp q)
		      (if (equal (car p) (car q))
			  ;; (x 3) (x 4) -> (x 7)
			  (if (and (numberp (cadr p))
				   (numberp (cadr q)))
			      ;; both widths known, compute total now
			      (cons (list (car p) (+ (cadr p) (cadr q)))
				    (cdr r))
			      ;; both sizes not known, add computation
			      (cons (list (car p) `(+ ,(cadr p) ,(cadr q)))
				    (cdr r)))
			  ;; (x 3) (y 4) -> unchanged
			  (cons p r)))
		     ((symbolp q)
		      (if (equal (car p) q)
			  ;; (x 3) x -> (x 4)
			  (if (numberp (cadr p))
			      ;; width is known, increment it
			      (cons (list (car p) (1+ (cadr p)))
				    (cdr r))
			      ;; width is not known, add computation
			      (cons (list (car p) `(1+ ,(cadr p)))
				    (cdr r)))
			  ;; (x 3) y -> unchanged
			  (cons p r)))
		     (t
		      (cons p r))))
	      ((symbolp p)
	       (cond ((listp q)
		      (if (equal p (car q))
			  ;; x (x 4) -> (x 5)
			  (if (numberp (cadr q))
			      ;; width is known, increment it
			      (cons (list (car q) (1+ (cadr q)))
				    (cdr r))
			      ;; width is not known, add computation
			      (cons (list (car q) `(1+ ,(cadr q)))
				    (cdr r)))
			  ;; x (y 4) -> unchanged
			  (cons p r)))
		     ((symbolp q)
		      (if (equal p q)
			  ;; x x -> (x 2)
			  (cons (list p 2) (cdr r))
			  ;; x y -> unchanged
			  (cons p r)))
		     (t
		      (cons p r))))
	      (t
	       (cons p r))))))


(defun relabel-pattern-width-specifiers (pat)
  "Rewrite all variable width specifiers in PAT.

A constant width specifier is left unchanged. A computed one is
re-written to a new variable, and a list of definitions is returned
along with the new pattern. This is used to prevent repeated
computation of width expressions that might have side-effects.

Width specifiers consisting of a single variable reference are not
re-written, since such references can't generate side-effects."
  (if (null pat)
      '(() ())
      (let ((p (car pat))
	    (rest (relabel-pattern-width-specifiers (cdr pat))))
	(cond ((listp p)
	       (if (not (numberp (cadr p)))
		   ;; a computed width
		   (if (symbolp (cadr p))
		       ;; a variable reference that can't cause a side effects
		       (list (cons p (car rest))
			     (cadr rest))

		       ;; not a variable reference, re-write to a new variable
		       (let ((nv (gensym)))
			 (list (cons (list (car p) nv)    ; re-written width
				     (car rest))
			       (cons (list nv (cadr p))   ; new variable with width calculaton
				     (cadr rest)))))

		   ;; a fixed width, return unchanged
		   (list (cons p (car rest))
			 (cadr rest))))
	      (t
	       (list (cons p (car rest))
		     (cadr rest)))))))


(defun generate-with-bitfields-destructure (pattern var escape)
  "Generate the code to destructure PATTERN from VAR.

This function is the main code generator for `with-bitfields' that
constructs the list of tests and assignments implied by the pattern. A
list of assignments is returned, with any errors in pattern-matching
at run-time resulting in a nil return from the block designated by
ESCAPE.

PATTERN should be side-effect-free, implying it has been passed
through `relabel-pattern-width-specifiers' to convert any forms into
variables.

Any failures will generate a return to the block designanted ESCAPE."
  (labels ((match-bit (pat bits known computed)
	     (if (null pat)
		 '()
		 (let ((p (car pat)))
		   (if (listp p)
		       ;; width specifier
		       (let ((n (car p))
			     (w (cadr p)))
			 (let ((exs (generate-extract bits w known computed)))
			   (cons `(setq ,n (+ ,(car exs)
					      (ash ,n ,w)))
				 (match-bit (cdr pat) bits (cadr exs) (caddr exs)))))

		       ;; single-bit match
		       (let ((exs (generate-extract bits 1 known computed)))
			 (case p
			   ;; a 0 or 1 matches that bit, or escapes the binding
			   ((0 1)
			    (cons `(if (not (equal ,(car exs) ,p))
				       (return-from ,escape nil))
				  (match-bit (cdr pat) bits (cadr exs) (caddr exs))))

			   ;; a - matches any bit
			   (-
			    (match-bit (cdr pat) bits (cadr exs) (caddr exs)))

			   ;; a symbol binds the bit in that variable
			   (otherwise
			    (cons `(setq ,p (+ ,(car exs)
					       (ash ,p 1)))
				  (match-bit (cdr pat) bits (cadr exs) (caddr exs))))))))))

	   ;; generate the code for the bit offset, collapsing ny zeros
	   ;; or empty lists
	   (generate-offset (bits known computed)
	     (if (null computed)
		 (if (equal known 0)
		     bits
		     `(- ,bits ,known))
		 (if (equal known 0)
		     `(- ,bits ,@computed)
		     `(- ,bits ,known ,@computed))))

	   ;; generate the call to `extract-bits'
	   (generate-extract (bits wanted known computed)
	     (if (numberp bits)
		 ;; known width, implies no computation and all numbers
		 (list `(extract-bits ,(- bits known) ,wanted ,var)
		       (+ known wanted)
		       computed)

		 ;; unknown width
		 (let ((offset (generate-offset bits known computed)))
		   (if (numberp wanted)
		       ;; known number of bits
		       (list `(extract-bits ,offset ,wanted ,var)
			     (+ known wanted)
			     computed)
		       ;; unknown number of bits
		       (list `(extract-bits ,offset ,wanted ,var)
			     known
			     (append computed (list wanted))))))))

    (let* ((bits (bits-in-pattern pattern)))
      (if (numberp bits)
	  ;; number of bits is known at compile-time, use constants
	  (match-bit pattern (1- bits) 0 nil)

	  ;; number of bits must be computed
	  (let ((nob (gensym)))
	    `((let ((,nob (1- ,(if (equal (length bits) 1)
				   (car bits)
				   bits))))
		,@(match-bit pattern nob 0 nil))))))))


(defun extract-relabelling (l)
  "Generate relabellings for the variables in L.

The patterns are the same as for `make-bitfields', with the difference
that - (don't-care) bits aren't allowed.

The relabellings are returned as a list of elements of the form (v nv)
where nv is a new variable taking the value of variable v. The list
acts as an alist to look-up the new names of variables; when the pairs
are reversed, it is suitable for creating the corresponding
let-bindings."
  (if (null l)
      '()
      (let ((syms (extract-relabelling (cdr l)))
	    (s (car l)))
	(cond ((member s '(1 0))
	       ;; explicit bits don't generate bindings
	       syms)
	      ((equal s '-)
	       ;; "don't care" bits not allowed
	       (error "- cannot appear in a make-bitfields pattern"))
	      ((symbolp s)
	       ;; variable references
	       (if (assoc s syms)
		   syms
		   (let ((x (gensym)))
		     (cons (list s x) syms))))
	      ((listp s)
	       ;; width specifiers
	       (if (assoc (car s) syms)
		   syms
		   (let ((x (gensym)))
		     (cons (list (car s) x) syms))))
	      (t
	       (error "~S cannot appear in bitfield pattern" s))))))


(defun generate-make-bitfields-restructure (pattern var)
  "Generate the code required to reconstruct PATTERN.

This is the main code generator for `make-bitfields'. VARS is an alist
mapping program variables to new variables, allowing the variables to
be changed as the bitfield value is built. VAR is the working variable
for the bitfield being built.

PATTERN should be side-effect-free, implying it has been passed
through `relabel-pattern-width-specifiers' to convert any forms into
variables."
  (labels ((make-bit (pat consumed vars)
	     (if (null pat)
		 '()
		 (let* ((p (car pat))
			(exs (make-bit (cdr pat) consumed vars)))
		   (if (listp p)
		       ;; width specifier
		       (let* ((n (car p))
			      (nv (lookup-new-variable n vars))
			      (w (cadr p))

			      ;; optimise the cases where the widths are known
			      (mask (if (numberp w)
					(1- (ash 1 w))
					`(1- (ash 1 ,w))))
			      (shift (if (numberp w)
					 (- w)
					 `(- ,w))))
			 (append exs (list `(setq ,var (logior ,var
							       (ash (logand ,nv ,mask) ,consumed)))
					   `(setq ,nv (ash ,nv ,shift))
					   `(setq ,consumed (+ ,consumed ,w)))))

		       ;; single-bit match
		       (case p
			 ;; 0 simply shifts the number of known bits
			 (0
			  (append exs
				  (list `(setq ,consumed (1+ ,consumed)))))

			 ;; one
			 (1
			  (append exs
				  (list `(setq ,var (logior ,var (ash 1 ,consumed)))
					`(setq ,consumed (1+ ,consumed)))))

			 ;; symbol has bit extracted
			 (otherwise
			  (let ((nv (cadr (assoc p vars))))
			    (append exs
				    (list `(setq ,var (logior ,var
							      (ash  (logand ,nv 1) ,consumed)))
					  `(setq ,nv (ash ,nv -1))
					  `(setq ,consumed (1+ ,consumed)))))))))))

	   ;; look-up the new corresponding to the old one
	   (lookup-new-variable (n vars)
	     (cadr (assoc n vars))))

    (let* ((variables (extract-relabelling pattern))
	   (let-bindings (mapcar (lambda (p) (list (cadr p) (car p)))
				variables))
	   (consumed (gensym)))
      `(let (,@let-bindings
	     (,consumed 0))
	 ,@(make-bit pattern consumed variables)))))


;; ---------- with-bitfields  ----------

(defmacro with-bitfields (pattern n &rest body)
  "Bind variables in PATTERN to the bits of N within BODY.

PATTERN is interpreted as a bitfield pattern with each element
corrsponding to a bit. The last element of the list corresponds to the
rightmpost (least significant) bit of N.

Each element of PATTERN is one of 0, 1, -, a list, or a symbol. 0 and
1 must match with 0 or 1 in the corrsponding bit of N. - matches
either 0 or 1 (the bit is ignored).

A symbol will be declared in a let binding and have the corresponding
bit bound to it. If the same symbol appears several times in PATTERN
then it receives all the bits in the obvious order. The bits taken
into a variable don't have to be consecutive.

A list element should take the form (x w) where x is a symbol and w is
a number. This creates a symbol with the name x that consumes w bits
from the pattern. As with symbols binding single bits, the same symbol
can appear in several width specifiers, or alone to bind a single bit.

The width specifier can be a constant, a variable reference, or an
arbitrary form that returns a number. Forms are evaluated in order and
only once, so side-effects occur as expected.

For example, the pattern '(x x x 0), when matched against the number
10 (#2r1010), will bind x to 5 (#2r101), the bits in the corresponding
positions. The same pattern matched against 11
(#2r1011) will fail because the rightmost bits of the number and the
pattern don't match. The following patterns are all the same:

   - '(x x x 0)
   - '((x 3) 0)
   - (let ((w 3)) '((x w) 0))
   - (let ((w 2)) '((x w) x 0)
   - (let ((w 2)) '((w (+1 w)) 0)

WITH-BITFIELDS returns the value of executing BODY in an
environment extended by the extracted variables (if any), or nil if
PATTERN doesn't match N."
  (let* ((syms (extract-symbols pattern))
	 (let-bindings (mapcar #'(lambda (s) (list s 0))
			       syms))
	 (relabels (relabel-pattern-width-specifiers pattern))
	 (cpattern (compress-pattern (car relabels)))
	 (escape (gensym))
	 (var (gensym))
	 (decls-and-tests (generate-with-bitfields-destructure cpattern var escape))
	 (matcher `(let (,@let-bindings
			 (,var ,n))
		     ,@decls-and-tests
		     ,@body))
	 (widths (if (null (cadr relabels))
		     matcher
		     `(let ,(cadr relabels)
			,matcher))))
    `(block ,escape
       ,widths)))


;; ---------- make-bitfields ----------

(defmacro make-bitfields (pattern)
  "Construct a value according to the bitfield PATTERN.

The variables references in PATTERN should all be in scope. Literal 1
and 0 symbols are mapped to those bits. Variables may appear several
times, and their bits are extrated and placed into the value in the
expected order.

Width specifiers appear as pairs (x w) where x is a variable name and
w a width in bits, which may be a constant, a variable, or a form.

For example, if x = 10 (#2r1010), the pattern '(x x x 0) creates the
value 4 (#2r100)."
  (let* ((relabels (relabel-pattern-width-specifiers pattern))
	 (cpattern (compress-pattern (car relabels)))
	 (result (gensym))
	 (decls-and-shifts (generate-make-bitfields-restructure cpattern result))
	 (matcher `(let ((,result 0))
		     ,decls-and-shifts
		     ,result))
	 (widths (if (null (cadr relabels))
		     matcher
		     `(let ,(cadr relabels)
			,matcher))))
    widths))


;; ---------- setf-bitfields ----------

(defmacro setf-bitfields (v pattern)
  "Set the place identified by V to the bitfields constructed from PATTERN.

This is shorthand for '(setf V (make-bitfields pattern))'."
  `(setf ,v (make-bitfields ,pattern)))


;; ---------- with-bitfields-f ----------

(defmacro with-bitfields-f (pattern n &rest body)
  "Destructure N according to pattern, then re-create it from values changed by BODY.

N must designate a place suitable to be assigned to by `setf'. The
value of N is first destructured using PATTERN (as used in
`with-bindings', creating variables that are in scope for the forms in
BODY. Oce all the BODY forms have been executed, N is re-created using
PATTERN in the manner of `make-bitfields'.

If N cannot be destructured according to PATTERN, `with-bitfields-f'
returns nil. Otherwise it returns the value of the last BODY form."
  (let* ((syms (extract-symbols pattern))
	 (let-bindings (mapcar #'(lambda (s) (list s 0))
			       syms))
	 (relabels (relabel-pattern-width-specifiers pattern))
	 (cpattern (compress-pattern (car relabels)))
	 (escape (gensym))
	 (var (gensym))
	 (decls-and-tests (generate-with-bitfields-destructure cpattern var escape))
	 (result (gensym))
	 (update (gensym))
	 (decls-and-shifts (generate-make-bitfields-restructure cpattern update))
	 (matcher `(let (,@let-bindings
			 (,var ,n))
		     ,@decls-and-tests
		     (let ((,result (progn
				      ,@body))
			   (,update 0))
		       ,decls-and-shifts
		       (setf ,n ,update)
		       ,result)))
	 (widths (if (null (cadr relabels))
		     matcher
		     `(let ,(cadr relabels)
			,matcher))))
    `(block ,escape
       ,widths)))
