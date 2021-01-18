;;;-*- Mode: common-lisp; syntax: common-lisp; package: cbr; base: 10 -*-
;;;
;;;; New Memory Organization Package (MOP) Build Package
;;;
;;; Copyright (c) 2016 Seiji Koide <koide@ontolonomy.co.jp>
;;; This software is provided under the FreeBSD lisence, excepting 
;;; the part of coded by Christopher K. Riesbeck and Roger C. Schank in 
;;; "INSIDE CASE-BASED REASONING" 1989, LEA.
;;;
;;; ------------------------------------------------------------------------------
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met: 
;;; 
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer. 
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution. 
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; The views and conclusions contained in the software and documentation are those
;;; of the authors and should not be interpreted as representing official policies, 
;;; either expressed or implied, of the FreeBSD Project.
;;; ------------------------------------------------------------------------------
;; --- history ---
;; 2016/12/27: file created.

(cl:defpackage cbr
  (:use cl)
  (:export #:*application-home* 
           #:mop-absts #:mop-all-absts #:mop-specs #:mop-type
   )
  )

(in-package :cbr)

;;; ====================================================================
;;;   Build up module as stub between old MOP and new MOP modules
;;; ====================================================================
;;; Modified by Seiji Koide

;;; Mop Building
;;; --------------------------------------------------------------------
;;;
;;; Basic MOPs
;;;

;; Before creating following MOPs, you need to install m-root using clear-memory.

(clear-memory)

(defmop m-event (m-root))
(defmop m-state (m-root))
(defmop m-act (m-root))
(defmop m-actor (m-root))

;;;
;;; Constraint Function
;;;
;;; A constraint is a pattern abstract that has :abst-fn slot.

(defmop m-function (m-root))
(defmop constraint-fn (m-function))

(defmop m-pattern (m-root)
  (abst-fn constraint-fn))
(defmop not-constraint (constraint-fn))

(defun constraint-fn (constraint filler slots)
  (declare (ignore constraint filler slots))
  "always returns true, that is, there is no constraint on <filler>."
  t)

(defun not-constraint (constraint filler slots)
  "is true if <filler> does not satisfy the constraint found in the
   <object> slot of <constraint>."
  (assert (not (null filler)) () "NOT-CONSTRAINT: The filler is null.")
  (not (satisfiedp (get-filler 'object constraint)
                   filler slots)))

;;;
;;; Getting Deamon
;;;
;;; A getting deamon is a pattern abstract that has :calc-fn slot.

(defmop get-sibling (m-function))

(defmop m-role (m-root))
(defmop m-case (m-root)
  (old m-pattern (calc-fn get-sibling)))

(defmop m-not (m-pattern)
  (abst-fn not-constraint))
(defmop m-failed-solution (m-root))

(defun get-sibling (pattern mop)
  (declare (ignore pattern))
  "finds a sibling of <mop>. This function don't look for instance mops,
   and instances of <m-failed-solution>."
  (loop for abst in (mop-absts mop)
        thereis
        (loop for spec in (mop-specs abst)
              thereis
              (when (and (instance-mopp spec)
                         (not (eql spec mop))
                         (not (abstp 'm-failed-solution spec)))
                spec))))

;;;
;;; Sequence
;;;
;;; You can't make instance directly under m-sequence,
;;; because an instance of sequence can't be made under any slotless abstract.
;;; You have to define a mediate special of sequence for creating instances.

(defmop m-sequence (m-root))
(defmop m-empty-sequence (m-sequence))
(definstance I-m-empty-sequence (m-empty-sequence))

;;; --------------------------------------------------------------------
;;;  Sequence MOP

(defun sequencep (x)
  "returns true if <mop> is a special mop of m-sequence.
   Sequence mops are used to hold time sequences of MOPs."
  (abstp 'm-sequence x))

(defun list->sequence (l)
  "returns a sequence mop with members from <list>. The first element of 
   <list> fills the first role, the second fills the second role, and so on. 
   If the list is empty, the instance mop I-M-EMPTY-SEQUENCE is returned."
  (if (null l) 'I-m-empty-sequence
      (slots->mop
       (loop for x in l
             for i from 1 to (length l)
             collect (make-slot i x))
       '(m-sequence)
       t)))

(defun sequence-size (x)
  "returns the size of the sequence."
  (and (sequencep x) (length (mop-slots x))))

(defun sequence->list (sequence)
  "returns a list of the members of the sequence, or the filler of first
   role, of second role, and so on."
  (and sequence
       (progn (assert (sequencep sequence) () "SEQUENCE->LIST: illegal MOP.") t)
       (loop for index from 1 to (sequence-size sequence)
             when (role-filler index sequence)
             collect it)))

(defun sequence-member (mop sequence)
  "returns true if <mop> is a member of <sequence>."
  (and (sequencep sequence)
       (loop for slot in (mop-slots sequence)
            thereis (eql (slot-filler slot) mop))))

(defun sequence-splice (new old sequence)
  "returns a new sequence mop with all the elements of <sequence>, except 
   that <mop> is replaced with the elements of <mop-list>.  Note that a NIL 
   <mop-list> returns a sequence with <mop> removed."
  (list->sequence
   (loop for mop in (sequence->list sequence)
        append (cond ((eql mop old) new)
                      (t (list mop))))))

(defun sequence-insert (mop sequence)
  "returns a new sequence mop with all the elements of <sequence> plus <mop>,
   added at the end."
  (cond ((null mop) sequence)
        ((sequence-member mop sequence) sequence)
        (t (list->sequence (append (sequence->list sequence) (list mop))))))

;;;
;;; Group
;;;

(defmop m-group (m-root))
(defmop m-empty-group (m-group))
(defmop I-m-empty-group (m-empty-group) instance)

#|
;;;
;;; Setting and Adding Deamon
;;;
;;; A setting deamon is a pattern abstract that has set-fn slot.
;;; A adding deamon is a pattern abstract that has add-fn slot.

(defun put-filler (mop role filler)
  "puts the <filler> to the <role> slot of <mop>, eventhough the <role> slot
   exists or not.  <filler> is returned.  If the <role> has a putting or adding
   deamon in the abstracts of <mop>, it works.  When the slot newly added to 
   the <mop>, a message to help you keep track of what's going on is printed."
  (assert (mopp mop) () "~%PUT-FILLER: ~S is not a MOP." mop)
  (let ((deamon (most-specific-inherited-filler mop role))
        (slot (role-slot mop role)))
    (cond (slot (setf (slot-filler slot) filler)
                (when (patternp deamon)
                  (let ((fn (get-filler 'set-fn deamon)))
                    (when fn (funcall fn mop deamon)))))
          (t (add-role-filler mop role filler)
             (when (patternp deamon)
               (let ((fn (get-filler 'add-fn deamon)))
                 (when fn (funcall fn mop deamon))))))
    filler))

;;;
;;; Deamon when a mop newly created
;;;
;;; A creating deamon is a pattern abstract that has :new-fn slot.

;;;
;;; Customized Functions
;;;

(defun calc-all-siblings (mop pattern)
  (declare (ignore pattern))
  "CALC-ALL-SIBLINGS <mop> <pattern>
   returns all siblings of <mop>, except <mop>"
  (let ((siblings
         (loop for abst in (mop-absts mop)
               append (loop for spec in (mop-specs abst)
                            append (when (and (not (eq mop spec))
                                              (eq (mop-type mop) (mop-type spec))
                                              (not (abstp 'm-failed-solution spec)))
                                     (list spec))))))
    (loop for sibling in siblings
          do (delete-filler sibling ':all-siblings))
    siblings))

(defun sibling-p (mop1 mop2)
  "SIBLING-P <mop1> <mop2>
   returns true if <mop1> and <mop2> are siblings."
  (member mop1 (get-filler mop2 ':all-siblings)))

(defun shared-absts (mop1 mop2)
  "SHARED-ABSTS <mop1> <mop2>
   returns shared abstractions between <mop1> and <mop2>."
  (when (sibling-p mop1 mop2)
    (intersection (mop-absts mop1) (mop-absts mop2))))

(defun delete-filler (mop role)
  "DELETE-ROLE-FILLER <mop> <role>
   deletes the slot (<role> filler) from the slots of <mop>,
   if exists, and returns the slot. This function prints a 
   message to help you keep track of what's going on.
   If <slot> does not exist, nothing done, and returns NIL."
  (assert (mopp mop) ())
  (let ((slot (role-slot mop role)))
    (when slot
      (setf (mop-slots mop)
            (delete-slot slot (mop-slots mop)))
      (format t "~&~S:~S deleted." mop role)
      slot)))

(defun delete-slot (slot slots)
  (when slots
    (if (eq slot (car slots))
      (cdr slots)
      (rplacd slots (delete-slot slot (cdr slots))))))

(defun sequence-add (time data sequence)
  (push `(,time ,data) (mop-slots sequence)))
|#
;;; End of module
;;; --------------------------------------------------------------------