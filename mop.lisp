;;;-*- Mode: common-lisp; syntax: common-lisp; package: cbr; base: 10 -*-
;;;
;;;; New Memory Organization Package (MOP) Module
;;;
;;; Copyright (c) 2016-2020 Seiji Koide <koide@ontolonomy.co.jp>
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
;; 2016/12/24: file created.

(cl:provide :mop)

(cl:defpackage cbr
  (:use cl)
  (:export #:*application-home* 
           #:mop-absts #:mop-all-absts #:mop-specs #:mop-type
   )
  )

(in-package :cbr)

(defparameter *application-home*
  (make-pathname :name nil
                 :type nil
                 :defaults *load-pathname*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-table (fn vars place)
    (let ((key (car vars))
          (set-fn (gentemp "set-fn."))
          (val (gentemp "val.")))
      `(progn (defun ,fn (,key) (getf ,place ,key))
         (defun ,set-fn (,key ,val)
           (setf (getf ,place ,key) ,val))
         (defsetf ,fn ,set-fn)
         ',fn)))
  )

(defun delete-key (table key)
  (remf table key) table)

(defun table-keys (table)
  (and table
       (cons (car table)
             (table-keys (cddr table)))))

(defvar *mop-tables* nil)
(define-table mop-table (table-name) *mop-tables*)

(define-table mop-absts (mop) (mop-table 'absts))
(define-table mop-all-absts (mop) (mop-table 'all-absts))
(define-table mop-specs (mop) (mop-table 'specs))
(define-table mop-slots (mop) (mop-table 'slots))
(define-table mop-type (mop) (mop-table 'type))

#|
(in-package :cbr)
(progn
(setf (mop-type 'thing) 'mop)
(setf (mop-type 'dwelling) 'mop)
(setf (mop-absts 'dwelling) 'thing)
(setf (mop-type 'apartment) 'mop)
(setf (mop-absts 'apartment)'dwelling)
(setf (mop-type 'apt-at-100-york) 'mop)
(setf (mop-absts 'apt-at-100-york) 'apartment)
(setf (mop-slots 'apt-at-100-york) '((street-name york)))
(setf (mop-slots 'apt-at-100-york) '((street-number 100)))
(setf (mop-slots 'apt-at-100-york) '((wall-color white)))
(setf (mop-slots 'apt-at-100-york) '((floor-surface wood)))
(setf (mop-type 'apt1) 'instance)
(setf (mop-absts 'apt1) 'apt-at-100-york)
(setf (mop-slots 'apt1) '((number-of-rooms 3)))
)

(mopp 'apt1)
(instance-mopp 'apt1)
(mop-absts 'apt1)
(mop-slots 'apt1)
(role-filler 'number-of-rooms 'apt1)
(role-filler 'wall-color 'apt1)
(add-role-filler 'wall-color 'apt1 'green)
|#

(defun mopp (x)
  (or (numberp x) (and (symbolp x) (mop-type x))))

(defun instance-mopp (x)
  (and (mopp x)
       (or (numberp x)
           (eql (mop-type x) 'instance))))

(defun abst-mopp (x)
  (and (mopp x)
       (eql (mop-type x) 'mop)))

(defun abstp (abst spec)
  (or (eql abst spec)
      (member abst (mop-all-absts spec))))

(defun patternp (x) (abstp 'm-pattern x))

(defun groupp (x) (abstp 'm-group x))

(defun slot-role (slot) (car slot))
(defun slot-filler (slot) (cadr slot))
(defun make-slot (role mop) (list role mop))

(defun role-slot (role x)
  (assert (or (mopp x) (listp x)) () "ROLE-SLOT: slot ~S must be a list or a mop." x)
  (assoc role
         (cond ((mopp x) (mop-slots x))
               (t x))))

(defun role-filler (role x)
  (slot-filler (role-slot role x)))

(defun add-role-filler (role mop filler)
  (assert (mopp mop) () "ADD-ROLE-FILLER: ~S given as mop." mop)
  (assert (null (role-filler role mop)) () "ADD-ROLE-FILLER: mop ~S has already a filler of role ~S." mop role)
  (format t "~&~S:~S <= ~S" mop role filler)
  (setf (mop-slots mop)
    (cons (make-slot role filler)
          (mop-slots mop)))
  filler)

(defun calc-all-absts (mop)
  "calculates a list of all the abstractions for <mop>.
   We assume that all the abstractions are correct for the immediate abstractions of <mop>, 
   so all we have to do is get those abstractions, with <mop-all-absts>, add <mop> itself to 
   the front of the list, and remove duplicates."
  (remove-duplicates
   (cons mop
         (loop for abst in (mop-absts mop)
             append (mop-all-absts abst)))))

(defun redo-all-absts (mop)
  (setf (mop-all-absts mop) (calc-all-absts mop))
  (loop for spec in (mop-specs mop)
      do (redo-all-absts spec)))

(defun link-abst (spec abst)
  (assert (abst-mopp abst) () "LINK-ABST: abst ~S must be an abstraction." abst)
  (assert (mopp spec) () "LINK-ABST: spec ~S must be a mop." spec)
  (assert (not (abstp spec abst)) () "LINK-ABST: ~S is already linked to abst ~S." spec abst)
  (cond ((not (abstp abst spec))
         (setf (mop-absts spec)
           (cons abst (mop-absts spec)))
         (setf (mop-specs abst)
           (cons spec (mop-specs abst)))
         (redo-all-absts spec)))
  spec)

(defun unlink-abst (spec abst)
  "removes the abstraction link between the two mops, if any,
   updates the abstraction hierarchy, and returns <spec>."
  (cond ((abstp abst spec)
         (setf (mop-absts spec)
           (remove abst (mop-absts spec)))
         (setf (mop-specs abst)
           (remove spec (mop-specs abst)))
         (redo-all-absts spec)))
  spec)

(defun inherit-filler (role mop)
  (loop for abst in (mop-all-absts mop)
      thereis (role-filler role abst)))

(defun get-filler (role mop)
  (or (role-filler role mop)
      (let ((filler (inherit-filler role mop)))
        (and filler
             (or (and (instance-mopp filler) filler)
                 (and (abstp 'm-function filler) filler)
                 (let ((fn (get-filler 'calc-fn filler)))
                   (and fn
                        (let ((new-filler (funcall fn filler mop)))
                          (and new-filler
                               (add-role-filler role mop new-filler))))))))))

(defun slots-abstp (mop slots)
  (and (abst-mopp mop)
       (not (null (mop-slots mop)))
       (loop for slot in (mop-slots mop)
           always (satisfiedp (slot-filler slot)
                              (get-filler (slot-role slot) slots)
                              slots))))

(defun satisfiedp (constraint filler slots)
  (cond ((null constraint))
        ((patternp constraint)
         (funcall (inherit-filler 'abst-fn constraint)
                  constraint filler slots))
        ((abstp constraint filler))
        ((instance-mopp constraint) (null filler))
        (filler (slots-abstp constraint filler))
        (t nil)))

(defun calc-type (absts slots)
  (or (loop for abst in absts
          when (patternp abst)
          return 'mop)
      (and (null slots) 'mop)
      (loop for slot in slots
          when (not (instance-mopp (slot-filler slot)))
          return 'mop)
      'instance))

(defun spec-name (absts type)
  (gentemp (format nil (cond ((eql type 'mop) "~S.")
                             (t "I-~S."))
             (car absts))))

(defun new-mop (name absts type slots)
  (assert (symbolp name) () "NEW-MOP: name ~S must be a lisp symbol." name)
  (assert (loop for abst in absts always (mopp abst)) () "NEW-MOP: one of absts is not a mop.")
;;;  (and (or (symbolp name) (error "~S failed in ~S" '(symbolp name) 'new-mop))
;;;       (or (for (abst :in absts) :always (mopp abst))
;;;           (error "~S failed in ~S" '(for (abst :in absts) :always (mopp abst)) 'new-mop)))
  (or type (setf type (calc-type absts slots)))
  (or name (setf name (spec-name absts type)))
  (setf (mop-type name) type)
  (and slots (setf (mop-slots name) slots))
  (loop for abst in absts do (link-abst name abst))
  name)

;;;
;;;
;;;

(defun clear-memory ()
  (setf *mop-tables* nil)
  (new-mop 'm-root nil 'mop nil)
  (setf (mop-all-absts 'm-root)
    (calc-all-absts 'm-root))
  'm-root)

(defun all-mops ()
  (table-keys (mop-table 'type)))

(defun remove-mop (name)
  (loop for abst in (mop-absts name)
      do (unlink-abst name abst))
  (loop for table-name in (table-keys *mop-tables*)
      do (setf (mop-table table-name)
           (delete-key (mop-table table-name)
                       name))))

(defun path-filler (path mop)
  (and (loop for role in path
           always (setf mop (get-filler role mop)))
       mop))

(defun mop-includesp (mop1 mop2)
  (and (eql (mop-type mop1) (mop-type mop2))
       (loop for slot in (mop-slots mop2)
           always (eql (slot-filler slot)
                       (get-filler (slot-role slot)
                                   mop1)))
       mop1))

(defun mop-equalp (mop1 mop2)
  (and (mop-includesp mop2 mop1)
       (mop-includesp mop1 mop2)))

(defun get-twin (mop)
  (loop for abst in (mop-absts mop)
      thereis (loop for spec in (mop-specs abst)
                  thereis (when (not (eql spec mop))
                            (mop-equalp spec mop)))))

(defun mops-abstp (mops instance)
  (not (null (loop for mop in mops
                 when (slots-abstp mop instance)
                 collect (link-abst instance mop)))))

(defun refine-instance (instance)
  (loop for abst in (mop-absts instance)
      thereis
        (when (mops-abstp (mop-specs abst) instance)
          (unlink-abst instance abst)
          (refine-instance instance))))

(defun legal-abstp (abst instance)
  (declare (ignore instance))
  (and (mop-slots abst)
       (loop for spec in (mop-specs abst)
           always (instance-mopp spec))))

(defun has-legal-absts-p (instance)
  (loop for abst in (mop-absts instance)
      when (not (legal-abstp abst instance))
      do (unlink-abst instance abst))
  (mop-absts instance))

(defun install-instance (instance)
  (refine-instance instance)
  (let ((twin (get-twin instance)))
    (cond (twin (remove-mop instance) twin)
          ((has-legal-absts-p instance) instance)
          (t (remove-mop instance) nil))))


(defun reindex-siblings (mop)
  (loop for abst in (mop-absts mop)
      do (loop for spec in (mop-specs abst)
             when (and (instance-mopp spec)
                       (slots-abstp mop spec))
             do (unlink-abst spec abst)
               (link-abst spec mop)))
  mop)

(defun install-abstraction (mop)
  (let ((twin (get-twin mop)))
    (cond (twin (remove-mop mop) twin)
          (t (reindex-siblings mop)))))


(defun slots->mop (slots absts must-work)
  (assert (and (not (null absts)) (loop for abst in absts always (mopp abst)))
          () "SLOTS->MOP: Illegal absts given ~S." absts)
  (or (and (null slots) (null (cdr absts)) (car absts))
      (let ((type (and slots (atom (car slots)) (car slots))))
        (and type (setf slots (cdr slots)))
        (let ((mop (new-mop nil absts type slots)))
          (let ((result
                 (cond ((instance-mopp mop)
                        (install-instance mop))
                       (t (install-abstraction mop)))))
            (assert (or result (null must-work)) () "SLOTS->MOP: null result cannot be accepted in this case.")
;;;            (unless result (warn "Null result is obtained in SLOTS->MOP."))
            result)))))

(defmacro defmop (name absts &rest args)
  (let ((type (and args (atom (car args)) (car args))))
    (let ((slot-forms (cond (type (cdr args))
                            (t args))))
      `(new-mop ',name ',absts ',type
                (forms->slots ',slot-forms)))))

(defun forms->slots (slot-forms)
  (loop for slot-form in slot-forms
      collect
        (cond ((atom slot-form) slot-form)
              (t (make-slot (slot-role slot-form)
                            (let ((abst (cadr slot-form)))
                              (assert (atom abst) () "FORMS->SLOTS: illegal abst ~S." abst)
                              (and abst
                                   (slots->mop
                                    (forms->slots (cddr slot-form))
                                    (list abst)
                                    t))))))))

(defun add-instance (name absts slots)
  "creates an instance mop with the given <name>, <abstractions>, 
   and <slots>.  This returns <name>."
  (new-mop name absts 'instance slots))

(defmacro definstance (name absts &rest args)
  "DEFINSTANCE <name> <abstractions> [ (<role> <filler>) ... ]
   creates an instance mop with the given <name>, direct <abstractions>, 
   and slots which are a pair of <role> and <filler>.  <filler> is an atom, 
   a list of slot forms, or a filler form which is a list containing an 
   abstract mop, an optional mop type (mop or instance), and zero or 
   more slot forms.  A slot form is a list of a role and a filler form."
  `(progn
     (assert (loop for abst in ',absts always (abst-mopp abst))
             () "DEFINSTANCE: some of absts ~S is not an abstraction." ',absts)
     (add-instance ',name ',absts ',args)
     ',name))

(defun group-size (x)
  (and (groupp x) (length (mop-slots x))))

(defun make-m-n (m n)
  (assert (integerp m) () "MAKE-M-N: the first parameter is not integer.")
  (assert (integerp n) () "MAKE-M-N: the second parameter is not integer.")
  (cond ((eql m n) (list n))
        ((< m n) (cons m (make-m-n (+ m 1) n)))
        (t (cons m (make-m-n (- m 1) n)))))

(defun group->list (group)
  (and group
       (groupp group)
       (loop for index in (make-m-n 1 (group-size group))
           when (role-filler index group)
           collect it)))

(defun list->group (l)
  (cond ((null l) 'I-m-empty-group)
        (t (slots->mop
            (loop for x in l
                  for i in (make-m-n 1 (length l))
                 collect (make-slot i x))
            '(m-group)
            t))))

(defun tree->list (mop fn visited)
  (cond ((member mop visited) (list mop))
        (t (setf visited (cons mop visited))
           `(,mop ,@(funcall fn mop visited)))))

(defun specs->list (mop visited)
  (loop for spec in (mop-specs mop)
      collect (tree->list spec #'specs->list visited)))

(defun dah (mop)
  (pprint (tree->list mop #'specs->list nil)))

(defun mop->form (mop visited)
  (tree->list mop #'slots->forms visited))

(defun slots->forms (mop visited)
  (loop for slot in (mop-slots mop)
      collect (cons (slot-role slot)
                    (mop->form (slot-filler slot)
                               visited))))

(defun dph (mop)
  (pprint (tree->list mop #'slots->forms nil)))

(defun show-mop (mop)
  `(,(mop-type mop) ,mop ,(mop-absts mop)
      ,@(mop-slots mop)))

(defun show-mop-all-absts (mop)
  (mop-all-absts mop))
