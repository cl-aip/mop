;;;-*- Mode: common-lisp; syntax: common-lisp; package: nmop; base: 10 -*-
;;;
;;;; New Memory Organization Package (New MOP) DMAP Module
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
;; 2016/12/30: file created.

(cl:defpackage newmop
  (:nicknames nmop)
  (:use cl)
  (:export 
   )
  )

(in-package nmop)

(define-table mop-preds (mop) (mop-table 'preds))

(defun predicted-mops ()
  (table-keys (mop-table 'preds)))

(defvar *pred-slots-table* nil)
(define-table pred-slots (pred) *pred-slots-table*)

(defvar *predictions* nil)
(define-table pos-preds (pos) *predictions*)

(defvar *sent-pos* 1)

(defun dmap-init ()
  (setf *predictions* nil)
  (setf *pred-slots-table* nil)
  (setf *sent-pos* 1))

(defun pred-filler (role pred)
  (role-filler role (pred-slots pred)))

(defun add-pred-filler (role pred value)
  (assert (mopp value) () "ADD-PRED-FILLER: value ~S given is not a mop." value)
  (assert (null (pred-filler role pred)) () "ADD-PRED-FILLER: prediction for ~S already exists." role)
  (setf (pred-slots pred)
    (cons (make-slot role value)
          (pred-slots pred)))
  pred)

(defun pred-target (pred) (first pred))
(defun pred-phrase (pred) (second pred))
(defun pred-base (pred) (third pred))
(defun pred-start (pred) (fourth pred))

(defun make-pred (phrase base slots start)
  (let ((target (get-target phrase base)))
    (assert (or (null phrase) target) () "MAKE-PRED: target does not exist.")
    (let ((pred (list target phrase base start)))
      (setf (pred-slots pred) slots)
      pred)))

(defun get-target (phrase base)
  (cond ((null phrase) nil)
        ((role-specifierp (car phrase))
         (get-filler (role-specifier-role (car phrase))
                     base))
        (t (car phrase))))

(defun role-specifierp (x) (listp x))
(defun role-specifier-role (x) (car x))

(defun get-triggered-preds (mop start)
  (append (mop-dynamic-predictions mop start)
          (mop-default-predictions mop)))

(defun mop-dynamic-predictions (mop start)
  (loop for pred in (pos-preds start)
      when (refersp (pred-target pred) mop)
      collect pred))

(defun refersp (mop1 mop2)
  (or (abstp mop1 mop2) (abstp mop2 mop1)))

(defun mop-default-predictions (mop)
  (loop for target in (predicted-mops)
      when (abstp target mop)
      append (loop for pred in (mop-preds target)
                 collect pred)))

(defun advance-pred (pred mop)
  (let ((phrase (pred-phrase pred)))
    (let ((new-pred (make-pred (cdr phrase)
                               (pred-base pred)
                               (pred-slots pred)
                               (or (pred-start pred)
                                   *sent-pos*))))
      (cond ((role-specifierp (car phrase))
             (add-pred-filler
              (role-specifier-role (car phrase))
              new-pred mop)))
      (cond ((null (cdr phrase))
             (pred->mop new-pred))
            (t (let ((next-pos (+ *sent-pos* 1)))
                 (format t "~&Adding ~S to ~S" new-pred (pos-preds next-pos))
                 (setf (pos-preds next-pos)
                   (cons new-pred
                         (pos-preds next-pos)))))))))

(defun pred->mop (pred)
  (let ((mop (slots->mop (pred-slots pred)
                         (list (pred-base pred))
                         t))
        (start (pred-start pred)))
    (format t "~&Activating ~S" mop)
    (loop for next-pred in (get-triggered-preds mop start)
        do (advance-pred next-pred mop))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defphrase (mop &rest phrase)
    `(let ((pred (make-pred ',phrase ',mop nil nil)))
       (let ((target (pred-target pred)))
         (setf (mop-preds target)
           (cons pred (mop-preds target)))
         ',phrase)))
  )

(defun dp (n)
  (display-preds (pos-preds n))
  nil)

(defun dap ()
  (loop for n in (table-keys *predictions*)
      do (format t "~&Predictions on Position ~S" n)
        (display-preds (pos-preds n)))
  nil)

(defun ddp ()
  (loop for mop in (predicted-mops)
      do (display-preds (mop-preds mop)))
  nil)

(defun display-preds (preds)
  (let ((i 0))
    (loop for pred in preds
        do (format t "~&~S: ~S ==> ~S in ~S"
             (setf i (+ i 1))
             (pred-target pred)
             (car (pred-phrase pred))
             (pred-base pred)))))

(defmop m-actor (m-root))
(defmop m-economist (m-actor))
(defmop m-monetarist (m-economist))
(defmop I-m-Friedman (m-monetarist) instance)
(defmop m-supply-sider (m-economist))
(defmop I-m-Laffer (m-supply-sider) instance)

(defmop m-act (m-root))
(defmop I-m-mtrans-act (m-act) instance)

(defmop m-variable (m-root))
(defmop m-bad-var (m-variable))
(defmop m-neutral-var (m-variable))
(defmop I-m-interest-rates (m-bad-var) instance)
(defmop I-m-money-supply (m-neutral-var) instance)

(defmop m-behavior (m-root))
(defmop I-m-increase (m-behavior) instance)
(defmop I-m-decrease (m-behavior) instance)

(defmop m-state-change (m-root))

(defmop m-vtrans (m-state-change)
  (var m-variable) (beh m-behavior))

(defmop m-good-vtrans (m-vtrans)
  (var m-variable))
(defmop m-bad-vtrans (m-vtrans)
  (var m-variable))
(defmop m-neutral-vtrans (m-vtrans)
  (var m-neutral-var))

(defmop m-bad-down (m-good-vtrans)
  (var m-bad-var) (beh I-m-decrease))
(defmop m-bad-up (m-bad-vtrans)
  (var m-bad-var) (beh I-m-increase))

(defmop I-m-ms-up (m-neutral-vtrans)
  (var I-m-money-supply))

(defmop m-causal (m-root)
  (ante m-state-change) (cnsq m-state-change))
(defmop m-causal/econ (m-causal)
  (ante m-vtrans) (cnsq m-vtrans))
(defmop m-causal/mon (m-causal/econ)
  (ante I-m-ms-up) (cnsq m-bad-vtrans))

(defmop m-mobject (m-root)
  (content nil))
(defmop m-mobject/state-change (m-mobject)
  (content m-state-change))
(defmop m-arg (m-mobject)
  (content m-causal))
(defmop m-arg/econ (m-arg)
  (content m-causal/econ))
(defmop m-arg/mon (m-arg/econ)
  (content m-causal/mon))

(defmop m-mtrans (m-event)
  (action I-m-mtrans-act) (actor m-actor)
  (info m-mobject))

(defmop m-mtrans/state-change (m-mtrans)
  (info m-mobject/state-change))

(defmop m-mtrans/econ (m-mtrans)
  (actor m-economist)
  (info m-arg (content m-causal/econ)))

(defphrase I-m-Friedman Milton Friedman)
(defphrase I-m-Laffer Arthur Laffer)

(defphrase I-m-interest-rates interest rates)
(defphrase I-m-money-supply money supply)

(defphrase I-m-increase rise)
(defphrase I-m-increase increase)
(defphrase I-m-increase go up)
(defphrase I-m-decrease drop)
(defphrase I-m-decrease decrease)
(defphrase I-m-decrease go down)

(defphrase m-vtrans (var) will (beh))

(defphrase I-m-ms-up monetary explosion)

(defphrase m-causal (cnsq) as a consequence of (ante))
(defphrase m-causal (cnsq) because of (ante))
(defphrase m-causal (ante) can cause (cnsq))
(defphrase m-causal if (ante) then (cnsq))

(defphrase m-mobject/state-change (content))
(defphrase m-arg (content))

(defphrase m-mtrans (actor) says (info) period)

(defvar *sent1*)
(defvar *sent2*)

(defun dmap-demo ()
  (format t "~&----------------")
  (dmap-init)
  (setq *sent1*
    '(Milton Friedman says interest rates will rise because of the monetary explosion period))
  (format t "~&Parsing ~S" *sent1*)
  (dmap *sent1*)
  (format t "~&----------------")
  (dmap-init)
  (setq *sent2*
    '(Arthur Laffer says interest rates will drop period))
  (format t "~&Parsing ~S" *sent2*)
  (dmap *sent2*)
  nil)

(defun dmap (word-list)
  (loop for word in word-list
      do (format t "~&~%Reading ~S" word)
        (let ((preds (get-triggered-preds word *sent-pos*)))
          (cond (preds
                 (loop for pred in preds
                     do (advance-pred pred word))
                 (setf *sent-pos* (+ *sent-pos* 1))))))
  (dp (+ *sent-pos* 1)))