;;;-*- Mode: common-lisp; syntax: common-lisp; package: cbr; base: 10 -*-
;;;
;;;; New Memory Organization Package (MOP) Judge Module
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
;;;   Micro Judge module for new MOP system
;;; ====================================================================
;;; Modified by Seiji Koide

;;;
;;; JUDGE Basic MOPs
;;;

;;; Actors

(definstance I-m-Al (m-actor))     ; AL is a m-actor.
(definstance I-m-Chuck (m-actor))  ; CHUCK is a m-actor.
(definstance I-m-David (m-actor))  ; DAVID is a m-actor.
(definstance I-m-Randy (m-actor))  ; RANDY is a m-actor.
(definstance I-m-Ted (m-actor))    ; TED is a m-actor.
(definstance I-m-Tim (m-actor))    ; TIM is a m-actor.

;;; Frequency

(defmop m-frequency (m-root) (severity nil))  ; m-frequency has a severity. 
(definstance I-m-once (m-frequency) (severity 0)) ; ONCE is a m-frequency and has 0. 
(definstance I-m-several-times (m-frequency) (severity 1)) ; SEVERAL-TIMES has 1.
(definstance I-m-repeatedly (m-frequency) (severity 2)) ; REPEATEDLY has 2 as :severity.

;;; Motives

(defmop m-motive (m-root))               ; There is a m-motive.
(defmop m-justified (m-motive))          ; m-justified is a kind of m-motive.
(defmop m-unjustified (m-motive))        ; m-unjustified is a kind of m-motive.
(definstance I-m-self-defence (m-justified)) ; SELF-DEFENCE is a m-justified.
(definstance I-m-retaliation (m-unjustified)); RETALIATION is a m-unjustified.
(definstance I-m-unprovoked (m-unjustified)) ; UNPROVOKED is a m-unjustified.

;;; Crime

(defmop m-crime-type (m-root))        ; There is a m-crime-type.
(definstance I-m-homicide (m-crime-type)) ; HOMICIDE is a kind of m-crime-type.

;;; Range constraint

(defmop range-constraint (constraint-fn)) ; range-constraint is a 
(defmop m-range (m-pattern)               ; constraint about m-range.
  (abst-fn range-constraint))

(defun range-constraint (constraint filler slots)
  (declare (ignore slots))
  (and (numberp filler)
       (let ((below (role-filler 'below constraint))
             (above (role-filler 'above constraint)))
         (and (or (null below) (< filler below))
              (or (null above) (< above filler))))))

;;; Events

;; m-fight-act is a kind of m-act and has a :severity.
;; m-hurt-act is a kind of m-fight-act and has a :severity below 5.
;; I-m-slap is a m-hurt-act and has :severity 1.
;; I-m-hit is a m-hurt-act and has :severity 1.
;; I-m-strike is a m-hurt-act and has :severity 2.
;; I-m-knock-down is a m-hurt-act and has :severity 3.
;; I-m-slash is a m-hurt-act and has :severity 4.
(defmop m-fight-act (m-act) (severity nil))
(defmop m-hurt-act (m-fight-act) (severity m-range (below 5)))
(definstance I-m-slap (m-hurt-act) (severity 1))
(definstance I-m-hit (m-hurt-act) (severity 1))
(definstance I-m-strike (m-hurt-act) (severity 2))
(definstance I-m-knock-down (m-hurt-act) (severity 3))
(definstance I-m-slash (m-hurt-act) (severity 4))

;; m-wound-act is a kind of m-fight-act and has a :severity above 4.
;; STAB is a m-wound-act and has :severity 5.
;; SHOOT is a m-wound-act and has :severity 5.
;; BREAK-SKULL is a m-wound-act and has :severity 5.
(defmop m-wound-act (m-fight-act) (severity m-range (above 4)))
(definstance I-m-stab (m-wound-act) (severity 5))
(definstance I-m-shoot (m-wound-act) (severity 5))
(definstance I-m-break-skull (m-wound-act) (severity 5))

;; There is a m-state.
;; m-phys-state is a kind of m-state and has a severity.
;; I-m-bruised is a m-state and has severity 1.
;; I-m-knocked-down is a m-state and has severity 2.
;; I-m-cut is a m-state and has severity 3.
;; I-m-dead is a m-state and has severity 5.
(defmop m-state (m-root))
(defmop m-phys-state (m-state) (severity nil))
(definstance I-m-bruised (m-phys-state) (severity 1))
(definstance I-m-knocked-down (m-phys-state) (severity 2))
(definstance I-m-cut (m-phys-state) (severity 3))
(definstance I-m-dead (m-phys-state) (severity 5))

(defmop m-state (m-root))
(defmop m-phys-state (m-state) (severity nil))
(defmop I-m-bruised (m-phys-state) (severity 1))
(defmop I-m-knocked-down (m-phys-state) (severity 2))
(defmop I-m-cut (m-phys-state) (severity 3))
(defmop I-m-dead (m-phys-state) (severity 5))

;; There is an m-outcome.
;; m-fight-outcome is a kind of m-outcome
;;   and has m-phys-state as state and m-actor as actor.
(defmop m-outcome (m-root))
(defmop m-fight-outcome (m-outcome)
  (state m-phys-state)
  (actor m-actor))

;; m-fight-event is a kind of m-event
;;   and has a m-fight-act as action.
(defmop m-fight-event (m-event)
  (action m-fight-act))

;; m-event-sequence is a kind of m-sequence about events.
;; m-outcome-sequence is a kind of m-sequence about outcomes.
;; m-escalation-sequence is a kind of m-sequence about ranges.
;; m-motive-sequence is a kind of m-sequence about motives.
(defmop m-event-group (m-group) (1 m-event))
(defmop m-outcome-group (m-group) (1 m-outcome))
(defmop m-escalation-group (m-group) (1 m-range))
(defmop m-motive-group (m-group) (1 m-motive))

(defmop calc-escalations (m-function))
(defmop calc-motives (m-function))
(defmop adapt-sentence (m-function))
(defmop calc-sentence (m-function))

;; m-crime is a kind of m-case
;;   and has a m-crime-type, an m-actor as defendant,
;;   an m-actor as victim, an m-event-group as events, 
;;   an m-outcome-group as outcomes, 
;;   and escalations, motives and sentence are calculated.
(defmop m-crime (m-case)
  (crime-type m-crime-type)
  (defendant m-actor)
  (victim m-actor)
  (events m-event-group)
  (outcomes m-outcome-group)
  (escalations m-pattern (calc-fn calc-escalations))
  (motives m-pattern (calc-fn calc-motives))
  (sentence m-pattern (calc-fn adapt-sentence)))

;;;
;;; Calculation
;;;

(defun calc-escalations (pattern mop)
  "takes the sequence of events in <mop> and calculates a sequence of
   corresponding escalations"
  (declare (ignore pattern))
  (format t "~&---------------")
  (format t "~&Calculating escalations in ~S" mop)
  (list->group
   (let ((prev-severity 0))
     (loop for event in (group->list (role-filler 'events mop))
       collect (let ((this-severity (path-filler '(action severity) event)))
                  (let ((result (- this-severity prev-severity)))
                    (setf prev-severity this-severity)
                    result))))))

(defun calc-motives (pattern mop)
  "takes the sequence of events in <mop> and calculates a sequence of
   corresponding motives."
  (declare (ignore pattern))
  (format t "~&---------------")
  (format t "~&Calculating motives in ~S" mop)
  (list->group
   (let ((prev-motive 0))
     (loop for escalation in (group->list (get-filler 'escalations mop))
       collect (setf prev-motive
                     (mop-calc `((role motive)
                                 (escalation ,escalation)
                                 (prev-motive ,prev-motive))))))))

;; slots like ((role sentence) (index ...) (old-sentence ...)
;;             (old-action ...) (old-motive ...) (old-severity ...)
;;             (this-action ..) (this-motive ..) (this-severity ..))
;; is input in the adaptation.
(defun mop-calc (slots)
  "finds the specialization of m-calc with the given slots and
   returns its value filler."
  (let ((instance (slots->mop slots '(m-calc) nil)))
    (and instance (get-filler 'value instance))))

(definstance motive (m-role))

(defmop m-calc (m-root))

(defmop m-calc-motive (m-calc)
  (role motive)
  (value nil))

;; If the role is motive, and the escalation is above 0,
;; then it is m-calc-escalation-motive, and its value is I-m-retaliation.
(defmop m-calc-escalation-motive (m-calc-motive)
  (escalation m-range (above 0))
  (value I-m-retaliation))

;; If the role is motive, and the escalation is above 1,
;; and prev-motive is UNJUSTIFIED,
;; then it is m-calc-self-defence-motive, and its value is I-m-self-defence.
(defmop m-calc-self-defence-motive (m-calc-motive)
  (escalation m-range (below 1))
  (prev-motive m-unjustified)
  (value I-m-self-defence))

;; If the role is motive, and the escalation is below 1,
;; and prev-motive is JUSTIFIED,
;; then it is m-calc-retaliation-motive, and its value is I-m-retaliation.
(defmop m-calc-retaliation-motive (m-calc-motive)
  (escalation m-range (below 1))
  (prev-motive m-justified)
  (value I-m-retaliation))

(defun adjust-sentence (pattern mop)
  "is called by the sentence calculation MOPs to adjust the sentence
   appropriately. The slots in <mop> contain the old sentence, a
   wieghing factor (between 0.00 and 0.50) that says how critical
   the difference is, an index (between 0 and the number of events
   in the shortest fight) that says how close the difference was to
   the final act, and a direction (1 or -1) that says which case is
   worse, the old one(-1) or the new one(1)."
  (declare (ignore pattern))
  (format t "~&---------------")
  (format t "~&~S applied, ~S events from the end" mop (get-filler 'index mop))
  (adjust-fn
   (get-filler 'old-sentence mop)
   (get-filler 'weight mop)
   (get-filler 'index mop)
   (get-filler 'direction mop)))

(defun adjust-fn (x y index direction)
  "ADJUST-FN <sentence> <weight> <index> <direction>
   determines the value of the new sentence, based on the old sentence,
   using the formula
       sentence + (sentence x (weight + closeness) x direction)
   where closeness is 0.25 if <index> is 0 or 1, i.e., the last or
   sencond last events differed, or 0.00 if <index> >= 1."
  (+ x  (* x
           (+ y (cond ((< index 2) 0.25)
                      (t 0.0)))
           direction)))

(defun adapt-sentence (pattern mop)
  "gets another case with the same immediate abstraction as <mop>,
   and compares the actions, motives, and outcomes in the two cases,
   working backwards, until a difference is found and an adjusted
   sentence is calculated."
  (declare (ignore pattern))
  (let ((old-mop (get-filler 'old mop)))
    (format t "~%ADAPT-SENTENCE: old-mop ~S" old-mop)
    (let ((old-size (group-size (get-filler 'events old-mop)))
          (size (group-size (get-filler 'events mop)))
          (old-sentence (get-filler 'sentence old-mop)))
      (format t "~&--------------")
      (format t "~&Adapting the sentence in ~S" old-mop)
      (or (loop for old-pos downfrom old-size to 1
                for pos downfrom size to 1
            thereis (mop-calc
                      `((role sentence) (index ,(- size pos))
                        (old-sentence ,old-sentence)
                        ,@(crime-compare-slots old-mop old-pos
                           `(old-action old-motive old-severity))
                        ,@(crime-compare-slots mop pos
                           `(this-action this-motive this-severity)))))
          (progn (format t "~&--------------------")
                 (format t "~&No major difference found")
                 (format t "~&Using old sentence")
                 old-sentence)))))

(defun crime-compare-slots (mop pos roles)
  "returns a list of slots, where the first role in <role-list> is
   filled with the :action of the nth-last event, the second role
   with the nth-last motive, and the third role with the severity
   of the nth-last outcome."
  (let ((paths `((events ,pos action)
                 (motives ,pos)
                 (outcomes ,pos state severity))))
    (assert (eql (length roles) (length paths)) ()
      "CRIME-COMPARE-SLOT: illegal length of roles ~S" roles)
    (loop for role in roles
          for path in paths
      collect (make-slot role (path-filler path mop)))))

;;;
;;; Comparison MOPs
;;;

(defmop compare-constraint (constraint-fn))

(defmop m-compare (m-pattern)                ; define generic function
  ;; You should know that (to ...) slot is top level in slots.
  (abst-fn compare-constraint)               ; method for generic
  (to m-role)
  (compare-fn m-function))                   ; hands-on function

(defmop eql (m-function))
(defmop < (m-function))

(defmop m-equal (m-compare) (compare-fn eql))
(defmop m-less-than (m-compare) (compare-fn <))

(defun compare-constraint (constraint filler slots)
  "applies the comparison function in <constraint> to <filler> and the filler 
   of <role> in <slots>, where <role> is role that fills the to slot in 
   <constraint>."
  (funcall (get-filler 'compare-fn constraint)
           filler
           (indirect-filler 'to constraint slots)))

(defun indirect-filler (role mop slots)
  "gets the filler of <role> in <mop>, which should be a role, and 
   gets the filler of that role in <slots>."
  (get-filler (get-filler role mop) slots))

;;;
;;; Micro JUDGE MOPs for Adaptation
;;;

(definstance sentence (m-role))
(definstance old-severity (m-role))   ; needed becase of (to old-severity)
(definstance this-severity (m-role))  ; needed becase of (to this-severity)


(defmop adjust-sentence (m-function))

(defmop m-adapt-sentence (m-calc)
  (role sentence)
  (value m-pattern (calc-fn adjust-sentence)))

;;;
;;; Micro JUDGE Force and Motive Adaptation MOPs
;;;
;;; The most important thing is if every old-xxxx and this-xxxx
;;; is same each other, it does not match anything of the followings.

;; If old-action is m-wound-act, and this-action is not m-wound-act,
;; and old-motive is m-unjustified, and this-motive is m-unjustified, too,
;; then the m-adapt-sentence is m-adapt-extreme-force-old, 
;; and the older is worse extremely.
(defmop m-adapt-extreme-force-old (m-adapt-sentence)
  (old-action m-wound-act)
  (this-action m-not (object m-wound-act))
  (old-motive m-unjustified)
  (this-motive m-unjustified)
  (weight 0.50)
  (direction -1))

;; If old-action is not m-wound-act, this-action is also m-wound-act,
;; old-motive is m-unjustified, and this-motive is m-unjustified, too,
;; then the m-adapt-sentence is m-adapt-extreme-force-new, 
;; and the newer is worse extremely.
(defmop m-adapt-extreme-force-new (m-adapt-sentence)
  (old-action m-not (object m-wound-act))
  (this-action m-wound-act)
  (old-motive m-unjustified)
  (this-motive m-unjustified)
  (weight 0.50)
  (direction 1))

;; For any old-severity, 
;; if this-severity is equal to the old-severity,
;; and old-motive is m-unjustified, and this-motive is m-justified,
;; then the m-adapt-sentence is m-adapt-worse-motive-old, 
;; and the older is worse.
(defmop m-adapt-worse-motive-old (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-equal (to old-severity))
  (old-motive m-unjustified)
  (this-motive m-justified)
  (weight 0.25)
  (direction -1))

;; For any old-severity, 
;; if this-severity is equal to old-severity,
;; and old-motive is m-justified, and this-motive is m-unjustified,
;; then the m-adapt-sentence is m-adapt-worse-motive-new, 
;; and the newer is worse.
(defmop m-adapt-worse-motive-new (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-equal (to old-severity))
  (old-motive m-justified)
  (this-motive m-unjustified)
  (weight 0.25)
  (direction 1))

;;;
;;; Micro JUDGE Mixed Comparison Adaptation MOPs
;;;

;; For any old-severity, 
;; if this-severity is less than old-severity,
;; and old-motive is m-justified, and this-motive is m-unjustified,
;; then the m-adapt-sentence is m-adapt-mixed-old, 
;; and the older is not so bad.
(defmop m-adapt-mixed-old (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-less-than (to old-severity))
  (old-motive m-justified)
  (this-motive m-unjustified)
  (weight 0.00)
  (direction -1))

;; For any this-severity, 
;; if old-severity is less than this-severity,
;; and old-motive is m-unjustified, and this-motive is m-justified,
;; then the m-adapt-sentence is m-adapt-mixed-new, 
;; and the older is not so bad.
(defmop m-adapt-mixed-new (m-adapt-sentence)
  (this-severity nil)
  (old-severity m-less-than (to this-severity))
  (old-motive m-unjustified)
  (this-motive m-justified)
  (weight 0.00)
  (direction -1))

;;;
;;; Demonstration
;;;

(defparameter *case1*
  '((crime-type I-m-homicide)
    (defendant I-m-Ted) (victim I-m-Al)
    (events m-group
            (1 m-fight-event
             (action I-m-slash)
             (actor I-m-Ted) (object I-m-Al)
             (freq I-m-once))
            (2 m-fight-event
             (action I-m-slash)
             (actor I-m-Al) (object I-m-Ted)
             (freq I-m-once))
            (3 m-fight-event
             (action I-m-stab)
             (actor I-m-Ted) (object I-m-Al)
             (freq I-m-repeatedly)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-cut) (actor I-m-Al))
              (2 m-fight-outcome
               (state I-m-cut) (actor I-m-Ted))
              (3 m-fight-outcome
               (state I-m-dead) (actor I-m-Al)))
    (sentence 40)))

(defparameter *case2*
  '((crime-type I-m-homicide)
    (defendant I-m-Randy) (victim I-m-Chuck)
    (events m-group
            (1 m-fight-event
             (action I-m-strike)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-repeatedly))
            (2 m-fight-event
             (action I-m-strike)
             (actor I-m-Chuck) (object I-m-Randy)
             (freq I-m-repeatedly))
            (3 m-fight-event
             (action I-m-slash)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-once))
            (4 m-fight-event
             (action I-m-slash)
             (actor I-m-Chuck) (object I-m-Randy)
             (freq I-m-once))
            (5 m-fight-event
             (action I-m-stab)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-repeatedly)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-bruised) (actor I-m-Chuck))
              (2 m-fight-outcome
               (state I-m-bruised) (actor I-m-Randy))
              (3 m-fight-outcome
               (state I-m-cut) (actor I-m-Chuck))
              (4 m-fight-outcome
               (state I-m-cut) (actor I-m-Randy))
              (5 m-fight-outcome
               (state I-m-dead) (actor I-m-Chuck)))))

(defparameter *case3*
  '((crime-type I-m-homicide)
    (defendant I-m-Tim) (victim I-m-David)
    (events m-group
            (1 m-fight-event
             (action I-m-slap)
             (actor I-m-David) (object I-m-Tim)
             (freq I-m-several-times))
            (2 m-fight-event
             (action I-m-strike)
             (actor I-m-Tim) (object I-m-David)
             (freq I-m-several-times))
            (3 m-fight-event
             (action I-m-knock-down)
             (actor I-m-David) (object I-m-Tim)
             (freq I-m-once))
            (4 m-fight-event
             (action I-m-stab)
             (actor I-m-Tim) (object I-m-David)
             (freq I-m-several-times)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-bruised) (actor I-m-Tim))
              (2 m-fight-outcome
               (state I-m-bruised) (actor I-m-David))
              (3 m-fight-outcome
               (state I-m-knocked-down) (actor I-m-Tim))
              (4 m-fight-outcome
               (state I-m-dead) (actor I-m-David)))))

(defun judge-demo ()
  (run-judge *case1* '*case1*)
  (run-judge *case2* '*case2*)
  (run-judge *case3* '*case3*))

(defun run-judge (case case-name)
  (format t "~&---------------")
  (format t "~&Sentencing ~S in ~S"
            (role-filler 'defendant case) case-name)
  (let ((instance (judge (forms->slots case))))
    (assert (not (null instance)) () "RUN-JUDGE: null instance.")
    (format t "~&Sentence in ~S is ~S years"
              instance (role-filler 'sentence instance))
    instance))

(defun judge (slots)
  "JUDGE <slot-list>
   finds or creates a case under M-CRIME with the given slots
   and returns it."
  (let ((instance (slots->mop slots '(m-crime) t)))
    (and (get-filler 'sentence instance)
         instance)))

;;; To demonstrate, type "judge-demo"
