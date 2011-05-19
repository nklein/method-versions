;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

;; load the ContextL library
(require 'contextl)

;; prepare some layers for the welcom
(contextl:deflayer :latin)
(contextl:deflayer :pig-latin (:latin))
(contextl:deflayer :french)
(contextl:deflayer :spanish)

;; declare our method
(contextl:define-layered-function welcome ())

;; define a variant of our method without a version
(contextl:define-layered-method welcome ()
  :welcome)

;; set up a macro to quickly let us make some versioned about methods
(defmacro defwelcome ((version) &body body)
  `(contextl:define-layered-method welcome :in ,version ()
     ,@body))

;; make some versioned welcome methods
(defwelcome (:latin)     :velkominum)  ;; someone fire that translator
(defwelcome (:pig-latin) :elcomeway)
(defwelcome (:french)    :bonjour)

;; define a little helper macro because the contextl:with-active-layers
;; macro quotes things we don't want to have quoted
(defmacro with-layer ((layer) &body body)
  `(unwind-protect
       (progn
         (when ,layer (contextl:ensure-active-layer ,layer))
         ,@body)
     (when ,layer (contextl:ensure-inactive-layer ,layer))))

;; verify
(let ((greets (mapcar #'(lambda (vv)
                          (with-layer (vv)
                            (welcome)))
                      '(nil :latin :pig-latin :french :spanish))))
  (format t "Greets: ~S~%" greets)
  (assert (equal greets
                 '(:welcome :velkominum :elcomeway :bonjour :welcome))))

;; prepare some versions for the serialize methods
(contextl:deflayer :v1)
(contextl:deflayer :v1.1 (:v1))
(contextl:deflayer :v1.2 (:v1.1))
(contextl:deflayer :v2   (:v1))

;; declare the serialize method
(contextl:define-layered-function serialize (value))

;; set up a function that turns an integer VALUE into a list of a
;; given number of BYTES (in big-endian order).
(defun encode-int (value bytes)
  (loop :for bb :from (1- bytes) :downto 0
     :collecting (ldb (byte 8 (* bb 8)) value)))

;; Originally, we think integers can always fit in 2 bytes and
;; we can just encode strings as strings.
(contextl:define-layered-method serialize ((value integer))
  (encode-int value 2))

(contextl:define-layered-method serialize ((value string))
  value)

;; Then, we realized that we really should have put more
;; information when encoding as a string, like the fact that
;; it is a string and the length of the string.
(contextl:define-layered-method serialize :in :v1 ((value string))
  (list :string (serialize (length value)) value))

;; Oops, we realized after :v1.0 that we need more than two bytes,
;; so we up it to 4 bytes.
(contextl:define-layered-method serialize :in :v1.1 ((value integer))
  (encode-int value 4))

;; In :v1.2, we think we can get away without the length of the string
(contextl:define-layered-method serialize :in :v1.2 ((value string))
  (list :string value))

;; In :v1.2, we came dangerously close to overflowing 4 bytes for
;; an int, so we upped it to 8 bytes for :v2.
(contextl:define-layered-method serialize :in :v2 ((value integer))
  (encode-int value 8))

;; And, now we test all of our versions
(let ((encoded-strings (mapcar #'(lambda (vv)
                                   (with-layer (vv)
                                     (serialize "foo")))
                               '(nil :v1 :v1.1 :v1.2 :v2))))
  (format t "Encoded strings: ~S~%" encoded-strings)
  (assert (equalp encoded-strings
                  '("foo"
                    (:string (0 3) "foo")
                    (:string (0 0 0 3) "foo")
                    (:string "foo")
                    (:string (0 0 0 0 0 0 0 3) "foo")))))
