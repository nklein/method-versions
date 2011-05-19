;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

;; load the method-versions library
(require 'method-versions)

;; prepare some versions for the about
(method-versions:define-method-version :latin)
(method-versions:define-method-version :pig-latin :latin)
(method-versions:define-method-version :french)
(method-versions:define-method-version :spanish)

;; prepare a version parameter for our first test program
(declaim (special *language*))
(defparameter *language* nil)

;; declare our generic method to use the method-version method combination
(defgeneric welcome ()
  (:method-combination method-versions:method-version-method-combination
                       *language*))

;; define a variant of our method without a version
(defmethod welcome ()
  :welcome)

;; set up a macro to quickly let us make some versioned about methods
(defmacro defwelcome ((version) &body body)
  `(defmethod welcome ,version ()
     ,@body))

;; make some versioned welcome methods
(defwelcome (:latin)     :velkominum)  ;; someone fire that translator
(defwelcome (:pig-latin) :elcomeway)
(defwelcome (:french)    :bonjour)

;; verify
(let ((greets (mapcar #'(lambda (vv)
                          (let ((*language* vv))
                            (welcome)))
                      '(nil :latin :pig-latin :french :spanish))))
  (format t "Greets: ~S~%" greets)
  (assert (equal greets
                 '(:welcome :velkominum :elcomeway :bonjour :welcome))))

;; prepare some versions for the serialize methods
(method-versions:define-method-version :v1)
(method-versions:define-method-version :v1.1 :v1)
(method-versions:define-method-version :v1.2 :v1.1)
(method-versions:define-method-version :v2   :v1)

;; declare a separate version parameter for serialization functions
(declaim (special *serialization-version*))
(defparameter *serialization-version* nil)

;; declare the serialize method to use the method-version-method-combination
;; with the serialization version parameter just declared
(defgeneric serialize (value)
  (:method-combination method-versions:method-version-method-combination
                       *serialization-version*))

;; set up a function that turns an integer VALUE into a list of a
;; given number of BYTES (in big-endian order).
(defun encode-int (value bytes)
  (loop :for bb :from (1- bytes) :downto 0
     :collecting (ldb (byte 8 (* bb 8)) value)))

;; Originally, we think integers can always fit in 2 bytes and
;; we can just encode strings as strings.
(defmethod serialize ((value integer))
  (encode-int value 2))

(defmethod serialize ((value string))
  value)

;; Then, we realized that we really should have put more
;; information when encoding as a string, like the fact that
;; it is a string and the length of the string.
(defmethod serialize :v1 ((value string))
  (list :string (serialize (length value)) value))

;; Oops, we realized after :v1.0 that we need more than two bytes,
;; so we up it to 4 bytes.
(defmethod serialize :v1.1 ((value integer))
  (encode-int value 4))

;; In :v1.2, we think we can get away without the length of the string
(defmethod serialize :v1.2 ((value string))
  (list :string value))

;; In :v1.2, we came dangerously close to overflowing 4 bytes for
;; an int, so we upped it to 8 bytes for :v2.
(defmethod serialize :v2 ((value integer))
  (encode-int value 8))

;; And, now we test all of our versions
(let ((encoded-strings (mapcar #'(lambda (vv)
                                   (let ((*serialization-version* vv))
                                     (serialize "foo")))
                               '(nil :v1 :v1.1 :v1.2 :v2))))
  (format t "Encoded strings: ~S~%" encoded-strings)
  (assert (equalp encoded-strings
                  '("foo"
                    (:string (0 3) "foo")
                    (:string (0 0 0 3) "foo")
                    (:string "foo")
                    (:string (0 0 0 0 0 0 0 3) "foo")))))
