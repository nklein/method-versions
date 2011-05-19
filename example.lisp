;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

;; load the method-versions library
(require 'method-versions)

;; prepare some versions
(method-versions:define-method-version :v1)
(method-versions:define-method-version :v1.1 :v1)
(method-versions:define-method-version :v1.2 :v1.1)
(method-versions:define-method-version :v2   :v1)

;; prepare a version parameter for our first test program
(declaim (special *about-version*))
(defparameter *about-version* nil)


;; declare our generic method to use the method-version method combination
(defgeneric about ()
  (:method-combination method-versions:method-version-method-combination
                       *about-version*))

;; define a variant of our about method without a version
(defmethod about ()
  :unversioned)

;; set up a macro to quickly let us make some versioned about methods
(defmacro defabout ((version) &body body)
  `(defmethod about ,version ()
     ,@body))

;; make some versioned about methods
(defabout (:v1)   :version-1)
(defabout (:v1.1) :version-1.1)
(defabout (:v2)   :version-2)

;; make sure that the versioned methods work as expected:
;;
;;   :v1   => :version-1
;;   :v1.1 => :version-1.1
;;   :v1.2 => :version-1.1 (because :v1.1 is the most-specific ancestor
;;                          of :v1.2 with the about method defined for it)
;;   :v2   => :version-2
;;
(let ((abouts (mapcar #'(lambda (vv)
                          (let ((*about-version* vv))
                            (about)))
                      '(:v1 :v1.1 :v1.2 :v2 nil))))
  (format t "Abouts: ~S~%" abouts)
  (assert (equal abouts
                 '(:version-1 :version-1.1 :version-1.1
                   :version-2 :unversioned))))

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
