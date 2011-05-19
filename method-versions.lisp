;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package #:method-versions)

(defgeneric version-precedence (version)
  (:method ((version (eql nil))) nil))

(defmacro define-method-version (name &optional super)
  `(defmethod version-precedence ((version (eql ',name)))
     '(,name ,@(version-precedence super))))

(defun find-matching-version (aa bb)
  (find-if #'(lambda (mm) (member mm bb)) (version-precedence aa)))

(define-method-combination method-version-method-combination (var)
     ((all *))
  (let* ((versioned (remove-if #'(lambda (mm)
                                   (null (method-qualifiers mm))) all))
         (versions (apply #'append (mapcar #'method-qualifiers versioned))))
    (if (null versioned)
        `(call-method ,(first all))
        `(ecase (find-matching-version ,var ',versions)
           ,@(mapcar #'(lambda (mm)
                         (let ((qq (method-qualifiers mm)))
                           (if qq
                               `(,(first qq) (call-method ,mm))
                               `((nil) (call-method ,mm)))))
                  all)))))
