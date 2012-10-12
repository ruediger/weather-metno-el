;;; weather-metno-query.el --- Query language for weather-metno-el

;; Copyright (C) 2012 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; weather-el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; weather-el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with weather-el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A query language that allows better procession of weather-el data.

;;; Code:

(defun weather-metno-query~split (body)
  "Split BODY at every :get."
  (let (ret current)
    (dolist (i body ret)
      (if (not (and (eq i :get) current))
          (setq current (append current (list i)))
        (setq ret (append ret (list current)))
        (setq current (cons i nil))))
    (append ret (list current))))

(defun weather-metno-query~get-op (op ops)
  "Get OP from OPS."
  (let (flag ret)
    (dolist (i ops)
      (if flag
          (if (keywordp i)
              (return)
            (setq ret (append ret (list i))))
        (when (eq i op)
          (setq flag t)
          (setq ret (cons i nil)))))
    ret))

(defun weather-metno-query~name (ops)
  "Get the :name parameter from OPS."
  (or
   (cadr (weather-metno-query~get-op :name ops))
   (cadr (weather-metno-query~get-op :get ops))))

(defun weather-metno~index (x list)
  "Return the index of X in LIST."
  (let ((r 0))
    (dolist (i list r)
      (if (eq i x)
          (return)
        (setq r (1+ r))))
    r))

(defun weather-metno~op-each (ops entry)
  ""
  (let ((each (cadr (weather-metno-query~get-op :each ops)))
        (symbol (cadr (weather-metno-query~get-op :get ops))))
    (if (not each)
        entry
      (if (functionp each)
          (list 'funcall `(quote ,each) entry)
        (if (listp each)
            (progn
              (car each)
              )
            entry)))))

(defun weather-metno~op-select (ops entry)
  "Select according to OPS from ENTRY.
Implements :select operation."
  (let ((select (cadr (weather-metno-query~get-op :select ops))))
    (if (consp select)
        `(list
          ,@(mapcar
             (lambda (i)
               (weather-metno~op-each
                ops
                `(cdr (assq (quote ,i) (cadr ,entry)))))
             select))
      (if (symbolp select)
          (weather-metno~op-each
           ops
           `(cdr (assq (quote ,select) (cadr ,entry))))
        entry))))

(defun weather-metno-query~merge-cases (lst &optional ret)
  "Merge case statements in LST."
  (if lst
    (weather-metno-query~merge-cases
     (cdr lst)
     (let ((elem (assoc (caar lst) ret)))
       (if elem
          (append (remove elem ret)
                  (list (append elem (cdar lst))))
         (append ret (list (car lst))))))
    ret))

(defmacro weather-metno-query (x &rest body)
  "Queries DATA for values at LOCATION for DATE.

\(fn (DATA LOCATION DATE) BODY...)"

  (let ((data (nth 0 x))
        (location (nth 1 x))
        (date (nth 2 x))
        (body2 (weather-metno-query~split body)))

    `(let (ret)
       (dolist (forecast (cadr (car ,data))) ;; TODO get location
         (let* ((date-range (car forecast))
                (from (car date-range))
                (from-date (weather-metno~time-to-date from))
                (to (cadr date-range))
                (to-date (weather-metno~time-to-date to)))
           (when (and (calendar-date-equal ,date from-date)
                      (calendar-date-equal ,date to-date))
             (dolist (entry (cdr forecast))
               (case (car entry)
                 ,@(weather-metno-query~merge-cases
                    (mapcar
                     (lambda (ops)
                       (let ((symbol (cadr (weather-metno-query~get-op :get ops)))
                             (name (weather-metno-query~name ops))
                             (max (weather-metno-query~get-op :max ops))
                             (min (weather-metno-query~get-op :min ops))
                             (min-max (weather-metno-query~get-op :min-max ops)))
                         `(,symbol
                           (let ((storage (assq (quote ,name) ret))
                                 (value ,(weather-metno~op-select ops 'entry)))
                             (unless storage
                               (setq storage
                                     (cons (quote ,name)
                                           ,(if max
                                                'most-negative-fixnum
                                              (if min
                                                  'most-positive-fixnum
                                                (when min-max
                                                  '(cons most-positive-fixnum
                                                         most-negative-fixnum))))))
                               (setq ret (append ret (list storage))))
                             (setcdr storage
                                     ,(if max
                                          '(max value (cdr storage))
                                        (if min
                                            '(min value (cdr storage))
                                          (if min-max
                                              '(cons (min value (cadr storage))
                                                     (max value (cddr storage)))
                                            '(append (cdr storage)
                                                     (list value))))))))))
                     body2)))))))
       ,@(mapcar
          (lambda (ops)
            (let ((name (weather-metno-query~name ops))
                  (reduce (cadr (weather-metno-query~get-op :reduce ops))))
              (when (functionp reduce)
                `(let ((storage (assq ',name ret)))
                   (when storage
                     (setcdr storage (funcall (quote ,reduce) (cdr storage))))))))
          body2)
       ret)))

(defmacro weather-metno-query~regexp-iterate (x &rest body)
  "Match REGEXP on STRING and call BODY each time.

Inside the body the variable STRING can be accessed.

\(fn (REGEXP STRING) BODY...)"

  (let ((regexp (car x))
        (string (cadr x)))
    `(let ((i 0)
           (string ,string))
       (block loop
         (while (numberp i)
           (setq i (string-match ,regexp ,string (1+ i)))
           (unless (numberp i)
             (return-from loop))
           ,@body)))))

(defun weather-metno-query-format (format data)
  (let ((ret format))
    (weather-metno-query~regexp-iterate
     ("{\\(.*?\\)}" format)

     (let* ((what (match-string 1 string))
            (what-symb (intern what))
            (data (assq what-symb data)))
       (when data
         (setq ret (replace-in-string ret (concat "{" what "}")
                                      (format "%s" (cdr data)))))))
    ret))


(provide 'weather-metno-query)

;;; weather-metno-query.el ends here
