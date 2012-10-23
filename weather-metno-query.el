;;; weather-metno-query.el --- Query language for weather-metno-el

;; Copyright (C) 2012 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; URL: https://github.com/ruediger/weather-metno-el
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

(eval-when-compile
  (require 'cl))

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

LOCATION is currently ignored.  BODY contains query language instructions:

- :get NAME ... Gets entries with NAME and starts an instruction set.
- :select PARAMETER ... Select PARAMETER from entry.
- :name RESULT-NAME ... Store values with RESULT-NAME.
- :each FUNC ... Call FUNC on each selected parameter.
- :max ... Store max and time of max in RESULT-NAME-time.
- :min ... Store min and time of min in RESULT-NAME-time.
- :reduce FUNC ... call FUNC with accumulated results as parameter.

Each set of query instructions begins with a `:get' instruction!

Example:

:get temperature :name temperature-max :select value :each string-to-number :max
:get temperature :name temperature-min :select value :each string-to-number :min
:get temperature :name temperature-avg :select value :each string-to-number
    :reduce avg

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
                       (let* ((symbol (cadr (weather-metno-query~get-op :get ops)))
                              (name (weather-metno-query~name ops))
                              (time-name (intern (concat (symbol-name name)
                                                         "-time")))
                              (max (weather-metno-query~get-op :max ops))
                              (min (weather-metno-query~get-op :min ops)))
                         `(,symbol
                           (let ((storage (assq (quote ,name) ret))
                                 (time-storage
                                  ,(when (or max min)
                                     `(assq ',time-name ret)))
                                 (value ,(weather-metno~op-select ops 'entry)))
                             (unless storage
                               (setq storage
                                     (cons (quote ,name)
                                           ,(if max
                                                'most-negative-fixnum
                                              (when min
                                                  'most-positive-fixnum))))
                               (setq ret (append ret (list storage))))
                             ,(when (or max min)
                                `(unless time-storage
                                   (setq time-storage (cons ',time-name nil))
                                   (setq ret (append ret (list time-storage)))))
                             (setcdr storage
                                     ,(if max
                                          '(if (< value (cdr storage))
                                               (cdr storage)
                                             (setcdr time-storage (list from))
                                             value)
                                        (if min
                                            '(if (> value (cdr storage))
                                                 (cdr storage)
                                               (setcdr time-storage (list from))
                                               value)
                                          '(append (cdr storage)
                                                   (list value)))))))))
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
    `(let ((i -1)
           (string ,string))
       (block loop
         (while (numberp i)
           (setq i (string-match ,regexp ,string (1+ i)))
           (unless (numberp i)
             (return-from loop))
           ,@body)))))

(defun weather-metno-query-format (string data &optional no-exec prefix)
  "Format STRING with DATA.

This function is similar to `format'.  But uses a named syntax instead.
DATA is an `assq' list and {NAME} gets replaced by the `cdr' of the entry in
the `assq' list.

It is possible to apply actions to the data first by using the {NAME|ACTION}
syntax.  Data in this context is the `cdr' of the entry returned by `assq'.
ACTION can be one of the following:

- Starts with a %: Use a different format than the default %s.  See `format'.
- Starts with a (: If NO-EXEC is nil then the expression is evaluated and the
  variable $data is set to the matching data.
- Starts with a ': If NO-EXEC is nil then the function is called with
  the data as first argument.
- Starts with a :: If NO-EXEC is nil or `prefix' then assume
  (concat PREFIX ACTION) is a function and execute it.
- car,cdr,cadr,nthX: the result of car, cdr, cadr, or (nth X $data)

Warning: Always set NO-EXEC if the format string comes from an outside source!"
  (let ((ret string))
    (weather-metno-query~regexp-iterate
     ("{\\(.+?\\)\\(?:}\\||\\(.*?\\)}\\)" string)

     (let* ((what (match-string 1 string))
            (what-symb (intern what))
            (action (match-string 2 string))
            (data (assq what-symb data)))
       (when data
         (setq ret
               (replace-regexp-in-string
                (regexp-quote (match-string 0 string))
                (cond
                 ((and (stringp action) (string-prefix-p "%" action))
                  (format action (cdr data)))
                 ((and (not no-exec) (stringp action)
                       (string-prefix-p "(" action))
                  (format "%s" (let (($data (cdr data)))
                                 (eval (read action)))))
                 ((and (not no-exec) (stringp action)
                       (string-prefix-p "'" action))
                  (format "%s" (funcall (intern (substring action 1))
                                        (cdr data))))
                 ((and (stringp action) (string= "car" action))
                  (format "%s" (cadr data)))
                 ((and (stringp action) (string= "cdr" action))
                  (format "%s" (cddr data)))
                 ((and (stringp action) (string= "cadr" action))
                  (format "%s" (caddr data)))
                 ((and (stringp action) (string-prefix-p "nth" action))
                  (format "%s" (nth (string-to-number (substring action 3))
                                    (cdr data))))
                 ((and (or (not no-exec) (eq no-exec 'prefix)) prefix
                       (stringp action) (string-prefix-p ":" action))
                  (format "%s" (funcall (intern
                                         (concat prefix
                                                 (substring action 1)))
                                        (cdr data))))
                 (t (format "%s" (cdr data))))
                ret)))))
    ret))

(provide 'weather-metno-query)

;;; weather-metno-query.el ends here
