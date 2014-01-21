;;; weather-metno-mode-line.el --- Weather in the mode line

;; Copyright (C) 2012-2014 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

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
;;

;;; Code:

(require 'weather-metno)
(require 'calendar)

(require 'cl-lib)

(defvar weather-metno-mode-line--string ""
  "String to display in the mode line.")
;;;###autoload (put 'weather-metno-mode-line-string 'risky-local-variable t)

(defcustom weather-metno-mode-line-interval 3600
  "Update interval for mode-line in seconds.
Less than 3600s (1h) does NOT make sense!"
  :group 'weather-metno
  :type 'integer)

(defvar weather-metno-mode-line--timer nil
  "Update timer.")

(defun weather-metno-mode-line--date<= (a b)
  "returns non-nil if A <= B.
Values are expected in `decode-time' format."
  (if (and (<= (nth 5 a) (nth 5 b))
           (<= (nth 4 a) (nth 4 b))
           (<= (nth 3 a) (nth 3 b)))
      (if (= (nth 2 a) (nth 2 b))
          (if (= (nth 1 a) (nth 1 b))
              (<= (nth 0 a) (nth 0 b))
            (<= (nth 1 a) (nth 1 b)))
        (<= (nth 2 a) (nth 2 b)))))

(defun weather-metno-mode-line--time-in-range? (time from to)
  "Returns non-nil if TIME is beteween FROM and TO.
Values are expected in `decode-time' format."
  (and
   (weather-metno-mode-line--date<= from time)
   (weather-metno-mode-line--date<= time to)))

(defun weather-metno-mode-line--format-weather ()
  (let ((location (car weather-metno--data))
        (time (decode-time))
        (temperature most-negative-fixnum)
        (cloudiness most-negative-fixnum)
        (precipitation most-negative-fixnum)
        (last-time '(0 0 0 0 0 0)))

    (dolist (forecast (cadr location))
      (let* ((date-range (car forecast))
             (from (car date-range))
             (from-time (decode-time from))
             (to (cadr date-range))
             (to-time (decode-time to)))
        (if (weather-metno-mode-line--time-in-range? time from-time to-time)
            (dolist (entry (cdr forecast))
              (cl-case (car entry)
                (temperature (let ((value (string-to-number
                                           (cdr (assq 'value (cadr entry))))))
                               (when (< temperature value)
                                 (setq temperature value))))
                (cloudiness (let ((value (string-to-number
                                          (cdr (assq 'percent (cadr entry))))))
                              (when (< cloudiness value)
                                (setq cloudiness value))))
                (precipitation (let ((value (string-to-number
                                             (cdr (assq 'value (cadr entry))))))
                                 (when (< precipitation value)
                                   (setq precipitation value))))))
          (when (and (weather-metno-mode-line--date<= from-time time)
                     (weather-metno-mode-line--date<= last-time from-time))
            (setq last-time from-time)
            (dolist (entry (cdr forecast))
              (cl-case (car entry)
                (temperature (setq temperature (string-to-number
                                                (cdr (assq 'value (cadr entry))))))
                (cloudiness (setq cloudiness (string-to-number
                                              (cdr (assq 'percent (cadr entry))))))
                (precipitation (setq precipitation (string-to-number
                                                    (cdr (assq 'value (cadr entry))))))))))))
    (format "[%s℃ %s㎜ %s%%]"
            (if (= temperature most-negative-fixnum)
                "X" temperature)
            (if (= precipitation most-negative-fixnum)
                "X" precipitation)
            (if (= cloudiness most-negative-fixnum)
                "X" cloudiness))))

(defun weather-metno-mode-line--update ()
  "Update mode line."
  (unless weather-metno--data
    (weather-metno-update))
  (setq weather-metno-mode-line--string
        (weather-metno-mode-line--format-weather))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode weather-metno-mode-line
  "Toggle weather forecast display in mode line.
With a prefix argument ARG, enable display if ARG is positive, and disable
it otherwise."
  :global t
  :group 'weather-metno

  (setq weather-metno-mode-line--string "")
  (unless global-mode-string
    (setq global-mode-string '("")))
  (when weather-metno-mode-line--timer
    (cancel-timer weather-metno-mode-line--timer))
  (unless weather-metno-mode-line
    (add-to-list 'global-mode-string 'weather-metno-mode-line--string t)
    (weather-metno-mode-line--update)
    (setq weather-metno-mode-line--timer
          (run-at-time nil weather-metno-mode-line-interval
                       'weather-metno-mode-line--update))))


(provide 'weather-metno-mode-line)

;;; weather-metno-mode-line.el ends here
