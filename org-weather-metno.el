;;; org-weather-metno.el --- Org support for weather-metno-el -*- lexical-binding: t -*-

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

;; Add the following entry to an agenda file

;; * Weather
;; #+CATEGORY: Weather
;; %%(org-weather-metno)

;;; Code:

(require 'weather-metno)
(require 'weather-metno-query)

(eval-when-compile
  (require 'cl))

(defvar org-weather-metno~data nil
  "The retreived weather data.")

(defun org-weather-metno~q-avg (x)
  "Calculate average of X."
  (/ (apply #'+ x)
     (length x)))

(defcustom org-weather-metno-query
  '(:get temperature :name temperature-max :select value :each string-to-number :max
    :get temperature :name temperature-min :select value :each string-to-number :min
    :get temperature :name temperature-avg :select value :each string-to-number
      :reduce org-weather-metno~q-avg
    :get precipitation :name precipitation-max :select value :each string-to-number :max
    :get precipitation :name precipitation-min :select value :each string-to-number :min
    :get symbol :select number :each string-to-number :max)
  "The query used by `org-weather-metno-format'.
See `weather-metno-query' for more information."
  :group 'weather-metno)
;;;###autoload (put 'org-weather-metno-query 'risky-local-variable t)

(defcustom org-weather-metno-format "{symbol|:symbol} {precipitation-min}–{precipitation-max} ㎜ ({precipitation-min-time|:time}–{precipitation-max-time|:time}) {temperature-min}–{temperature-max} ℃ ({temperature-min-time|:time}–{temperature-max-time|:time})"
  "The format of the org agenda weather entry.
See `org-weather-metno-query' and `weather-query-format' for more information."
  :group 'org-weather-metno
  :type 'string)
;;;###autoload (put 'org-weather-metno-format 'risky-local-variable t)

(defun org-weather-metno~f-time (date-range)
  "Convert DATE-RANGE to some time."
  (format-time-string "%Hh" (car date-range)))

(defun org-weather-metno~f-symbol (number)
  "Fetch symbol for NUMBER."
  (let ((image (weather-metno-get-weathericon number)))
    (if image
        (propertize "icon"
                    'display (append image '(:ascent center))
                    'rear-nonsticky '(display))
      "")))


;;;###autoload
(defun org-weather-metno ()
  "Display weather in diary/org-mode."
  (unless weather-metno~data
    (weather-metno-update))

  (let ((query-data (eval `(weather-metno-query
                            (weather-metno~data nil date)
                            ,@org-weather-metno-query))))
    (when query-data
      (weather-metno-query-format
       org-weather-metno-format
       query-data
       nil "org-weather-metno~f-"))))

(provide 'org-weather-metno)

;;; org-weather-metno.el ends here
