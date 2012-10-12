;;; org-weather-metno.el --- Org support for weather-metno-el -*- lexical-binding: t -*-

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

;; Add the following entry to an agenda file

;; * Weather
;; #+CATEGORY: Weather
;; %%(org-weather-metno)

;;; Code:

(require 'weather-metno)
(require 'weather-metno-query)

(defvar org-weather-metno~data nil
  "The retreived weather data.")

(defun org-weather-metno~q-avg (x)
  "Calculate average of X."
  (/ (reduce #'+ x)
     (length x)))

(defcustom org-weather-metno-query
  '(:get temperature :name temperature-max :select value :each string-to-number :max
    :get temperature :name temperature-min :select value :each string-to-number :min
    :get temperature :name temperature-avg :select value :each string-to-number
      :reduce org-weather-metno~q-avg
    :get precipitation :select value :each string-to-number :max)
  ""
  :group 'weather-metno)

(defcustom org-weather-metno-format "{temperature-min}℃ ({temperature-min-time|:time}) to {temperature-max}℃ ({temperature-max-time|:time})"
  "The for"
  :group 'org-weather-metno
  :type 'string)

(defun org-weather-metno~f-time (date-range)
  "Convert DATE-RANGE to some time."
  (format-time-string "%Hh" (car date-range)))

;;;###autoload
(defun org-weather-metno ()
  "Display weather in diary/org-mode."
  (unless weather-metno~data
    (weather-metno-update))

  (let ((query-data (eval `(weather-metno-query
                            (weather-metno~data nil date)
                            ,@org-weather-metno-query))))
    (when query-data
      (message "Query: %s" query-data)
      (weather-metno-query-format
       org-weather-metno-format
       query-data
       nil "org-weather-metno~f-"))))

(provide 'org-weather-metno)

;;; org-weather-metno.el ends here
