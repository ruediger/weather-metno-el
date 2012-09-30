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

(defvar org-weather-metno~data nil
  "The retreived weather data.")

(defun org-weather-metno-update (&optional lat lon msl)
  "Update weather data."
  (weather-metno-forecast-receive
   (lambda (lat lon msl raw-xml data)
     (assert (not raw-xml))
     (setq org-weather-metno~data data))
   (or lat weather-metno-location-latitude)
   (or lon weather-metno-location-longitude)
   (or msl weather-metno-location-msl)))

(defun org-weather-metno~time-to-date (time)
  "Convert TIME in Emacs's time format to a date in calendar format."
  (let ((d (decode-time time)))
    (list (nth 4 d) (nth 3 d) (nth 5 d))))

(defun org-weather-metno~date-range-to-time (date-range)
  "Convert DATE-RANGE to some time."
  (format-time-string "%Hh" (car date-range)))

;;;###autoload
(defun org-weather-metno ()
  "Display weather in diary/org-mode."
  (unless org-weather-metno~data
    (org-weather-metno-update))

  (let ((location (car org-weather-metno~data))
        temperature
        cloudiness
        precipitation
        (out ""))

    (dolist (forecast (cadr location))
      (let* ((date-range (car forecast))
             (from (car date-range))
             (from-date (org-weather-metno~time-to-date from))
             (to (cadr date-range))
             (to-date (org-weather-metno~time-to-date to)))
        (when (and (calendar-date-equal date from-date)
                   (calendar-date-equal date to-date))
          (dolist (entry (cdr forecast) res)
            (case (car entry)
              (temperature (setq temperature (append temperature
                                                     (list
                                                      (list entry from to)))))
              (cloudiness (setq cloudiness (append cloudiness
                                                   (list
                                                    (list entry from to)))))
              (precipitation (setq precipitation (append precipitation
                                                         (list
                                                          (list entry from
                                                                to))))))))))
    (let ((temp-min most-positive-fixnum) temp-min-time
          (temp-max most-negative-fixnum) temp-max-time)
      (dolist (data temperature)
        (let ((temp (string-to-number (cdr (assq 'value (cadar data))))))
          (when (< temp temp-min)
            (setq temp-min temp)
            (setq temp-min-time (cdr data)))
          (when (> temp temp-max)
            (setq temp-max temp)
            (setq temp-max-time (cdr data)))))
      (format "Temperatures %s℃ (%s) to %s℃ (%s)"
              temp-min
              (org-weather-metno~date-range-to-time temp-min-time)
              temp-max
              (org-weather-metno~date-range-to-time temp-max-time)))))

(provide 'org-weather-metno)

;;; org-weather-metno.el ends here
