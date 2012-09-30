;;; weather-metno.el --- Weather data from met.no in Emacs -*- lexical-binding: t -*-

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
;; Internal commands use ~ in the prefix.

;;; Code:

(require 'url)
(require 'url-cache)
(require 'xml)

(eval-when-compile
  (require 'cl))

(defgroup weather-metno nil
  "Weather data from met.no in Emacs."
  :prefix "weather-metno-"
  :group 'comm)

(defconst weather-metno-url "http://api.met.no/weatherapi/"
  "URL to api.met.no.")

(defconst weather-metno-forecast-version "1.8"
  "Version of locationforecast.")

(defun weather-metno~parse-time-string (time-string)
  "Parse a RFC3339 compliant TIME-STRING.
This function is similar to `decode-time' but works with RFC3339 (ISO 8601)
compatible timestamps.  Except for fractional seconds! Thanks to tali713."
  (destructuring-bind (year month day time zone)
      (append (timezone-parse-date time-string) nil)
    `(,@(subseq (parse-time-string time) 0 3)
      ,(string-to-int day)
      ,(string-to-int month)
      ,(string-to-int year)
      nil
      nil
      ,(if zone
           (mod (* 60
                   (timezone-zone-to-minute
                    (replace-regexp-in-string ":" "" zone)))
            (* 3600 24))
         (car (current-time-zone))))))

(defun weather-metno~forecast-url (lat lon &optional msl)
  "Create the url from LAT, LON and MSL to be used by `weather-metno-forecast'."
  (concat (format "%slocationforecast/%s/?lat=%s;lon=%s"
                  weather-metno-url weather-metno-forecast-version lat lon)
          (if msl
              (format ";msl=%s" msl)
            "")))

(defun weather-metno~date-to-time (x)
  "Converts RFC3339 string X to Emacs's time format.
Emacs's time format is (HIGH LOW . IGNORED)."
  (apply 'encode-time (weather-metno~parse-time-string x)))

(defun weather-metno~forecast-convert (xml)
  "Convert the XML structure from met.no to an internal format.
Internal format is ((COORD ((DATE-RANGE) (ENTRY0) (ENTRY1) ...))).
COORD is (LAT LON ALT).
DATE-RANGE is (FROM TO) with FROM and TO in Emacs's time format.
ENTRY is (TYPE (ATTRIBUTES))."
  (let (res)
    (dolist (i
             (xml-node-children (car (xml-get-children
                                      (car xml)
                                      'product))))
      ;; iterator over all <time> entries
      (when (and (consp i) (eq (car i) 'time))
        ;; extract from,to attributes
        (let ((from (weather-metno~date-to-time
                     (xml-get-attribute i 'from)))
              (to (weather-metno~date-to-time
                   (xml-get-attribute i 'to))))

          ;; iterator over <location> entries
          (dolist (loc (xml-get-children i 'location))

            (let* ((coord (list
                           (xml-get-attribute-or-nil loc 'latitude)
                           (xml-get-attribute-or-nil loc 'longitude)
                           (xml-get-attribute-or-nil loc 'altitude)
                           )) ;; Coord: (lat lon alt)
                   (entry (assoc coord res))
                   (date-range (list from to))
                   (forecast (assoc date-range (cdr entry))))
              (unless entry
                (setq entry (list coord))
                (setq res (append res (list entry))))

              (unless forecast
                (setq forecast (list date-range))
                (setcdr entry (list (append (cadr entry)
                                            (list forecast)))))


              (dolist (fcast (xml-node-children loc))
                (when (consp fcast)
                  (setcdr forecast
                          (append (cdr forecast)
                                  (list
                                   (list (xml-node-name fcast)
                                         (xml-node-attributes fcast))))))))))))
    res))

(defun weather-metno-forecast-receive (callback lat lon &optional msl raw-xml)
  "Fetch weather forecast from met.no for LAT LON (MSL).
CALLBACK is called when the request is completed.  CALLBACK gets called with
 (LAT LON MSL RAW-XML DATA) as arguments.  DATA is the received data in the
format described in `weather-metno~forecast-convert'.  Unless RAW-XML is set in
which case DATA is simply the result of `xml-parse-region'.

See http://api.met.no/weatherapi/locationforecast/1.8/documentation for the
documentation of the web API."
  (let ((url (weather-metno~forecast-url lat lon msl)))
    (url-retrieve url
                  (lambda (status callback url lat lon msl)
                    (switch-to-buffer (current-buffer))
                    (goto-char (point-min))
                    (unless (search-forward "\n\n" nil t)
                      (kill-buffer)
                      (error "Error in http reply"))
                    (let ((headers (buffer-substring (point-min) (point))))
                      (unless (string-match-p "^HTTP/1.1 200 OK" headers)
                        (kill-buffer)
                        (error "Unable to fetch data"))
                      (url-store-in-cache (current-buffer))

                      (let ((xml (xml-parse-region (point) (point-max))))
                        (kill-buffer)

                        (funcall callback lat lon msl raw-xml
                                 (if raw-xml
                                     xml
                                   (weather-metno~forecast-convert xml))))))
                  (list callback url lat lon msl))))

(provide 'weather-metno)

;;; weather-metno.el ends here
