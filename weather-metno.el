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
;; 

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
  (apply 'encode-time (weather-metno~parse-time-string x)))

(defvar weather-dbg) ;; DBG!

(defun weather-metno~forecast-convert (xml)
  "Convert the XML structure from met.no to an internal format."
  (dolist (i
           (xml-node-children (car (xml-get-children
                                    (car weather-dbg) ;; replace with (car xml)!!!!!
                                    'product)))
           res)
;    (setq res nil)
    (when (and (consp i) (eq (car i) 'time))
      (let* ((from (weather-metno~date-to-time
                    (xml-get-attribute i 'from)))
             (to (weather-metno~date-to-time
                  (xml-get-attribute i 'to))))

        (dolist (loc (xml-get-children i 'location))

          (let* ((coord (list
                         (xml-get-attribute-or-nil loc 'latitude)
                         (xml-get-attribute-or-nil loc 'longitude)
                         (xml-get-attribute-or-nil loc 'altitude)
                         ))
                 (entry (assoc coord res)))
            (unless entry
              (setq entry (list coord))
              (setq res (append res (list entry)))
              (message "RES %s -> %s" res entry))

            (dolist (fcast (xml-node-children loc))
              (when (consp fcast)
                (setcdr entry
                        (list (append (cadr entry)
                                      (list (list
                                       (xml-node-name fcast)
                                       )))))
                ))

            ))

        ))))

(defun weather-metno-forecast (lat lon &optional msl)
  "."
  (let ((url (weather-metno~forecast-url lat lon msl)))
    (url-retrieve url
                  (lambda (status url lat lon msl)
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

                        (setq weather-dbg xml) ;; DBG

                        

                        )))
                  (list url lat lon msl))))
    

(provide 'weather-metno)

;;; weather-metno.el ends here
