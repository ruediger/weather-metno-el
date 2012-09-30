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

;; See http://api.met.no/weatherapi/documentation
;; and http://api.met.no/license_data.html

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

(defun weather-metno~get-default-location-name ()
  "Find default location name."
  (if (boundp 'user-location-name)
      user-location-name
    (if (boundp 'calendar-location-name)
        calendar-location-name
      "")))

(defcustom weather-metno-location-name
  (weather-metno~get-default-location-name)
  "Name of the default weather location.
See `weather-metno-location-latitude', `weather-metno-location-longitude', and
`weather-metno-location-msl'."
  :group 'weather-metno
  :type 'string)

(defun weather-metno~get-default-location-latitude ()
  "Find default location latitude."
  (if (boundp 'user-location-latitude)
      user-location-latitude
    (if (require 'solar nil t)
        (calendar-latitude)
      0))) ;; TODO better default?

(defcustom weather-metno-location-latitude
  (weather-metno~get-default-location-latitude)
  "Latitude of `weather-metno-location-name' in degrees.
See `weather-metno-location-longitude' and `weather-metno-location-msl'."
  :group 'weather-metno
  :type '(number :tag "Exact"))

(defun weather-metno~get-default-location-longitude ()
  "Find default location latitude."
  (if (boundp 'user-location-longitude)
      user-location-longitude
    (if (require 'solar nil t)
        (calendar-longitude)
      0))) ;; TODO better default?

(defcustom weather-metno-location-longitude
  (weather-metno~get-default-location-longitude)
  "Longitude of `weather-metno-location-name' in degrees.
See `weather-metno-location-latitude' and `weather-metno-location-msl'."
  :group 'weather-metno
  :type '(number :tag "Exact"))

(defcustom weather-metno-location-msl nil
  "Whole meters above sea level of `weather-metno-location-name' in degrees.
See `weather-metno-location-latitude' and `weather-metno-location-msl'."
  :group 'weather-metno
  :type '(choice (const nil)
                 (number :tag "Exact")))

(defcustom weather-metno-format-time-string "%Y-%m-%dT%H:%M:%S%Z"
  "Format string used to format time data.
See `format-time-string' for a description of the format."
  :group 'weather-metno
  :type 'string)

(defconst weather-metno-url "http://api.met.no/weatherapi/"
  "URL to api.met.no.")

(defconst weather-metno-weathericon-version "1.0"
  "Version of weathericon.")

(defconst weather-metno-forecast-version "1.8"
  "Version of locationforecast.")

(defconst weather-metno-logo "met-no.png"
  "File name of the met.no logo.")

(defun weather-metno~weathericon-url (icon &optional nightp polarp content-type)
  "Create URL for weathericon API."
  (assert (integerp icon))
  (format "%sweathericon/%s/?symbol=%s%s%s;content_type=%s" weather-metno-url
          weather-metno-weathericon-version icon
          (if nightp ";is_night=1" "")
          (if polarp ";is_polarday=1" "")
          (or content-type "image/png")))

(defun weather-metno-insert-weathericon (buffer point icon &optional nightp
                                                polarp content-type)
  "Fetch the weather ICON and insert it into BUFFER at POINT.
This function works asynchronously.  If NIGHTP is set then a night icon will be
fetched.  If POLARP then an icon for a polarday will be fetched.  CONTENT-TYPE
specifies the content-type (default image/png).

This uses the met.no weathericon API
http://api.met.no/weatherapi/weathericon/1.0/documentation

The data is available under CC-BY-3.0."
  (let ((url (weather-metno~weathericon-url icon nightp polarp content-type)))
    (url-retrieve
     url
     (lambda (status buffer point)
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

         (let ((image (create-image (buffer-substring (point) (point-max))
                                    (if content-type nil 'png) t)))
           (kill-buffer)
           (with-current-buffer buffer
             (put-image image point)))))
     (list buffer point))))

(defun weather-metno~parse-time-string (time-string)
  "Parse a RFC3339 compliant TIME-STRING.
This function is similar to `decode-time' but works with RFC3339 (ISO 8601)
compatible timestamps.  Except for fractional seconds! Thanks to tali713."
  (destructuring-bind (year month day time zone)
      (append (timezone-parse-date time-string) nil)
    `(,@(subseq (parse-time-string time) 0 3)
      ,(string-to-number day)
      ,(string-to-number month)
      ,(string-to-number year)
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
  "Convert RFC3339 string X to Emacs's time format.
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
                  (lambda (status callback lat lon msl)
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
                  (list callback lat lon msl))))

(defun weather-metno~string-empty? (x)
  "Return non-nil when X is either nil or empty string."
  (or (string= x "") (not x)))

(defun weather-metno~format-with-loc (x)
  "Convert X into a minibuffer query string."
  (if (weather-metno~string-empty?
       weather-metno-location-name)
      (concat x ": ")
    (format "%s [Default for %s]: " x weather-metno-location-name)))

(defun weather-metno~n2s (n)
  "Convert N from number to string or nil if not a number."
  (if (numberp n)
      (number-to-string n)))

(defvar weather-metno-buffer-name "*Weather*"
  "Name for the forecast buffer.")

(defface weather-metno-header
  '((t :inherit header-line))
  "Face for top header line."
  :group 'weather-metno)

(defface weather-metno-date-range
  '((t :inherit header-line))
  "Face for date range line."
  :group 'weather-metno)

(defface weather-metno-entry
  '((t :inherit font-lock-variable-name-face))
  "Face for entry."
  :group 'weather-metno)

(defface weather-metno-footer
  '((t :inherit font-lock-comment-face))
  "Face for entry."
  :group 'weather-metno)

(defun weather-metno~insert (face &rest args)
  "Insert ARGS into current buffer with FACE."
  (insert (propertize (apply 'concat args) 'face face)))

(defun weather-metno~format-value-unit (name attributes)
  "Helper to format entries that contain UNIT and VALUE.
E.g. temperature, pressure, precipitation, ..."
  (format "%s %s %s"
          name
          (cdr (assq 'value attributes))
          (cdr (assq 'unit attributes))))

(defun weather-metno~format~precipitation (attributes _)
  "Format precipitation."
  (weather-metno~format-value-unit "Precipitation" attributes))

(defun weather-metno~format~temperature (attributes _)
  "Format temperature."
  (weather-metno~format-value-unit "Temperature" attributes))

(defun weather-metno~format~pressure (attributes _)
  "Format pressure."
  (weather-metno~format-value-unit "Pressure" attributes))

(defun weather-metno~format~humidity (attributes _)
  "Format humidity."
  (weather-metno~format-value-unit "Humidity" attributes))

(defun weather-metno~format~windDirection (attributes _)
  "Format wind direction."
  (format "Wind direction %s° (%s)"
          (cdr (assq 'deg attributes))
          (cdr (assq 'name attributes))))

(defcustom weather-metno-translate-wind-name
  '(("Stille" . "Calm")                ; 0  beaufort scale
    ("Flau vind" . "Light air")        ; 1
    ("Svak vind" . "Light breeze")     ; 2
    ("Lett bris" . "Gentle breeze")    ; 3
    ("Laber bris" . "Moderate breeze") ; 4
    ("Frisk bris" . "Fresh breeze")    ; 5
    ("Liten kuling" . "Strong breeze") ; 6
    ("Stiv kuling" . "High wind")      ; 7
    ("Sterk kuling" . "Fresh gale")    ; 8
    ("Liten storm" . "Strong gale")    ; 9
    ("Full storm" . "Storm")           ; 10
    ("Sterk storm" . "Violent storm")  ; 11
    ("Orkan" . "Hurricane"))           ; 12
  "Table to translate wind names from Norwegian."
  :group 'weather-metno) ; TODO type

(defun weather-metno~translate-wind-name (name)
  "Translate NAME from Norwegian."
  (cdr (assoc name weather-metno-translate-wind-name)))

(defun weather-metno~format~windSpeed (attributes _)
  "Format wind speed."
  (format "Wind speed %s m/s (Beaufort scale %s) %s"
          (cdr (assq 'mps attributes))
          (cdr (assq 'beaufort attributes))
          (weather-metno~translate-wind-name (cdr (assq 'name attributes)))))

(defun weather-metno~format~cloudiness (attributes _)
  "Format cloudiness."
  (format "Cloudiness %s%%"
          (cdr (assq 'percent attributes))))

(defun weather-metno~format~fog (attributes _)
  "Format fog."
  (format "Fog %s%%"
          (cdr (assq 'percent attributes))))

(defun weather-metno~format~lowClouds (attributes _)
  "Format low clouds."
  (format "Low clouds %s%%"
          (cdr (assq 'percent attributes))))

(defun weather-metno~format~mediumClouds (attributes _)
  "Format medium clouds."
  (format "Medium clouds %s%%"
          (cdr (assq 'percent attributes))))

(defun weather-metno~format~highClouds (attributes _)
  "Format high clouds."
  (format "High clouds %s%%"
          (cdr (assq 'percent attributes))))

(defun weather-metno~format~symbol (attributes last-headline)
  "Format symbol."
  (weather-metno-insert-weathericon
   (current-buffer) last-headline
   (string-to-number (cdr (assq 'number attributes))))
  "")

;; Todo the last-headline thing sucks. Find something better!
(defun weather-metno~format-entry (entry &optional last-headline)
  "Format ENTRY.
LAST-HEADLINE should point to the place where icons can be inserted."
  (let ((formatter (intern (concat "weather-metno~format~"
                                   (symbol-name (car entry))))))
    (if (fboundp formatter)
        (funcall formatter (cadr entry) last-headline)
      (format "Unknown entry %s" entry))))

;;;###autoload
(defun weather-metno-forecast (lat lon &optional msl)
  "Fetch weather forecast from met.no for LAT LON (MSL)."
  (interactive
   (list
    (read-string (weather-metno~format-with-loc "Latitude")
                 (weather-metno~n2s weather-metno-location-latitude))
    (read-string (weather-metno~format-with-loc "Longitude")
                 (weather-metno~n2s weather-metno-location-longitude))
    (read-string (weather-metno~format-with-loc "Whole meters above sea level")
                 (weather-metno~n2s weather-metno-location-msl))))
  (when (weather-metno~string-empty? msl)
    (setq msl nil))

  (weather-metno-forecast-receive
   (lambda (lat lon msl raw-xml data)
     (assert (not raw-xml))

     (when (get-buffer weather-metno-buffer-name)
       (kill-buffer weather-metno-buffer-name))

     (switch-to-buffer weather-metno-buffer-name)
     (erase-buffer)
     (goto-char (point-min))

     (dolist (location data)
       (weather-metno~insert 'weather-metno-header
                             (format "Forecast for location %s,%s %s\n"
                                     (caar location) (cadar location)
                                     (caddar location)))

       (dolist (forecast (cadr location))
         (let ((date-range (car forecast))
               (last-headline (point)))
           (weather-metno~insert 'weather-metno-date-range
                                 "* From "
                                 (format-time-string
                                  weather-metno-format-time-string
                                  (car date-range))
                                 " to "
                                 (format-time-string
                                  weather-metno-format-time-string
                                  (cadr date-range))
                   "\n")

           (dolist (entry (cdr forecast))
             (let ((fmt-entry (weather-metno~format-entry entry last-headline)))
               (unless (weather-metno~string-empty? fmt-entry)
                 (weather-metno~insert 'weather-metno-entry
                                       "** " fmt-entry "\n"))
               ))
           ))
       )
     (insert "\n")
     (when (file-exists-p weather-metno-logo)
       (insert-image-file weather-metno-logo))
     (weather-metno~insert
      'weather-metno-footer
      "Data from The Norwegian Meteorological Institute (CC BY 3.0)\n")) ;; TODO link!
     lat lon msl))

(provide 'weather-metno)

;;; weather-metno.el ends here
