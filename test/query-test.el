;;; query-test.el --- Test suite for query. -*- lexical-binding: t -*-

;; Copyright (C) 2012 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; URL: https://github.com/ruediger/weather-metno-el
;; Keywords: internal

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

;;; Code:

(require 'ert)
(require 'weather-metno-query)

(ert-deftest query~split-test ()
  "Test `weather-metno-query~split'."
  (let ((x '(:get a :foo :bar :get b x y))
        (y '((:get a :foo :bar) (:get b x y))))
    (should (equal (weather-metno-query~split x) y))))

(ert-deftest query~get-op-test ()
  "Test `weather-metno-query~get-op'."
  (should (equal (weather-metno-query~get-op :get '(:get x :foo :bar)) '(:get x)))
  (should (equal (weather-metno-query~get-op :foo '(:get x :foo :bar)) '(:foo)))
  (should (equal (weather-metno-query~get-op :bar '(:get x :bar a b c :d)) '(:bar a b c))))

(ert-deftest query~index ()
  "Test `weather-metno~index'."
  (should (= (weather-metno~index 'd '(a b c d e f g)) 3)))

(ert-deftest query~merge-cases ()
  "Test `weather-metno~merge-cases'."
  (should (equal (weather-metno-query~merge-cases '((a b c) (d e) (a x)))
                 '((d e) (a b c x)))))

(ert-deftest query~regexp-iterator ()
  "Test `weather-metno-query~regexp-iterate'."
  (let ((data "{b} c {d} e")
        (matches '("b" "d"))
        (n 0))
  (weather-metno-query~regexp-iterate
   ("{\\(.*?\\)}" data)
   (should (string= data string))
   (should (string= (nth n matches) (match-string 1 string)))
   (setq n (1+ n)))
  (should (= n 2))))

(defun weather-metno-query-test-x (x) (nth 2 x))

(ert-deftest query-format ()
  "Test `weather-metno-query-format'."
  (should (string= (weather-metno-query-format "hello {x}" '((x . "world")))
                   "hello world"))
  (should (string= (weather-metno-query-format "hello {y}" '((x . "world")))
                   "hello "))
  (should (string= (weather-metno-query-format "hello {z}" '((x . "world")) nil nil "z")
                   "hello z"))
  (should (string= (weather-metno-query-format "Hello {x}{y}" '((z . "Z")
                                                                (x . "world")
                                                                (y . "!")))
                   "Hello world!"))
  (should (string= (weather-metno-query-format "Hello {x|'not}" '((x . nil)))
                   "Hello t"))
  (should (string= (weather-metno-query-format "Hello {x|(concat $data \"!\")}"
                                               '((x . "world")))
                   "Hello world!"))
  (should (string= (weather-metno-query-format "{x|%+d}" '((x . 12)))
                   "+12"))
  (should (string= (weather-metno-query-format "Hello {x|'not}" '((x . nil)) t)
                   "Hello nil"))
  (should (string= (weather-metno-query-format "{x|car}" '((x 1 2))) "1"))
  (should (string= (weather-metno-query-format "{x|cdr}" '((x 1 2 3))) "(2 3)"))
  (should (string= (weather-metno-query-format "{x|cadr}" '((x 1 2))) "2"))
  (should (string= (weather-metno-query-format "{x|nth3}" '((x 1 2 3 4 5)))
                                               "4"))
  (should (string= (weather-metno-query-format "{x|:x}" '((x 1 2 3 4 5))
                                               'prefix
                                               "weather-metno-query-test-")
                   "3"))
  (should (string= (weather-metno-query-format "a{x-y}b" '((x . 1) (x-y . 2)))
                   "a2b")))

(provide 'query-test)
;;; query-test.el ends here.
