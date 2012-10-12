
(defun avg (x)
  (/ (reduce #'+ x)
     (length x)))


(weather-metno-query
 (weather-metno~data '(lat lon msl) '(10 12 2012))

 :get temperature :name temperature-avg :select value :each string-to-number :reduce avg
 :get temperature :name temperature-max :select value :each string-to-number :max
 :get windSpeed :select (mps name beaufort)
 :get precipitation :select value :each string-to-number :max
 :get pressure :select value :each string-to-number :min)


; :get temperature :name temperature-max :select value :each string-to-number :max
; :get temperature :name temperature-min :select value :each string-to-number :min

