
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

(defmacro weather-metno-query (x &rest body)
  "Queries DATA for values at LOCATION for DATE.

\(fn (DATA LOCATION DATE) BODY...)"

  (let ((data (nth 0 x))
        (location (nth 1 x))
        (date (nth 2 x))
        (body2 (weather-metno-query~split body)))

    `(let (ret)
       (dolist (forecast (cadr (car ,data))) ;; TODO get location
         (let* ((date-range (car forecast))
                (from (car date-range))
                (from-date (org-weather-metno~time-to-date from))
                (to (cadr date-range))
                (to-date (org-weather-metno~time-to-date to)))
           (when (and (calendar-date-equal ,date from-date)
                      (calendar-date-equal ,date to-date))
             (dolist (entry (cdr forecast))
               (case (car entry)
                 ,@(mapcar
                    (lambda (ops)
                      (let ((symbol (cadr (weather-metno-query~get-op :get ops))))
                        `(,symbol
                          (let ((storage (assq (quote ,symbol) ret)))
                            (unless storage
                              (setq storage (cons (quote ,symbol) nil))
                              (setq ret (append ret (list storage))))

                            (setcdr storage (append (cdr storage) (cdr entry)))))))
                    body2))))))
       ret)))



(weather-metno-query
 (org-weather-metno~data '(lat lon msl) '(10 1 2012))

 :get temperature :select value :each (convert or something) :reduce avg :format "%s°C"
 :get windSpeed :select (mps name beaufort) :max mps
 :get precipitation :select value :max-min
 :get pressure :select value :max)
