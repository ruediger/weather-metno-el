
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

(defun weather-metno~index (x list)
  "Return the index of X in LIST."
  (let ((r 0))
    (dolist (i list r)
      (if (eq i x)
          (return)
        (setq r (1+ r))))
    r))

(defun weather-metno~op-each (ops entry)
  ""
  (let ((each (cadr (weather-metno-query~get-op :each ops)))
        (symbol (cadr (weather-metno-query~get-op :get ops))))
    (if (not each)
        entry
      (if (functionp each)
          (list 'funcall `(quote ,each) entry)
        (if (listp each)
            (progn
              (car each)
              )
            entry)))))

(defun weather-metno~op-select (ops entry)
  "Select according to OPS from ENTRY.
Implements :select operation."
  (let ((select (cadr (weather-metno-query~get-op :select ops))))
    (if (consp select)
        `(list
          ,@(mapcar
             (lambda (i)
               (weather-metno~op-each
                ops
                `(cdr (assq (quote ,i) (cadr ,entry)))))
             select))
      (if (symbolp select)
          (weather-metno~op-each
           ops
           `(cdr (assq (quote ,select) (cadr ,entry))))
        entry))))

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
                      (let ((symbol (cadr (weather-metno-query~get-op :get ops)))
                            (max (weather-metno-query~get-op :max ops))
                            (min (weather-metno-query~get-op :min ops))
                            (min-max (weather-metno-query~get-op :min-max ops)))
                        `(,symbol
                          (let ((storage (assq (quote ,symbol) ret))
                                (value ,(weather-metno~op-select ops 'entry)))
                            (unless storage
                              (setq storage
                                    (cons (quote ,symbol)
                                          ,(if max
                                               'most-negative-fixnum
                                             (if min
                                                 'most-positive-fixnum
                                               (when min-max
                                                 '(cons most-positive-fixnum
                                                        most-negative-fixnum))))))
                              (setq ret (append ret (list storage))))
                            (setcdr storage
                                    ,(if max
                                         '(max value (cdr storage))
                                       (if min
                                           '(min value (cdr storage))
                                         (if min-max
                                             '(cons (min value (cadr storage))
                                                    (max value (cddr storage)))
                                           '(append (cdr storage)
                                                    (list value))))))))))
                    body2))))))
       ,@(mapcar
          (lambda (ops)
            (let ((symbol (cadr (weather-metno-query~get-op :get ops)))
                  (reduce (cadr (weather-metno-query~get-op :reduce ops))))
              (when (functionp reduce)
                `(let ((storage (assq (quote ,symbol) ret)))
                   (when storage
                     (setcdr storage (funcall (quote ,reduce) (cdr storage))))))))
          body2)
       ret)))

(defmacro weather-metno-query~regexp-iterate (x &rest body)
  "Match REGEXP on STRING and call BODY each time.

\(fn (REGEXP STRING) BODY...)"

  (let ((regexp (car x))
        (string (cadr x)))
    `(let ((i 0)
           (string ,string))
       (block loop
         (while (numberp i)
           (setq i (string-match ,regexp ,string (1+ i)))
           (unless (numberp i)
             (return-from loop))
           ,@body)))))

(defun weather-metno-query-format (format data)
  (let ((ret format))
    (weather-metno-query~regexp-iterate
     ("{\\(.*?\\)}" format)

     (let* ((what (match-string 1 string))
            (what-symb (intern what))
            (data (assq what-symb data)))
       (when data
         (setq ret (replace-in-string ret (concat "{" what "}")
                                      (format "%s" (cdr data)))))))
    ret))

;; {temperature}

(defun avg (x)
  (/ (reduce #'+ x)
     (length x)))


(weather-metno-query
 (org-weather-metno~data '(lat lon msl) '(10 2 2012))

 :get temperature :select value :each string-to-number :reduce avg
 :get windSpeed :select (mps name beaufort)
 :get precipitation :select value :each string-to-number :min-max
 :get pressure :select value :each string-to-number :min)
