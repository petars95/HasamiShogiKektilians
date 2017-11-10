(setq N (read))

(defun napravi_vrstu (dim el) (cond ((equal dim '0) '())
                                             (t (cons el (napravi_vrstu (1- dim) el)))))

(defun napravi_tablu1 (n n1) (cond ((equal n '0) '())
                                               ((or (equal n n1) (equal n (1- n1))) (cons (napravi_vrstu n1 'X) (napravi_tablu1 (1- n) n1)) )
                                               ((or (equal n '1) (equal n '2)) (cons (napravi_vrstu n1 'O) (napravi_tablu1 (1- n) n1)) )
                                               (t (cons (napravi_vrstu n1 '-) (napravi_tablu1 (1- n) n1)))))
											   
(defun napravi_tablu (n) (napravi_tablu1 n n))


(defun stampaj_tablu (tabla) (cond ((null tabla) '())
                                                (t (progn (format t "~{~a ~}~%" (car tabla)) (stampaj_tablu (cdr tabla))))))

