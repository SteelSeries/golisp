(define rumble-port nil)


(define (open-rumble)
  (unless rumble-port
          (set! rumble-port (open-output-file "/dev/cu.usbmodem1411"))))


(define (close-rumble)
  (when rumble-port
        (close-output-port rumble-port)
        (set! rumble-port nil)))


(define (join-string sep coll)
  (reduce (lambda (result-so-far element)
            (str result-so-far sep element))
          ""
          coll))


(define (join-list sep coll)
  (reduce (lambda (result-so-far element)
            (append result-so-far sep element))
          '()
          coll))


(define (replicate value count)
  (map (lambda (i) value) (interval 1 count)))


(define (rumble-scale val)
  (if (eq? n 0)
      0
      (+ 31 (* (- val 1) 16))))


(define (rumble data)
  (write-string (if (list? data)
                    (str "l" (join-string " " data))
                    (str "s" (rumble-scale data)))
                rumble-port)
  (newline rumble-port))


(define (pulse width . others)
  (let ((strength (if (car others) (rumble-scale (car others)) 255))
        (count (if (cadr others) (cadr others) 1))
        (space (if (caddr others) (caddr others) 0)))
    (join-list (replicate 0 (/ space 20))
               (replicate (replicate strength (/ width 20)) count))))


(define (multipulse args)
  (flatten (map (lambda (pulse-tuple)
                  (let* ((width (car pulse-tuple))
                         (strength (rumble-scale (cadr pulse-tuple)))
                         (space (if (caddr pulse-tuple) (caddr pulse-tuple) 0)))
                    (append! (replicate strength (/ width 20))
                             (replicate 0 (/ space 20)))))
                (partition 3 args))))


(define (wave args)
  (flatten (map (lambda (wave-tuple)
                  (let ((width (car wave-tuple))
                        (strength (cadr wave-tuple)))
                    (replicate (rumble-scale strength) (/ width 20))))
                (partition 2 args))))
