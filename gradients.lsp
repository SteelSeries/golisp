;;; DSL for calculating color sequences

(define (color c)
  (define (hex-to-tuple c)
    (let ((b (modulo c 256))
           (less-b (quotient c 256))
           (g (modulo less-b 256))
           (r (quotient less-b 256)))
      (list r g b)))
  (if (list? c)
      c
      (hex-to-tuple c)))


(define red car)
(define green cadr)
(define blue caddr)

(define step-interval-millis 100)

(define (color-shift)
  (let ((sequence '())
        (duration 1000)
        (step-interval 100)
        (percent-per-step 2)
        (current-percentage 0))

    (define (gradient steps start end)
      (map (lambda (i)
             (let ((end-portion (/ (float i) steps))
                   (start-portion (- 1.0 end-portion)))
               (list (integer (+ (* start-portion (red start)) (* end-portion (red end))))
                     (integer (+ (* start-portion (green start)) (* end-portion (green end))))
                     (integer (+ (* start-portion (blue start)) (* end-portion (blue end)))))))
           (interval 1 steps)))
    

    ;; set the duration of the gradient in milliseconds
    
    (define (duration-in-millis m)
      (set! duration m)
      (set! percent-per-step (* 100 (/ step-interval duration))))

    ;; set the number of milliseconds between steps
    
    (define (step-time-in-millis m)
      (set! step-interval m)
      (set! percent-per-step (* 100 (/ step-interval duration))))

    (define (steps-for-percentage p)
      (/ p percent-per-step))
    
    (define (prepend l)
      (set! sequence (append l sequence)))
    
    ;; compute a linear transition to the target color, taking the given
    ;; number of steps to do it
    
    (define (linear-to steps target)
      (prepend (reverse (gradient steps (car sequence) target))))

    ;; set the next color
    
    (define (set-to rgb)
      (prepend (list rgb)))

    ;; change the color at the given percentage to the target,
    ;; interpolating from the current point
    
    (define (set-to-at rgb percentage)
      (let ((required-steps (steps-for-percentage (- percentage current-percentage))))
        (set! current-percentage percentage)
        (linear-to required-steps rgb)))
    
    ;; pause for the given number of millseconds
    
    (define (pause millis)
      (define (pause-impl steps)
        (if (<= steps 0)
            ()
            (cons (car sequence) (pause-impl (- steps 1)))))
      (let ((step-count (/ millis step-interval)))
        (prepend (pause-impl step-count))))

    ;; include another precomputed sequence
    
    (define (include seq)
      (prepend (reverse (seq 'tuples))))

    ;; return the tuples (in order) of the sequence
    
    (define (tuples)
      (reverse sequence))

    ;; return the steps as css color values
    
    (define (css)
      (define (pre-pad required-length padding str)
        (if (eq? (string-length str) required-length)
            str
            (pre-pad required-length padding (str padding str))))
      (reverse (map (lambda (tuple)
                      (let ((val (+ (* (red tuple) 65536)
                                    (* (green tuple) 256)
                                    (blue tuple)))
                             (s (pre-pad 6 "0" (number->string val 16))))
                        (str "#" s)))
                    sequence)))

    ;; return the sequence as json
    
    (define (json)
      (str "["
           (reduce (lambda (so-far tuple-string)
                     (if (== so-far "")
                         tuple-string
                       (str tuple-string ", " so-far)))
                   ""
                   (map (lambda (tuple)
                          (str "["
                               (red tuple)
                               ", "
                               (green tuple)
                               ", "
                               (blue tuple)
                               "]"))
                        sequence))
           "]"))

    ;; command dispatcher
    
    (lambda (command . args)
      (case command
        ((duration-in-millis)
         (duration-in-millis (float (car args)))
         duration)
        ((step-time-in-millis)
         (step-time-in-millis (float (car args))))
        ((set-to)
         (set-to (color (car args)))
         (car sequence))
        ((set-to-at)
         (set-to-at (color (car args)) (cadr args))
         (car sequence))
        ((pause)
         (pause (car args))
         (car sequence))
        ((linear-to)
         (linear-to (car args) (color (cadr args)))
         (car sequence))
        ((include)
         (include (car args))
         (car sequence))
        ((tuples)
         (tuples))
        ((css)
         (css))
        ((json)
         (json))))))

;;; --------------------------------------------------------------------------------
;;; Convert a gradient in CSS format: linear-gradient(left, rgb(59,209,255) 0%, rgb(140,0,255) 50%, rgb(0,204,255) 100%)
;;; Into a json data for the sequence of color steps


(define (process-color c)
  (let ((maybe-rgb (split c "(")))
    (if (== (length maybe-rgb) 2)
        (let ((number-part (trim (cadr maybe-rgb) ")")))
          (map string->number (split number-part ","))))))


(define (process-point p)
  (let ((parts (split p " "))
        (p (string->number (trim (cadr parts) "%")))
        (c (process-color (car parts))))
    `(s 'set-to-at (color ',c) ,p)))


(define (process-gradient duration step gradient)
  (let ((points (cdr (split (trim gradient "") ", "))))
    (eval `(let ((s (color-shift)))
             (s 'duration-in-millis ,duration)
             (s 'step-time-in-millis ,step)
             ,@(map process-point points)
             (s 'json)))))


;;; --------------------------------------------------------------------------------
;;; morse code doodling

(define color-black (color '(0 0 0)))
(define color-white (color '(255 255 255)))

(define (char->code step char)
  (case char
    (("-") `((s 'set-to color-white)
             (s 'pause (- 500 ,step))
             (s 'set-to color-black)))
    ((".") `((s 'set-to color-white)
             (s 'pause (- 100 ,step))
             (s 'set-to color-black)))
    (else `((s 'pause (- 1000 ,step))))))

(define (join a-list separator)
  (let ((first-item (car a-list)))
    (cons first-item (flatten (map (lambda (x) `((,separator ,@x))) (cdr a-list))))))


(define (morse-code step morse-string)
  (let ((chars (split morse-string "")))
    `(let ((s (color-shift)))
       (s 'step-time-in-millis ,step)
       ,@(flatten (join (map (lambda (x) (char->code ,step x)) chars) '(s 'pause (- 100 ,step))))
       (s 'json))))


;;; convert a string of morsecode to a color sequence.
(define (morse step morse-string)
  (eval (morse-code step morse-string)))


(define morse-table
  '(("A" . ".-")
    ("B" . "-...")
    ("C" . "-.-.")
    ("D" . "-..")
    ("E" . ".")
    ("F" . "..-.")
    ("G" . "--.")
    ("H" . "....")
    ("I" . "..")
    ("J" . ".---")
    ("K" . "-.-")
    ("L" . ".-..")
    ("M" . "--")
    ("N" . "-.")
    ("O" . "---")
    ("P" . ".--.")
    ("Q" . "--.-")
    ("R" . ".-.")
    ("S" . "...")
    ("T" . "-")
    ("U" . "..-")
    ("V" . "...-")
    ("W" . ".--")
    ("X" . "-..-")
    ("Y" . "-.--")
    ("Z" . "--..")
    ("a" . ".-")
    ("b" . "-...")
    ("c" . "-.-.")
    ("d" . "-..")
    ("e" . ".")
    ("f" . "..-.")
    ("g" . "--.")
    ("h" . "....")
    ("i" . "..")
    ("j" . ".---")
    ("k" . "-.-")
    ("l" . ".-..")
    ("m" . "--")
    ("n" . "-.")
    ("o" . "---")
    ("p" . ".--.")
    ("q" . "--.-")
    ("r" . ".-.")
    ("s" . "...")
    ("t" . "-")
    ("u" . "..-")
    ("v" . "...-")
    ("w" . ".--")
    ("x" . "-..-")
    ("y" . "-.--")
    ("z" . "--..")
    ("0" . "-----")
    ("1" . ".----")
    ("2" . "..---")
    ("3" . "...--")
    ("4" . "....-")
    ("5" . ".....")
    ("6" . "-....")
    ("7" . "--...")
    ("8" . "---..")
    ("9" . "----.")
    ("." . ".-.-.-")
    ("," . "--..--")
    (":" . "---...")
    ("?" . "..--..")
    ("'" . ".----.")
    ("-" . "-....-")
    ("/" . "-..-.")
    ("@" . ".--.-.")
    ("=" . "-...-")
    (" " . "  ")))



(define (lookup-morse-for ch)
  (str (cdr (assoc ch morse-table)) " "))

;;; convert a string to morse dots and dashes

(define (morsify s)
  (reduce str "" (map lookup-morse-for (split s ""))))




