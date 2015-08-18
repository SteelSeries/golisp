;;; -*- mode: Scheme -*-

(context "system"

         ()

         ;; (describe timing
         ;;           (let ((t (time (sleep 2000))))
         ;; (>= t 2000)))

         (it apply
             (assert-eq (apply + '(1 2))
                        3)

             (assert-error (apply 5 '(1 2))) ;1st arg must be a function
             (assert-error (apply + 1 2))) ;last are must be a list

         (it eval
             (assert-eq (+ 1 2) 3)
             (assert-error (5 1 2))
             (assert-error ('list 1 2))))
