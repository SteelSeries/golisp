;;; -*- mode: Scheme -*-

;;; Copyright 2015 SteelSeries ApS. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.

;;; Frame support functions


;;; Output a frame in a semi-formatted way

(define (format-frame port frame)
    (define (format-frame-helper port frame level)
      (format port "{~%")
      (for-each (lambda (k)
                  (let* ((value (get-slot frame k)))
                    (format port "~V@A~S " (* 2 level) ""  k)
                    (if (frame? value)
                        (format-frame-helper port value (succ level))
                        (format port "~S~%" value))))
                (frame-keys frame))
      (format port "~V@A}~%" (* 2 (pred level)) ""))
    
    (format-frame-helper port frame 1))

(define (load-csgo-data filename)
  (let ((port (open-input-file filename)))
    (do ((f (read port) (read port))
         (coll '() (cons (eval f) coll)))
        ((eof-object? f)
         (close-port port)
         (reverse coll)))))

(define datafile "/Users/dastels/Projects/SteelSeries/SSEnext-Core/csgo-data.txt")


(define (fetch path state)
  (if (nil? path)
      state
      (let ((key (car path))
            (remaining-path (cdr path)))
        (cond ((eq? key *:) (fetch remaining-path (get-slot state (car (frame-keys state)))))
              ((has-slot? state key) (fetch remaining-path (get-slot state key)))
              (else nil)))))


(define (compute-delta previously prefix-path state)
  (let ((result {}))
    (map (lambda (key)
           (let ((new-path (append prefix-path key))
                 (value (get-slot previously key)))
             (set-slot! result key (if (frame? value)
                                       (compute-delta value new-path state)
                                       (cons value (fetch new-path state))))))
         (frame-keys previously))
    result))


(define (compute-state-change state)
  (let ((timestamp (timestamp: (provider: state)))
        (data (if (previously:? state)
                  (compute-delta (previously: state) '()  state)
                  {})))
    (timestamp:! data timestamp)
    data))

;;; Change characterizations
;;; ammo - ammo level in the active weapon clip has decreased
;;;   player: weapons: weapon_*: ammo_clip: decreased
;;; empty - clip empty
;;;   player: weapons: weapon_*: ammo_clip: -> 0
;;; reloading - active weapon has been reloaded
;;;   player: weapons: weapon_*: state: "active" -> "reloading"
;;; reloaded - reloading weapon is active again
;;;   player: weapons: weapon_*: state: "reloading" -> "active"
;;; kill - make a kill
;;;   player: state: round_kills: increased
;;; headshot - got a headshot
;;;   player: state: round_killhs: increased
;;; newround - round started
;;;   round: phase: "freezetime" -> "live"
;;; dead - got killed
;;; hit - took damage
;;;   player: state: health: decreased
;;; planted - bomb has been planted
;;;  round: bomb: -> "planted"
;;; defused - bomb has been defused
;;;   round: bomb: "plated" -> "defused"

(define (characterize-ammo state-change)
  (let ((ammo-change (fetch '(player: weapons: *: ammo_clip:) state-change)))
    (if (and ammo-change
             (< (cdr ammo-change) (car ammo-change)))
        'ammo
        nil)))


(define *characterizers* (list characterize-ammo))

(define (characterize state-change)
  (remove nil? (map (lambda (characterizer) (characterizer state-change)) *characterizers*)))
