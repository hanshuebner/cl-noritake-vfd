;; -*- Lisp -*-

(defpackage :vfd
  (:use :cl)
  (:export #:open-port
           #:send
           #:macro
           #:home
           #:clear-eol
           #:test
           #:cursor-position
           #:set-area
           #:clear-area
           #:invert-area
           #:set-outline
           #:clear-outline
           #:set-pixel
           #:clear-pixel
           #:graphic-write
           #:reset
           #:write-mode
           #:set-macro
           #:brightness
           #:checksum
           #:erase-macros
           #:lock-eeprom
           #:unlock-eeprom
           #:power-on
           #:power-off
           #:hex-mode
           #:binary-mode
           #:set-serial
           #:enable-io-port
           #:set-port-lines
           #:read-port
           #:enable-key-scanning
           #:set-mini-font
           #:set-5x7-font
           #:set-10x14-font
           #:graphic-area-write
           #:window-1-select
           #:window-2-select
           #:window-define
           #:window-mode
           #:window-show
           #:window-kill
           #:window-flash
           #:window-flash-speed
           #:window-wipe-effect
           #:window-wipe-speed
           #:window-pattern-select
           #:window-pattern-data
           #:window-pattern-option
           #:scroll-text-in-window
           #:scroll-speed
           #:select-extended-font
           #:draw-line
           #:auto-fade
           #:command-delay))

(in-package :vfd)

(defvar *port* nil)
(defparameter *default-devicename* (first (directory "/dev/cu.usbserial*")))

(defun open-port (&optional (device-pathname *default-devicename*))
  (when (and (null device-pathname)
             (null *default-devicename*))
    (error "no port name specified and no default define name set"))
  (when *port*
    (close *port*))
  (setf *port* (open device-pathname :direction :io :if-exists :append :element-type '(unsigned-byte 8)))
  (trivial-shell:shell-command (format nil "stty -f ~A 57600" device-pathname)))

(defun send-byte (byte)
  (format t "~X " byte)
  (write-byte byte *port*))

(defun send (&rest args)
  (dolist (arg args)
    (etypecase arg
      (character (send-byte (char-code arg)))
      (integer (send-byte arg))
      ((vector (integer))
       (loop for byte across arg
             do (send-byte byte)))
      (string
       (loop for byte across (flex:string-to-octets arg :external-format :latin1)
             do (send-byte byte)))))
  (terpri)
  (finish-output *port*))

(defmacro define-simple-command (name &rest args)
  `(defun ,name () (send ,@args)))

(defun macro (macro-number)
  (assert (<= 1 macro-number 7) () "assert macro number must be between 1 and 7")
  (send macro-number))

(define-simple-command home #x0b)
(define-simple-command clear-eol #x0e)
(define-simple-command test #x0f)

(defun cursor-position (x y)
  (send #x10 x y))

(defun set-area (left top right bottom)
  (send #x11 left top right bottom))

(defun clear-area (left top right bottom)
  (send #x12 left top right bottom))

(defun invert-area (left top right bottom)
  (send #x13 left top right bottom))

(defun set-outline (left top right bottom)
  (send #x14 left top right bottom))

(defun clear-outline (left top right bottom)
  (send #x15 left top right bottom))

(define-simple-command set-pixel #x16)
(define-simple-command clear-pixel #x17)

(defun graphic-write (data)
  (check-type data (vector (integer)))
  (send #x18 (length data) data))

(define-simple-command reset #x19)

(defun write-mode (&key
                     (graphic-data-orientation :horizontal)
                     (cursor-movement :horizontal)
                     (cursor-direction :forward)
                     cursor-enable-p
                     cursor-blink-p
                     (pen-type :overwrite))
  (send #x1a (logior (ecase graphic-data-orientation
                       (:horizontal 0)
                       (:vertical #x80))
                     (ecase cursor-movement
                       (:horizontal 0)
                       (:vertical #x40))
                     (ecase cursor-direction
                       (:forward 0)
                       (:backwards #x20))
                     (if cursor-enable-p #x10 0)
                     (if cursor-blink-p #x08 0)
                     (ecase pen-type
                       (:overwrite 0)
                       (:and 1)
                       (:or 2)
                       (:xor 3)))))

(defmacro set-macro (macro-number &body body)
  (assert (<= 0 macro-number 7) () "assert macro number must be between 0 and 7")
  (alexandria:with-gensyms (data)
    `(let ((,data (flex:with-output-to-sequence (*port* :element-type 'number)
                    ,@body)))
       (send #x1b ,macro-number (length ,data) ,data))))

(defun brightness (level)
  (assert (<= 0 level 7) () "brightness level must be between 0 and 7")
  (send #x1b (+ #xf8 level)))

(defun checksum ()
  (warn "checksum not currently supported")
  (send #x1b #x43)
  (read-byte *port*))

(define-simple-command erase-macros #x1b #x4d)
(define-simple-command lock-eeprom #x1b #x4c)
(define-simple-command unlock-eeprom #x1b #x55)
(define-simple-command power-on #x1b #x50)
(define-simple-command power-off #x1b #x46)
(define-simple-command hex-mode #x1b #x48)
(define-simple-command binary-mode #x1b #x42)

(defun set-serial (speed &key automatic-send-p packet-mode-p bufferp parityp)
  (send #x1b #x49 (logior (if automatic-send-p #x80 0)
                         (if packet-mode-p #x40 0)
                         (if bufferp #x20 0)
                         (if parityp #x08 0)
                         (ecase speed
                           (4800 0)
                           (9600 1)
                           (19200 2)
                           (38400 3)
                           (57600 4)
                           (76800 5)
                           (1200 6)
                           (2400 7)))))

(defun enable-io-port (direction-mask)
  (send #x1b #x44 direction-mask))

(defun set-port-lines (mask)
  (send #x1b #x4f mask))

(defun read-port ()
  (send #x1b #x52)
  (read-byte *port*))

(define-simple-command enable-key-scanning #x1b #x4b)
(define-simple-command set-mini-font #x1c)
(define-simple-command set-5x7-font #x1d)
(define-simple-command set-10x14-font #x1e)

(defun graphic-area-write (left top right bottom data)
  (send #x1f left top right bottom data))

(define-simple-command window-1-select #x1b #x80)
(define-simple-command window-2-select #x1b #x81)

(defun window-define (left top right bottom)
  (send #x1b #x82 left top right bottom))

(defun window-mode (mode)
  (send #x1b #x83 (ecase mode
                    (:invert 0)
                    (:clear 1)
                    (:fill 2)
                    (:pattern 3))))

(define-simple-command window-show #x1b #x84)
(define-simple-command window-kill #x1b #x85)

(defun window-flash (number-of-flashes)
  (send #x1b #x86 (if (eql number-of-flashes :infinite)
                      255
                      number-of-flashes)))

(defun window-flash-speed (on off)
  (flet ((encode-speed (time)
           (ecase time
             (15 0)
             (30 1)
             (45 2)
             (100 3)
             (150 4)
             (200 5)
             (250 6)
             (350 7)
             (500 8)
             (750 9)
             (1.0 10)
             (1.5 11)
             (2.0 12)
             (2.5 13)
             (3.0 14)
             (3.5 15))))
    (send #x1b #x87 (logior (ash (encode-speed on) 4)
                            (encode-speed off)))))

(defun window-wipe-effect (wipe)
  (send #x1b #x88
        (ecase wipe
          (:left-to-right-cover #x00)
          (:right-to-left-cover #x01)
          (:top-to-bottom-cover #x02)
          (:bottom-to-top-cover #x03)
          (:left-to-right-uncover #x04)
          (:right-to-left-uncover #x05)
          (:top-to-bottom-uncover #x06)
          (:bottom-to-top-uncover #x07)
          (:horizontal-center-to-edge-uncover #x08)
          (:horizontal-edge-to-center-uncover #x09)
          (:vertical-center-to-edge-uncover #x0a)
          (:vertical-edge-to-center-uncover #x0b))))

(defun window-wipe-speed (speed)
  (assert (<= 0 speed 15) () "wipe speed must be between 0 and 15")
  (send #x1b #x89 speed))

(defun window-pattern-select (pattern)
  (assert (<= 0 pattern 15) () "window pattern must be between 0 and 15")
  (send #x1b #x8d pattern))

(defun window-pattern-data (pattern)
  (assert (= (length pattern) 32) () "Expected 32 bytes (16x16 image)")
  (send #x1b #x8e pattern))

(defun window-pattern-option (&key invert-pattern-data-p pattern-alignment-p topp leftp)
  (send #x1b #x8f (logior (if invert-pattern-data-p #x08 0)
                          (if pattern-alignment-p #x04 0)
                          (if topp #x02 0)
                          (if leftp #x01 0))))

(defun scroll-text-in-window (text
                              &key pad-with-spaces-p scroll-window-contents-p (direction :up) (repeat-count 0))
  (send #x1b #x90
        (logior (if pad-with-spaces-p #x20 0)
                (if scroll-window-contents-p #x10 0)
                (ecase direction
                  (:up 0)
                  (:down 1)
                  (:left 2)
                  (:right 3)))
        repeat-count
        text
        0))

(defun scroll-speed (speed)
  (assert (<= 0 speed 8) () "scroll speed must be between 0 and 8")
  (send #x1b #x91 speed))

(defun select-extended-font (name &key (proportionalp t) (horizontal-spacing 1))
  (assert (<= 1 horizontal-spacing 8) () "horizontal spacing must be between 1 and 8")
  (send #x1b #x98
        (logior (ecase name
                  (:5x5-ascii-mini 0)
                  (:5x7-ascii 1)
                  (:10x14-ascii 2)
                  (:7x15-ascii 3)
                  (:5x7-cyrillic 4)
                  (:10x14-cyrillic 5))
                (if proportionalp #x08 0)
                (ash (1- horizontal-spacing) 4))))

(defun draw-line (x y)
  (send #x1b #x9a x y))

(defun auto-fade (luminance speed)
  (assert (<= 0 luminance 7) () "luminance level must be between 0 and 7")
  (assert (<= 0 speed 3) () "speed must be betweeen 0 and 3")
  (send #x1b #x9c (logior luminance
                          (ash speed 4))))

(defun command-delay (wait-for)
  (send #x1b #x9f (etypecase wait-for
                    (symbol
                     (ecase wait-for
                       (:display-scan 0)
                       (:scroll-finished #xf8)
                       (:window-1-flash #xfa)
                       (:window-2-flash #xfb)
                       (:window-1-wipe #xfc)
                       (:window-2-wipe #xfd)))
                    (number
                     (assert (<= #x01 wait-for #xf0) () "numeric delay must be between 0 and 240")
                     wait-for))))

