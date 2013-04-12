;; -*- Lisp -*-

;; Load :cl-noritake-vfd, then open this file in slime and evaluate
;; the (example) forms using C-M-x in Slime.

(defpackage :examples
  (:use :cl :vfd))

(in-package :examples)

(defmacro example (&body body)
  `(progn
     (unless vfd::*port*
       (open-port))
     (reset)
     ,@body))

;; 7.1 Character Writes (p20)

#+(or)
(example
  (set-5x7-font)
  (cursor-position #x46 #x17)
  (send "NORITAKE ITRON")
  (cursor-position #x50 #x26)
  (send "VFD MODULES"))

#+(or)
(example
  (set-10x14-font)
  (cursor-position #x1e #x25)
  (send "GU240x64D-K612A8"))

#+(or)
(example
  (set-5x7-font)
  (cursor-position #x02 #x09)
  (send "ENTER NAME:")
  (write-mode :cursor-enable-p t :cursor-blink-p t))

;; 7.2 Area Commands (p21)

#+(or)
(example
  (cursor-position #x46 #x23) (send "SETUP")
  (cursor-position #x70 #x23) (send "PRINT")
  (cursor-position #x9e #x23) (send "RUN")
  (set-outline #x44 #x1a #x64 #x24)
  (set-outline #x6e #x1a #x8e #x24)
  (set-outline #x9c #x1a #xb0 #x24)
  (set-area #x45 #x25 #x65 #x25) (set-area #x65 #x1b #x65 #x25)
  (set-area #x6f #x25 #x8f #x25) (set-area #x8f #x1b #x8f #x25)
  (set-area #x9d #x25 #xb1 #x25) (set-area #xb1 #x1b #xb1 #x25)
  (invert-area #x9d #x1b #xaf #x23))

;; 7.3 Write Modes (p21)

#+(or)
(example
  (set-area #x20 #x10 #xb7 #x2f)
  (write-mode :pen-type :xor)
  (cursor-position #x46 #x23)
  (send "INVERTED TEXT"))

#+(or)
(example
  (cursor-position #x28 #x0f)
  (send "PROGRESS")
  (set-outline #x00 #x12 #x7f #x1c)
  (set-area #x00 #x12 #x3f #x1c)
  (write-mode :pen-type :xor)
  (cursor-position #x3a #x1b)
  (send "50%"))

;; 7.4 Graphics Writes

#+(or)
(example
  (write-mode :cursor-movement :vertical)
  (cursor-position #x1f #x1c)
  (graphic-write #(#x1c #x5c #x48 #x3e #x1d #x1d #x14 #x36)))

#+(or)
(example
  (write-mode :cursor-movement :vertical)
  (cursor-position #x1f #x1c)
  (graphic-write #(#x1c #x5c #x48 #x3e #x1d #x1d #x14 #x36))
  (write-mode :graphic-data-orientation :vertical)
  (cursor-position #x48 #x16)
  (graphic-write #(#x00 #x00 #x00 #x00 #x07 #x04 #xc7 #xfe #xfe #x72 #x73
                   #x32 #x3e #x3f #x1d #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (cursor-position #x48 #x1c)
  (graphic-write #(#x00 #x3c #x42 #x81 #xb9 #xc1 #x42 #x7c #x20 #xd8
                   #xfc #x3c #xfc #xca #x49 #xb1 #x89 #x42 #x3c #x00)))

;; Macros (p23)

#+(or)
(example
  (set-macro 0
    (cursor-position #x1f #x0d)
    (send "PLEASE WAIT")
    (cursor-position #x07 #x19)
    (send "INITIALIZING SYSTEM")))

#+(or)
(example
  (set-macro 1
    (send #x1a #x40 #x18 #x08 #x1c #x5c
          #x48 #x3e #x1d #x1d #x14 #x36))
  (cursor-position #x0f #x09)
  (macro 1)
  (cursor-position #x77 #x08)
  (macro 1)
  (cursor-position #x36 #x12)
  (macro 1)
  (cursor-position #x4f #x18)
  (macro 1))

#+(or)
(example
  (set-macro 2
    (reset)
    (set-outline #x02 #x02 #x7d #x1d)
    (set-area #x00 #x00 #x04 #x04)
    (set-area #x00 #x1b #x04 #x1f)
    (set-area #x7b #x00 #x7f #x04)
    (set-area #x7b #x1b #x7f #x1f))
  (macro 2)
  (cursor-position #x1c #x13)
  (send "SYSTEM READY"))

;; 7.7 Windows (p24)
#+(or)
(example
  (select-extended-font :5x7-ascii :proportionalp t)
  (cursor-position #x0a #x14)
  (send "WELCOME TO NORITAKE ITRON")
  (window-1-select)
  (window-define #x10 #x0a #x2f #x16)
  (window-mode :fill)
  (window-show)
  (window-2-select)
  (window-define #x5f #x0a #x88 #x16)
  (window-mode :invert)
  (window-show))

;; 7.8 Flashing (p25)
#+(or)
(example
  (select-extended-font :5x7-ascii :proportionalp t)
  (cursor-position #x29 #x0f)
  (send "WARNING!!!")
  (cursor-position #x15 #x18)
  (send "Power Overload")
  (window-1-select)
  (window-define #x28 #x07 #x56 #x0f)
  (window-mode :invert)                 ; or :clear
  (window-flash-speed 150 150)
  (window-flash :infinite))

;; 7.9 Scrolling Text
#+(or)
(example
  (window-1-select)
  (window-define #x00 #x00 #x7f #x17)
  (select-extended-font :7x15-ascii :proportionalp t)
  (scroll-text-in-window "Scrolling Text Message" :direction :left :repeat-count 1))

;; 7.10 Pattern (p26)
#+(or)
(example
  (window-1-select)
  (window-define #x00 #x00 #xef #x40)
  (window-mode :pattern)
  (window-show)
  (window-pattern-data #(#x86 #xfc #xfe #x86 #x7e #x86 #x00 #x86 #xfc #x86 #x86 #xfe #x86 #x7e #x86 #x00
                         #x86 #xfc #xfe #x86 #x7e #x86 #x00 #x86 #xfc #x86 #x86 #xfe #x86 #x7e #x86 #x00))
  (window-pattern-option :invert-pattern-data-p t))

;; 7.11 Wipe Effects (p27)
#+(or)
(example
  (window-1-select)
  (window-define #x00 #x00 #xef #x40)
  (window-mode :pattern)
  (window-wipe-effect :top-to-bottom-cover)
  (command-delay :window-1-wipe)
  (window-pattern-select 7)
  (window-pattern-option :pattern-alignment-p t :topp t)
  (select-extended-font :7x15-ascii :proportionalp t)
  (cursor-position #x1d #x26)
  (send "WELCOME TO NORITAKE ITRON")
  (window-wipe-effect :bottom-to-top-uncover))
