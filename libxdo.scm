;;; Thin-wrapper of libxdo for Guile 
;;; Copyright (C) 2012  Krister Svanlund <krister.svanlund@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;; 

;;; Code:
(define-module (xdo libxdo)
               #:use-module (ice-9 optargs)
               #:use-module (ice-9 match)
               #:export (new-xdo
                         lib:xdo-version
                         lib:xdo-enable-feature
                         lib:xdo-disable-feature
                         lib:xdo-has-feature
                         xdo-move-mouse
                         xdo-mouse-button-up
                         xdo-mouse-button-down 
                         xdo-get-mouse-location 
                         xdo-wait-for-mouse-move
                         xdo-click
                         xdo-enter-text
                         xdo-send-keysequence
                         xdo-move-window
                         xdo-set-window-size
                         xdo-set-window-property
                         xdo-set-window-class
                         xdo-set-urgency
                         xdo-set-override-redirect
                         xdo-wait-for-mouse-move
                         xdo-wait-for-window
                         xdo-raise
                         xdo-map
                         xdo-unmap
                         xdo-activate
                         xdo-minimize
                         xdo-reparent-window
                         xdo-focus-window
                         xdo-kill-window
                         xdo-get-pid
                         xdo-get-window-location
                         xdo-get-window-size
                         xdo-get-window-name
                         xdo-get-window-property
                         xdo-set-number-of-desktops
                         xdo-get-number-of-desktops
                         xdo-get-current-desktop
                         xdo-set-current-desktop
                         xdo-get-active-window
                         xdo-get-focused-window
                         xdo-get-window-at-mouse
                         xdo-select-window-with-click
                         xdo-get-desktop-viewport
                         xdo-set-desktop-viewport
                         xdo-get-desktop-for-window
                         xdo-get-viewport-dimensions
                         xdo-find-window-client
                         xdo-search-windows
                         xdo-get-active-modifiers
                         xdo-set-active-modifiers
                         xdo-clear-active-modifiers
                         xdo-get-input-state
                         xdo-get-symbol-map
                         ))

(load-extension "./libxdo_guile.so" "scm_init_xdo_libxdo_module")

(define* (new-xdo #:optional display) 
         (if display
           (lib:xdo-new display)
           (lib:xdo-new))) 

;; Arguments are:
;; xdo - The xdo instance
;; x y - Position to move the cursor to
;; [#:window window] - Set the position relative to the window
;; [#:relative] | [screen] - With #:relative make the position
;;       relative to the current position, without this the 
;;       fourth argument should be what sceen the position 
;;       should be relative too.
(define (xdo-move-mouse xdo xy . rest)
  (let ((window (if rest (memq #:window rest) #f))
        (relative (if rest (memq #:relative rest) #f))
        (x (car xy)) (y (cadr xy)))
    (eq? 0 (if (and window (> (length window) 2))
             (lib:xdo-move-mouse-relative-to-window xdo window x y)
             (if relative
               (lib:xdo-move-mouse-relative xdo x y)
               (if (and rest (> (length rest) 1))
                 (lib:xdo-move-mouse xdo x y (car rest))
                 (lib:xdo-move-mouse xdo x y 0)))))))

(define* (xdo-mouse-button-up xdo button #:key window)
         (eq? 0 (lib:xdo-mouse-up xdo (or window (xdo-get-active-window xdo)) button)))

(define* (xdo-mouse-button-down xdo button #:key window)
         (eq? 0 (lib:xdo-mouse-down xdo (or window (xdo-get-active-window xdo)) button)))

(define* (xdo-get-mouse-location xdo #:optional with-window)
         (let ((ret (lib:xdo-get-mouse-location xdo (car with-window))))
           (if (pair? ret) ret #f)))

(define* (xdo-click xdo button #:key repeat delay window)
         (eq? 0 (lib:xdo-click-window xdo (or window (xdo-get-active-window xdo)) button repeat delay)))

(define* (xdo-enter-text xdo string #:key window delay)
         (eq? 0 (lib:xdo-enter-text-window xdo (or window (xdo-get-active-window xdo)) string delay)))

(define* (xdo-send-keysequence xdo sequence #:optional rest #:key window delay)
         (let ((window (or window (xdo-get-active-window xdo)))
               (up (memq #:up rest))
               (down (memq #:down rest)))
           (eq? 0 (lib:xdo-send-keysequence-window xdo window sequence delay (if up 'up
                                                                               (if down 'down
                                                                                 'both))))))

(define* (xdo-move-window xdo xy #:key window)
         (if (not (eq? 2 (length xy))) #f
           (let ((x (car xy))
                 (y (cadr xy))
                 (window (or window (xdo-get-active-window xdo))))
             (eq? 0 (lib:xdo-move-window xdo window x y)))))

;;(define* (xdo-translate-window-with-sizehint xdo width height #:key window)
;;         (let* ((window (or window (xdo-get-window-at-mouse xdo)))
;;                (ret (lib:xdo-translate-window-with-sizehint xdo window width height)))
;;           (if (list? ret) ret #f)))

(define* (xdo-set-window-size xdo width height #:optional rest #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (let ((ret (if (memq #:with-sizehint rest)
                        (lib:xdo-translate-window-with-sizehint xdo window width height)
                        (lib:xdo-set-window-size xdo window width height 0))))
             (if (list? ret)
               ret
               (eq? 0 ret)))))

(define* (xdo-set-window-property xdo property value #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-window-property xdo window property value))))

(define* (xdo-set-window-class xdo class value #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-window-class xdo window class value))))

(define* (xdo-set-urgency xdo urg #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-window-urgency xdo window (if urg 0 1)))))

(define* (xdo-set-override-redirect xdo ored #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-window-override-redirect xdo window (if ored 0 1)))))

(define* (xdo-get-focused-window xdo #:optional rest)
         (let* ((sane (eq? rest '(#:sane)))
                (ret (if sane
                       (lib:xdo-get-focused-window-sane xdo)
                       (lib:xdo-get-focused-window xdo))))
           (if (xdo-window? ret) ret #f)))

(define (xdo-get-window-at-mouse xdo)
  (let ((ret (lib:xdo-get-window-at-mouse xdo)))
    (if (xdo-window? ret) ret #f)))

(define (xdo-select-window-with-click xdo)
  (let ((ret (lib:xdo-select-window-with-click xdo)))
    (if (xdo-window? ret) ret #f)))

(define* (xdo-wait-for-mouse-move xdo #:key to from)
         (let ((coord (or to from)))
           (if (eq? 2 (length coord))
             (eq? 0 (lib:xdo-wait-for-mouse-move xdo (car coord) (cadr coord) (if to #t #f)))
             (throw 'bad-coords "Bad number of coordinates"))))

(define* (xdo-wait-for-window xdo window state)
         (eq? 0 (match state
                       (#:active (lib:xdo-wait-for-window-active xdo window 1))
                       (#:inactive (lib:xdo-wait-for-window-active xdo window 0))
                       (#:focus (lib:xdo-wait-for-window-focus xdo window 1))
                       (#:unfocus (lib:xdo-wait-for-window-focus xdo window 0)))))

(define* (xdo-focus-window xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-focus-window xdo window))))

(define* (xdo-raise xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-raise-window xdo window))))

(define* (xdo-map xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-map-window xdo window))))

(define* (xdo-unmap xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-unmap-window xdo window))))

(define* (xdo-activate xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-activate-window xdo window)))) 

(define* (xdo-minimize xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-minimize-window xdo window)))) 

(define (xdo-reparent-window xdo source-window target-window)
         (eq? 0 (lib:xdo-reparent-window xdo source-window target-window))) 

(define* (xdo-get-pid xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (lib:xdo-get-pid-window xdo window))) 

(define* (xdo-get-window-location xdo #:key window)
         (let* ((window (or window (xdo-get-active-window xdo)))
                (ret (lib:xdo-get-window-location xdo window)))
           (if (pair? ret) ret #f))) 

(define* (xdo-get-window-size xdo #:key window)
         (let* ((window (or window (xdo-get-active-window xdo)))
                (ret (lib:xdo-get-window-size xdo window)))
           (if (pair? ret) ret #f))) 

(define (xdo-get-active-window xdo)
  (let ((ret (lib:xdo-get-active-window xdo)))
    (if (xdo-window? ret) ret #f))) 

(define (xdo-kill-window xdo window)
         (eq? 0 (lib:xdo-kill-window xdo window))) 

(define* (xdo-get-window-property xdo name #:key window) 
         (let* ((window (or window (xdo-get-active-window xdo))) 
                (ret (lib:xdo-get-window-property xdo window name)))
           (if (bytevector? ret) ret #f))) 

(define (xdo-set-number-of-desktops xdo n)
         (eq? 0 (lib:xdo-set-number-of-desktops xdo n)))

(define (xdo-get-number-of-desktops xdo)
         (let ((ret (lib:xdo-get-number-of-desktops xdo)))
           (if (>= ret 0) ret #f)))

(define (xdo-get-current-desktop xdo)
         (let ((ret (lib:xdo-get-current-desktop xdo)))
           (if (>= ret 0) ret #f)))

(define* (xdo-set-current-desktop xdo desktop #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-current-desktop xdo desktop))))

(define (xdo-get-active-modifiers xdo)
  (let ((ret (lib:xdo-get-active-modifiers xdo)))
    (if (list? ret) ret #f)))

(define* (xdo-set-active-modifiers xdo mods #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-active-modifiers xdo window mods))))

(define* (xdo-clear-active-modifiers xdo mods #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-clear-active-modifiers xdo window mods))))

(define* (xdo-search-windows xdo #:optional rest #:key title winclass
                             winclassname winname pid max-depth
                             screen desktop)
         (let ((only-visible (if (memq #:only-visible (or rest '())) #t #f))
               (all (if (memq #:any (or rest '())) #f #t)))
           (display rest) (newline)
           (let ((search (make-xdo-search title winclass winclassname
                                          winname pid max-depth
                                          only-visible screen desktop
                                          (if all 0 1))))
             (let ((ret (lib:xdo-search-windows xdo search)))
               (if (eq? ret -1) #f ret)))))

(define xdo-get-input-state lib:xdo-get-input-state)
(define xdo-get-symbol-map lib:xdo-get-symbol-map)

(define* (xdo-get-viewport-dimensions xdo #:key screen)
         (let* ((screen (or screen 0))
                (ret (lib:xdo-get-viewport-dimensions xdo screen)))
           (if (pair? ret) ret #f)))

(define (xdo-get-desktop-viewport xdo)
  (let ((ret (lib:xdo-get-desktop-viewport xdo)))
    (if (pair? ret) ret #f)))

(define (xdo-set-desktop-viewport xdo xy)
  (let ((ret (lib:xdo-set-desktop-viewport xdo (car xy) (cadr xy))))
    (if (eq? -1 ret) #f #t)))

(define* (xdo-get-desktop-for-window xdo #:key window)
         (let* ((window (or window (xdo-get-active-window xdo)))
                (ret (lib:get-desktop-for-window xdo window)))
           (if (>= ret 0) ret #f)))

(define* (xdo-find-window-client xdo #:key window direction)
         (let ((window (or window (xdo-get-active-window xdo)))
               (direction (if (eq? direction 'parent) 0 1 )))
           (let ((ret (lib:xdo-find-window-client xdo window direction)))
             (if (xdo-window? ret) ret #f))))

(define* (xdo-get-window-name xdo #:key window)
         (let* ((window (or window (xdo-get-active-window xdo)))
                (ret (lib:xdo-get-window-name xdo window)))
           (if (pair? ret) ret #f)))
