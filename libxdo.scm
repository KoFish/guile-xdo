(define-module (xdo libxdo)
               #:use-module (ice-9 optargs)
               #:use-module (ice-9 match)
               #:export (new-xdo
                         lib:xdo-version
                         xdo-move-mouse
                         xdo-mouse-button-up
                         xdo-mouse-button-down 
                         xdo-get-mouse-location 
                         xdo-get-window-at-mouse 
                         xdo-wait-for-mouse-move
                         xdo-click
                         xdo-enter-text
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
(define* (xdo-move-mouse xdo xy #:optional rest)
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
         (lib:xdo-get-mouse-location xdo (car with-window)))

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
         (let ((sane (eq? rest '(#:sane))))
           (if sane
             (lib:xdo-get-focused-window-sane xdo)
             (lib:xdo-get-focused-window xdo))))

(define (xdo-get-window-at-mouse xdo) (lib:xdo-get-window-at-mouse xdo))
(define (xdo-select-window-with-click xdo) (lib:xdo-select-window-with-click xdo))

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
         (let ((window (or window (xdo-get-active-window xdo))))
           (lib:xdo-get-window-location xdo window))) 

(define* (xdo-get-window-size xdo #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (lib:xdo-get-window-size xdo window))) 

(define (xdo-get-active-window xdo)
         (lib:xdo-get-active-window xdo)) 

(define (xdo-kill-window xdo window)
         (eq? 0 (lib:xdo-kill-window xdo window))) 

(define* (xdo-get-window-property xdo name #:key window) 
         (let ((window (or window (xdo-get-active-window xdo)))) 
           (lib:xdo-get-window-property xdo window name))) 

(define (xdo-set-number-of-desktops xdo n)
         (eq? 0 (lib:xdo-set-number-of-desktops xdo n)))

(define (xdo-get-number-of-desktops xdo)
         (lib:xdo-get-number-of-desktops xdo))

(define (xdo-get-current-desktop xdo)
         (lib:xdo-get-current-desktop xdo))

(define* (xdo-set-current-desktop xdo desktop #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (eq? 0 (lib:xdo-set-current-desktop xdo desktop))))

(define (xdo-get-active-modifiers xdo)
         (lib:xdo-get-active-modifiers xdo))

(define* (xdo-set-active-modifiers xdo mods #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (lib:xdo-set-active-modifiers xdo window mods)))

(define* (xdo-clear-active-modifiers xdo mods #:key window)
         (let ((window (or window (xdo-get-active-window xdo))))
           (lib:xdo-clear-active-modifiers xdo window mods)))

#|
        "lib:xdo-search-windows",

        "lib:xdo-get-input-state",
        "lib:xdo-get-symbol-map",

        "lib:xdo-get-active-modifiers",
        "lib:xdo-set-active-modifiers",
        "lib:xdo-clear-active-modifiers",

        "lib:xdo-get-desktop-viewport",
        "lib:xdo-set-desktop-viewport",

        "lib:xdo-get-viewport-dimensions",

        "lib:xdo-find-window-client",

        "lib:xdo-get-window-name",

        "lib:xdo-disable-feature",
        "lib:xdo-enable-feature",
        "lib:xdo-has-feature",
|#
