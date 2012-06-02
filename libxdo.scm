(define-module (xdo libxdo)
               #:use-module (ice-9 optargs)
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
                      (if (> (length rest) 1)
                        (lib:xdo-move-mouse xdo x y (car rest))
                        (lib:xdo-move-mouse xdo x y 0)))))))

(define* (xdo-mouse-button-up xdo button #:key window)
         (eq? 0 (lib:xdo-mouse-up xdo (or window (xdo-get-window-at-mouse xdo)) button)))

(define* (xdo-mouse-button-down xdo button #:key window)
         (eq? 0 (lib:xdo-mouse-down xdo (or window (xdo-get-window-at-mouse xdo)) button)))

(define* (xdo-get-mouse-location xdo #:optional with-window)
         (lib:xdo-get-mouse-location xdo (car with-window)))

(define (xdo-get-window-at-mouse xdo) (lib:xdo-get-window-at-mouse xdo))

(define* (xdo-wait-for-mouse-move xdo #:key to from)
         (let ((coord (or to from)))
           (if (eq? 2 (length coord))
             (eq? 0 (lib:xdo-wait-for-mouse-move xdo (car coord) (cadr coord) (if to #t #f)))
             (throw 'bad-coords "Bad number of coordinates"))))

(define* (xdo-click xdo button #:key repeat delay window)
         (eq? 0 (lib:xdo-click-window xdo (or window (xdo-get-window-at-mouse xdo)) button repeat delay)))

(define* (xdo-enter-text xdo string #:key window delay)
         (eq? 0 (lib:xdo-enter-text-window xdo (or window (xdo-get-window-at-mouse xdo)) string delay)))

(define* (xdo-send-keysequence xdo sequence #:optional rest #:key window delay)
         (let ((window (or window (xdo-get-window-at-mouse xdo)))
               (up (memq #:up rest))
               (down (memq #:down rest)))
           (eq? 0 (lib:xdo-send-keysequence-window xdo window sequence delay (if up 'up
                                                                               (if down 'down
                                                                                 'both))))))

(define* (xdo-move-window xdo xy #:key window)
         (if (not (eq? 2 (length xy))) #f
           (let ((x (car xy))
                 (y (cadr xy))
                 (window (or window (xdo-get-window-at-mouse xdo))))
             (eq? 0 (lib:xdo-move-window xdo window x y)))))

(define* (xdo-translate-window-with-sizehint xdo width height #:key window)
         (let* ((window (or window (xdo-get-window-at-mouse xdo)))
                (ret (lib:xdo-translate-window-with-sizehint xdo window width height)))
           (if (list? ret) ret #f)))

#|
        "lib:xdo-translate-window-with-sizehint",
        "lib:xdo-set-windo-size",
        "lib:xdo-set-window-property",
        "lib:xdo-set-window-class",
        "lib:xdo-set-window-urgency",
        "lib:xdo-set-window-override-redirect",
        "lib:xdo-focus-window",
        "lib:xdo-raise-window",
        "lib:xdo-get-focused-window",
        "lib:xdo-wait-for-window-focus",
        "lib:xdo-get-pid-window",
        "lib:xdo-get-focused-window-sane",
        "lib:xdo-activate-window",
        "lib:xdo-wait-for-window-active",
        "lib:xdo-map-window",
        "lib:xdo-unmap-window",
        "lib:xdo-minimize-window",
        "lib:xdo-reparent-window",
        "lib:xdo-get-window-location",
        "lib:xdo-get-window-size",
        "lib:xdo-get-active-window",
        "lib:xdo-select-window-with-click",
        "lib:xdo-set-number-of-desktops",
        "lib:xdo-get-number-of-desktops",
        "lib:xdo-set-current-desktop",
        "lib:xdo-get-current-desktop",
        "lib:xdo-set-desktop-for-window",
        "lib:xdo-get-desktop-for-window",
        "lib:xdo-search-windows",
        "lib:xdo-get-window-property",
        "lib:xdo-get-input-state",
        "lib:xdo-get-symbol-map",
        "lib:xdo-get-active-modifiers",
        "lib:xdo-clear-active-modifiers",
        "lib:xdo-set-active-modifiers",
        "lib:xdo-get-desktop-viewport",
        "lib:xdo-set-desktop-viewport",
        "lib:xdo-kill-window",
        "lib:xdo-find-window-client",
        "lib:xdo-get-window-name",
        "lib:xdo-disable-feature",
        "lib:xdo-enable-feature",
        "lib:xdo-has-feature",
        "lib:xdo-get-viewport-dimensions",
|#
