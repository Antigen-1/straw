#lang racket/base
(require racket/gui/base racket/class racket/math racket/format racket/contract)
(provide (contract-out (main-window% (class/c (init-field (path (and/c path-string? file-exists?)))))))
(define main-window%
  (class frame%
    (init-field path)
    (super-new (label "straw"))

    ;;read the bitmap
    (define bitmap (read-bitmap path))
    (define bitmap-dc (make-object bitmap-dc% bitmap))
    (define height (send bitmap get-height))
    (define width (send bitmap get-width))

    ;;major layout
    (define main-pane (new vertical-pane% (parent this) (alignment '(left top))))
    (define color-panel (new vertical-panel% (parent main-pane) (enabled #f) (style '(border)) (border 5)))
    (define bitmap-pane (new vertical-pane% (parent main-pane) (min-width width) (min-height height)))

    ;;colors
    (define BLUE (make-object color% "blue"))
    (define RED (make-object color% "red"))
    (define GREEN (make-object color% "green"))
    (define WHITE (make-object color% "white"))
    (define temp (make-object color%))

    ;;text fields
    (define (make-field label background)
      (let ((f (new text-field%
                    (label label)
                    (parent color-panel))))
        (send f set-field-background background)
        f))
    (define red-field (make-field "RED" RED))
    (define green-field (make-field "GREEN" GREEN))
    (define blue-field (make-field "BLUE" BLUE))
    (define color-field (make-field "color" WHITE))
    
    ;;canvas for the bitmap
    (define canvas-with-event-handler%
      (class canvas%
        (super-new
         (parent bitmap-pane)
         (enabled #t)
         (paint-callback (lambda (_ d)
                           (send d draw-bitmap bitmap 0 0))))
        (inherit get-client-size)
        (define/override (on-event e)
          (cond ((send e get-left-down)
                 (define x (send e get-x))
                 (define y (send e get-y))
                 (define-values (w h) (get-client-size))
                 (send bitmap-dc get-pixel
                       (exact-floor (* width (/ x w)))
                       (exact-floor (* height (/ y h)))
                       temp)
                 (send red-field set-value (~a (send temp red)))
                 (send green-field set-value (~a (send temp green)))
                 (send blue-field set-value (~a (send temp blue)))
                 (send color-field set-field-background temp))))))
    (new canvas-with-event-handler%)

    (inherit show)
    (show #t)))
