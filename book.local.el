(setq running-osx (or (featurep 'mac-carbon) (eq 'ns window-system)))

;;(if (eq t 'running-osx)
;;    (
     (set-face-font 'default "-apple-inconsolata-medium-r-normal--32-0-72-72-m-0-iso10646-1")
     ;; characters wide, lines tall
     (set-frame-size (car (frame-list)) 100 28)
     ;; x y
     (set-frame-position (car (frame-list)) 17 33)
;;     ))

;; Show a list of available variants:
;; M-x eval-expression
;; (x-family-fonts "myriad pro")

;;(setq default-frame-alist
;;      '((wait-for-wm . nil)
;;        (left . 17)
;;        (top . 33)
;;        (width . 100)
;;        (height . 28)
;;        (font . "Inconsolata-32")))

