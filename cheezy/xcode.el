;; XCODE
(require 'objc-c-mode)

;; (setq c-default-style "bsd"
;;       c-basic-offset 2)

(require 'cc-menus)

(require 'xcode)
(define-key objc-mode-map [(meta r)] 'xcode-compile)
(define-key objc-mode-map [(meta K)] 'xcode-clean)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  [(meta O)] 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))         ; Hide and show blocks
(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
(require 'objj-mode)

(provide 'cheezy/xcode)
