;; rhtml-mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

(provide 'cheezy/rhtml)