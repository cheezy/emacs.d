
;; CHEEZY settings

(let ((path))
  (setq path (concat "/opt/local/bin:"
                     "~/bin:"
                     "/usr/local/bin:"
                     "/usr/bin"
                     "/bin"))
  (setenv "PATH" path))

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(require 'config/meta)


(require 'config/plain-text)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))

(require 'unbound)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(textmate-mode)

(require 'whitespace)


;; ruby-mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/ruby-complexity"))
(require 'linum)
(require 'ruby-complexity)
(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (flymake-mode)
;                      (linum-mode)
;                      (ruby-complexity-mode)
                      )))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/cucumber.el"))
(require 'feature-mode)
(require 'config/cucumber)

;; Remove scrollbars and make hippie expand
;; work nicely with yasnippet
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;;        try-expand-dabbrev-from-kill
        ;;         try-complete-file-name
        ;;         try-complete-file-name-partially
        ;;         try-complete-lisp-symbol
        ;;         try-complete-lisp-symbol-partially
        ;;         try-expand-line
        ;;         try-expand-line-all-buffers
        ;;         try-expand-list
        ;;         try-expand-list-all-buffers
        ;;        try-expand-whole-kill
        ))

(defun indent-or-complete ()
  (interactive)
  (if (and (looking-at "$") (not (looking-back "^\\s-*")))
      (hippie-expand nil)
    (indent-for-tab-command)))
(add-hook 'find-file-hooks (function (lambda ()
                                       (local-set-key (kbd "TAB") 'indent-or-complete))))


(require 'config/rinari)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/rspec-mode"))
(require 'rspec-mode)

;; Groovy
(add-to-list 'load-path (concat dotfiles-dir "/vendor/groovy.el"))
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; rhtml-mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rhtml"))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;; Scala
(add-to-list 'load-path (concat dotfiles-dir "/vendor/scala"))
(require 'scala-mode-auto)

;; Javascript
(setq js2-basic-offset 2)
(setq js2-auto-indent-flag nil)
(setq javascript-indent-level 2)


(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
(define-key haml-mode-map [(control meta left)] 'haml-up-list) 
(define-key haml-mode-map [(control meta right)] 'haml-down-list)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.js\.erb\\'" . ruby-mode))

;; gist
(require 'gist)

;; ColorThemes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)
;; (color-theme-charcoal-black)


;; Send the buffer to inf-ruby buffer
(defun send-buffer-to-ruby()
  (interactive)
  (ruby-send-region-and-go (point-min) (point-max)))
(global-set-key (kbd "M-j") 'send-buffer-to-ruby)

(require 'config/keyboard)

;; eshell preferences
(add-hook 'eshell-mode-hook
          '(lambda nil
             (let ((path))
               (setq path "~/bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin")
               (setenv "PATH" path))
             (local-set-key "\C-u" 'eshell-kill-input)))

(defun eshell/cl ()
  "Command to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


(require 'carbon-font)


;; theme-start
(defun cheezy-reload-theme ()
  "Reload the theme"
  (interactive)
  (save-buffer)
  (eval-buffer)
  (color-theme-cheezy))

(global-set-key [f2] 'cheezy-reload-theme)


(load (concat dotfiles-dir "cheezy-theme.el"))
;(color-theme-cheezy)
