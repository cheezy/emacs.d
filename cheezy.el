;; DESCRIPTION: cheezy settings

;; Manually set PATH for use by eshell, rspec-mode, etc.
(let ((path))
  (setq path (concat "/opt/local/bin:"
                     "~/bin:"
                     "/usr/local/bin:"
                     "/usr/bin:"
                     "/bin"))
  (setenv "PATH" path))

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(require 'cheezy/meta)

;; Clojure
;;(eval-after-load 'clojure-mode '(clojure-slime-config))

(require 'cheezy/plain-text)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))

(require 'unbound)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(require 'peepopen)
(require 'cheezy/textmate-ext)
(textmate-mode)
(setq ns-pop-up-frames nil)

(require 'rvm)
(rvm-use-default)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'whitespace)

(require 'cheezy/python)

(require 'cheezy/coffee)
(require 'cheezy/jade)

;; ruby-mode
(require 'cheezy/sinatra)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/ruby-complexity"))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Isolate\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.sake\\'" . ruby-mode))

(require 'linum)
(require 'ruby-complexity)
;; (add-hook 'ruby-mode-hook
;;           (function (lambda ()
;;                       (flymake-mode)
;;                       (linum-mode)
;;                       (ruby-complexity-mode)
;;                       )))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/cucumber.el"))
(require 'feature-mode)
(require 'cheezy/cucumber)
(load-file ".emacs.d/vendor/robot-mode.el")
;;(add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))

(require 'cheezy/js)

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

;; dabbrev-case-fold-search for case-sensitive search

(require 'cheezy/rinari)
(require 'cheezy/rhtml)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/rspec-mode"))
(require 'rspec-mode)

(require 'cheezy/applescript)
(require 'cheezy/org)
(require 'cheezy/textile)
(require 'cheezy/markdown)
(require 'cheezy/haml)
(require 'cheezy/xcode)
(require 'cheezy/keyboard)

;; gist
(require 'gist)

;; Mercurial
;;(require 'mercurial)

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)

;; Activate theme
(load (concat dotfiles-dir "cheezy/theme.el"))
(load (concat dotfiles-dir "cheezy/color-theme-sunburst.el"))
;;(color-theme-cheezy)
(color-theme-tm)

(require 'autotest)
