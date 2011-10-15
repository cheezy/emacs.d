
;; For Emacs from source. Opens files in the existing frame.
;;(setq ns-pop-up-frames nil)

;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq default-tab-width 2)
(setq tab-width 2)


(defun byte-recompile-home ()
  "Speed load time by compiling dotfiles"
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))


(defun tf-open-textmate ()
  "Open the current file in TextMate."
  (interactive)
  (shell-command-to-string (concat "mate " buffer-file-name)))

(defun tf-open-finder ()
  "Open the current directory in the Finder."
  (interactive)
  (shell-command-to-string "open ."))


(defun tf-open-plainview ()
  "Open the current file's HTML counterpart in Plainview browser."
  (interactive)
  (shell-command-to-string (concat "open -a Plainview.app "
                                   (file-name-directory buffer-file-name)
                                   "html/"
                                   (file-name-nondirectory buffer-file-name)
                                   ".html")))

;; Run Ruby Rake
(global-set-key [(meta shift r)] 'rake)

;; Full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key (kbd "M-n") 'toggle-fullscreen)

(defun recenter-to-top ()
  "Take the current point and scroll it to within a
   few lines of the top of the screen."
  (interactive)
  (recenter 3))
(global-set-key [(control shift l)] 'recenter-to-top)

(defun recenter-to-bottom ()
  "Take the current point and scroll it to within a
   few lines of the bottom of the screen."
  (interactive)
  (recenter -3))
(global-set-key [(control meta l)] 'recenter-to-bottom)


(defun kill-current-line ()
  "Kill the current line, no matter where the cursor is."
  (interactive)
  (textmate-select-line) (kill-region (region-beginning) (region-end)))
(global-set-key [(control shift k)] 'kill-current-line)


(defun kill-word-and-capitalize (arg)
  "Delete one or more words and capitalize next word.
With argument, do it a number of times.
Useful for reworking the beginning of a sentence."
  (interactive "p")
  (kill-word arg) (delete-char 1) (capitalize-word 1))
(global-set-key "∂" 'kill-word-and-capitalize)


(prefer-coding-system 'utf-8)

(server-start)

(require 'line-num)

(provide 'cheezy/meta)
