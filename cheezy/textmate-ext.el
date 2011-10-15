
;;;###autoload
(defun tf-textmate-ext-bind-keys ()
  (cond ((featurep 'aquamacs) (tf-textmate-ext-bind-aquamacs-keys))
        ((featurep 'mac-carbon) (tf-textmate-ext-bind-carbon-keys))
        ((featurep 'ns) (tf-textmate-ext-bind-ns-keys))))

(defun tf-textmate-ext-bind-carbon-keys ()
  (define-key *textmate-mode-map* [(meta /)] 'comment-or-uncomment-region-or-line)
  (define-key *textmate-mode-map* [C-return] 'tf-textmate-ext-previous-line)
  (define-key *textmate-mode-map* (kbd "M-[") 'align))

(defun tf-textmate-ext-bind-ns-keys ()
  (define-key *textmate-mode-map* [(meta /)] 'comment-or-uncomment-region-or-line)
  (define-key *textmate-mode-map* [C-return] 'tf-textmate-ext-previous-line)
  (define-key *textmate-mode-map* (kbd "M-[") 'align))


(defun tf-textmate-ext-previous-line ()
  "Insert a blank line above the cursor and move the cursor up one line."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

;;;###autoload
(add-hook 'textmate-mode-hook 'tf-textmate-ext-bind-keys)

(provide 'cheezy/textmate-ext)

