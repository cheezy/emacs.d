;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Functions
(defun timestamp-for-org ()
  "Insert string for the current time formatted like '2011-01-01 Thu 13:54'."
  (interactive)
  (end-of-line)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

;; Override
(add-hook 'org-mode-hook
          (lambda()
            (local-set-key [(control meta return)] 'org-insert-heading)
            (local-set-key [(control shift left)] 'previous-buffer)
            (local-set-key [(control shift right)] 'next-buffer)
            (local-set-key [(meta shift right)] 'ido-switch-buffer)
            (local-set-key [(meta shift left)] 'magit-status)
            (local-set-key [(control shift t)] 'timestamp-for-org)
            ))

(setq org-agenda-files (file-expand-wildcards "~/bin/org/*.org"))

(provide 'cheezy/org)
