(use-package magit
  :ensure t)

(use-package git-auto-commit-mode
  :ensure t)

(use-package git-link
  :ensure t
  :config (global-set-key (kbd "C-c g l") 'git-link))

(use-package git-timemachine
  :ensure t)

(use-package git-messenger
  :ensure t
  :config
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
  (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

  ;; Use magit-show-commit for showing status/diff commands
  (custom-set-variables '(git-messenger:use-magit-popup t)))
