(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'typescript-mode
    '(add-hook 'typescript-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

(use-package flycheck
  :defer 1
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)

  (add-hook 'org-mode-hook (lambda ()
			     (flycheck-mode -1)))

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist))))

(use-package eslint-fix
  :ensure t
  :config
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

  (eval-after-load 'typescript-mode
    '(add-hook 'typescript-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package yasnippet
  :ensure t
  :config

  (use-package yasnippet-snippets
    :ensure t)
  (use-package mocha-snippets
    :ensure t)
  (yas-global-mode 1))

(use-package jasminejs-mode
  :hook ((js-mode . jasminejs-mode)
	 (typescript-mode . jasminejs-mode)
	 (jasminejs-mode-hook . jasminejs-add-snippets-to-yas-snippet-dirs)))

(add-to-list 'load-path "/Users/ryandurling/.emacs.d/submodules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(setq js-indent-level 2)

(use-package auto-complete
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (auto-complete-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))

(add-hook 'after-init-hook 'global-company-mode)

(use-package indium
  :ensure t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode))

(use-package typescript-mode
  :ensure t
  :config (setq-default typescript-indent-level 2))

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'"
	 "\\.css\\'"))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)
	 (web-mode . emmet-mode)
	 (css-mode . emmet-mode)))
