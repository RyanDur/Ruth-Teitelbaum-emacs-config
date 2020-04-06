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
  :config
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'js-mode)

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist))))

(defun tim-eslint-fix-file ()
  (interactive)
  (message "eslint --fix the file" (buffer-file-name))
  (call-process-shell-command
   (concat "npx eslint --fix " (buffer-file-name))
   nil "*Shell Command Output*" t)
  (revert-buffer t t))

(add-hook 'js-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'tim-eslint-fix-file)))

(add-hook 'typescript-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'tim-eslint-fix-file)))

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
  :ensure t)
(setq-default typescript-indent-level 2)
(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
