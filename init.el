(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "Refreshing packages")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(defvar use-package-verbose t)
(require 'bind-key)

(org-babel-load-file (concat user-emacs-directory "README.org"))
