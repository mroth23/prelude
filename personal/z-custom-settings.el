;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/personal/z-settings.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/personal/z-settings.org")))
