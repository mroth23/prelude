(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png")
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)))
    (setq dashboard-banner-logo-title ""))

;; Custom shortcut to open this file.
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/personal/z-settings.org"))

(global-set-key (kbd "C-c v c") 'config-visit)

(key-chord-define-global "xf" 'iy-go-to-char)
(key-chord-define-global "xd" 'iy-go-to-char-backward)
(key-chord-define-global ";;" "\C-e;")

;; switch-window settings
;; Override global key bindings for switching windows.
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

;; Use home row instead of number keys.
(setq switch-window-input-style 'minibuffer)
(setq switch-window-increase 6)
(setq switch-window-threshold 2)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))

;; Prelude also comes with ace-window, which uses custom keybinds (Super-w).
;; Set it to also use homerow keys instead of numbers for buffers.
;; TODO: decide which one I like better, e.g.
;; (Super-w v a) or (C-x 2 a) to split window a.

(setq aw-keys '(?a ?s ?d ?f ?k ?l ?\; ?w ?e ?i))

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; If nothing is selected, pick the symbol under the cursor.
(global-set-key (kbd "C->") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Special commands for inserting numbers or chars, sorting and reversing.
(global-set-key (kbd "C-c m n") 'mc/insert-numbers)
(global-set-key (kbd "C-c m l") 'mc/insert-letters)
(global-set-key (kbd "C-c m s") 'mc/sort-regions)
(global-set-key (kbd "C-c m r") 'mc/reverse-regions)

;; Nyan cat mode
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
(setq nyan-bar-length 15)
(nyan-mode 1)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p nil)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-emacs-theme))

(setq display-time-24hr-format t)
(setq display-time-format " %H:%M ")
(setq display-time-default-load-average nil)
(display-battery-mode 0)

(display-time-mode 1)

(use-package fancy-battery
  :ensure t
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (if window-system
      (fancy-battery-mode)
      (display-battery-mode)))

(setq line-number-mode t)
(setq column-number-mode t)
(spaceline-toggle-line-column-on)

(spaceline-toggle-minor-modes-off)

;;  (add-to-list 'company-backends 'company-dabbrev-code)
;;  (add-to-list 'company-backends 'company-yasnippet)
;;  (add-to-list 'company-backends 'company-files)

;; (setq desktop-dirname             "~/.emacs.d/desktop/"
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-load-locked-desktop nil
;;       desktop-auto-save-timeout   30)
;; (desktop-save-mode 1)

(setq nlinum-highlight-current-line t)
(setq nlinum-format "%4d \u2502")

;; Use this to have nlinum globally.
;; (global-nlinum-mode 1)

(defun nlinum-set-face-attribute ()
  (set-face-attribute 'nlinum-current-line nil :background "gray20")
  (set-face-attribute 'linum nil :background "gray30" :foreground "gray80"))

(add-hook 'nlinum-mode-hook 'nlinum-set-face-attribute)
(add-hook 'prog-mode-hook 'nlinum-mode)

;; Enable mode, and whitespace cleanup on save.
(setq prelude-whitespace t)
(setq prelude-clean-whitespace-on-save t)

;; Mark lines exceeding 100 columns.
(setq whitespace-line-column 100)
;; Set whitespace style: cleanup empty lines / trailing whitespace, show whitespace characters.
(setq whitespace-style '(empty trailing face lines-tail indentation::space tabs newline tab-mark newline-mark))
;; Use spaces instead of tabs by default.
(setq-default indent-tabs-mode nil)

;; dotenv-mode
;; Also apply to .env with extension such as .env.local
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; Use swiper (with helm backend) for search.
(global-set-key (kbd "C-s") 'swiper)

;; Use tab to expand stuff in helm. (Sorry)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Create shortcut for things like the scratch buffer.
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                                 (message "Dot mode activated.")))

;; Turn on all the time.
(add-hook 'find-file-hooks 'dot-mode-on)

(emms-all)
(emms-default-players)

(emms-mode-line 1)
(emms-playing-time 1)
(emms-mode-line-cycle 1)

;; Some C/C++ settings.
;; yasnippet
(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

;; company
;; ctags
(setq path-to-ctags "/usr/local/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -R %s" path-to-ctags (directory-file-name dir-name))))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))


(defun c-mode-company-init ()
  (setq-local company-backends '((company-c-headers
                                  company-dabbrev-code
                                  company-irony
                                  company-yasnippet
                                  company-files
                                  ))))

(add-hook 'c-mode-hook 'c-mode-company-init)
(add-hook 'c++-mode-hook 'c-mode-company-init)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Use clang for formatting and flycheck in C/C++.
(flycheck-clang-analyzer-setup)

(global-set-key (kbd "C-c c f") 'clang-format-region)

;; Python
;; yasnippet
(add-hook 'python-mode-hook 'yas-minor-mode)

;; Set tab with to 4.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default tab-width 4)))

(setq debug-on-message "deferred error.*")


;; virtualenvwrapper init for eshell and interactive shell.
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
