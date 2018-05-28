;; Custom shortcut to open this file.
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/personal/z-custom-settings.el"))

(global-set-key (kbd "C-c C-e") 'config-visit)

;; Keep desktop state between runs.
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(desktop-save-mode 1)

;; Configure Whitespace mode
;; Enable mode, and whitespace cleanup on save.
(setq prelude-whitespace t)
(setq prelude-clean-whitespace-on-save t)

;; Mark lines exceeding 100 columns.
(setq whitespace-line-column 100)
;; Set whitespace style: cleanup empty lines / trailing whitespace, show whitespace characters.
(setq whitespace-style '(empty trailing face lines-tail indentation::space tabs newline tab-mark newline-mark))
;; Use spaces instead of tabs by default.
(setq-default indent-tabs-mode nil)

;; Use clang for formatting and flycheck in C/C++.
(flycheck-clang-analyzer-setup)
(global-set-key (kbd "C-c c f") 'clang-format-region)

;; Python
;; Set tab with to 4.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default tab-width 4)))

(key-chord-define-global "xf" 'iy-go-to-char)
(key-chord-define-global "xd" 'iy-go-to-char-backward)
(key-chord-define-global ";;" "\C-e;")

;; virtualenvwrapper init for eshell and interactive shell.
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support

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

;; dotenv-mode
;; Also apply to .env with extension such as .env.local
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; dot-mode
;; Create shortcut for things like the scratch buffer.
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                                 (message "Dot mode activated.")))

;; Turn on all the time.
(add-hook 'find-file-hooks 'dot-mode-on)

;; Helm config. I prefer it to ivy or ido.
;; Use swiper (with helm backend) for search.
(global-set-key (kbd "C-s") 'swiper)

;; Use tab to expand stuff in helm. (Sorry)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; EMMS config.
(emms-all)
(emms-default-players)

(emms-mode-line 1)
(emms-playing-time 1)
(emms-mode-line-cycle 1)

;; Nyan cat mode
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
(setq nyan-bar-length 15)
(nyan-mode 1)
