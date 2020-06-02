(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  ;; (setq mac-right-option-modifier 'none)
  (setq mac-command-modifier 'super)
  (setq mac-function-modifier 'hyper)
  (global-set-key [kp-delete] 'delete-char)
  ;; For some reason lockfiles break python anaconda-mode's autocomplete
  (setq create-lockfiles nil))

(when (eq system-type 'windows-nt)
  ;; Performance
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 64 1024)) ;; 64k Buffer Size
  (setq jit-lock-defer-time 0)
  (setq inhibit-compacting-font-caches t)
  ;; Scrolling fixes
  (setq fast-but-imprecise-scrolling t)
  (pixel-scroll-mode 0)
  (setq scroll-conservatively 10000
        scroll-preserve-screen-position 1
        scroll-step 1
        scroll-bar-mode -1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  ;; Font
  (set-face-attribute 'default nil :family "Meslo LG M" :height 90)
  ;; UTF-8 as default encoding
  (set-language-environment "UTF-8")

  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'super) ; Menu/App key
  )

;; Instead of setting gc-cons-threshold, use gcmh.
(use-package gcmh
  :ensure t
  :init
  (setq gcmh-high-cons-threshold 50000000
        gcmh-verbose nil
        gcmh-idle-delay 15)
  :config
  (gcmh-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-banner-logo-title "")
  (add-to-list 'dashboard-items '(agenda) t))

(setq projectile-indexing-method 'alien
      projectile-generic-command "fd . -0 --no-ignore-vcs"
      projectile-git-command "fd . -0 --no-ignore-vcs"
      projectile-svn-command "fd . -0 --no-ignore-vcs"
      projectile-git-submodule-command nil
      projectile-sort-order 'recentf
      projectile-enable-caching t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(scroll-bar-mode -1)

(if (eq system-type 'windows-nt)
    (pixel-scroll-mode -1)
  (pixel-scroll-mode 1))

(use-package iy-go-to-char
  :ensure t
  :config
  (key-chord-define-global "xf" 'iy-go-to-char)
  (key-chord-define-global "xd" 'iy-go-to-char-backward))

(use-package hydra
  :ensure t)

;; TODO: move everything here into use-package
(use-package switch-window
  :ensure t)
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

;; Hydra keybinds for ace-window
(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:color red
                                :columns nil)
   "window"
   ("h" windmove-left nil)
   ("j" windmove-down nil)
   ("k" windmove-up nil)
   ("l" windmove-right nil)
   ("H" hydra-move-splitter-left nil)
   ("J" hydra-move-splitter-down nil)
   ("K" hydra-move-splitter-up nil)
   ("L" hydra-move-splitter-right nil)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
    "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
    "horz")
   ("t" transpose-frame "'" :exit t)
   ("o" delete-other-windows "one" :exit t)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-maximize-window "ace-one" :exit t)
   ("b" ido-switch-buffer "buf")
   ("m" headlong-bookmark-jump "bmk")
   ("q" nil "cancel")
   ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
   ("f" nil)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :demand t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ;; If nothing is selected, pick the symbol under the cursor.
   ("C->" . mc/mark-next-like-this-symbol)
   ("C-<" . mc/mark-previous-like-this-symbol)
   ("C-c C-<" . mc/mark-all-like-this)
   ("H-SPC" . set-rectangular-region-anchor)
   ;; Special commands for inserting numbers or chars, sorting and reversing.
   ("C-c x n" . mc/insert-numbers)
   ("C-c x l" . mc/insert-letters)
   ("C-c x s" . mc/sort-regions)
   ("C-c x r" . mc/reverse-regions)))

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c x w") 'daedreth/kill-inner-word)

;; Another one of Uncle Dave's functions to copy a while line.
(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c x c") 'daedreth/copy-whole-line)

(defun all-over-the-screen ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

(global-set-key (kbd "C-c x a") 'all-over-the-screen)

;; Custom shortcut to open this file.
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/personal/z-settings.org"))

(global-set-key (kbd "C-c v c") 'config-visit)

;; Reload config file
(defun config-reload ()
  (interactive)
  (org-babel-load-file "~/.emacs.d/personal/z-settings.org"))

(global-set-key (kbd "C-c v r") 'config-reload)

;; Visit package list
(defun visit-package-list-buffer ()
  (interactive)
  (crux-start-or-switch-to (lambda ()
                             (package-list-packages))
                           "*Packages*"))

(global-set-key (kbd "C-c v p") 'visit-package-list-buffer)

(defun xref-pop-recenter ()
  "Like xref-pop-marker-stack, but recenters the screen around the cursor after jumping to the position."
  (interactive)
  (xref-pop-marker-stack)
  (recenter-top-bottom))

(global-set-key (kbd "M-,") 'xref-pop-recenter)

;; Bind avy-copy-line. Uses x d because it actually duplicates a line.
(global-set-key (kbd "C-c x d") 'avy-copy-line)

;; Currently disabed because it doesn't work with mood-line
  ;; (use-package nyan-mode
  ;;   :ensure t
  ;;   :config
  ;;   (setq nyan-animate-nyancat t
  ;;         nyan-wavy-trail t
  ;;         nyan-bar-length 13))

  ;; (nyan-mode 1)

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;   (setq spaceline-buffer-encoding-abbrev-p nil)
;;   (setq spaceline-line-column-p nil)
;;   (setq spaceline-line-p nil)
;;   (setq powerline-default-separator (quote arrow))
;;   (spaceline-emacs-theme))

(use-package mood-line
  :ensure t
  :config)

(mood-line-mode)

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

(use-package company
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . company-complete-selection))
  :hook
  (prog-mode . company-mode)
  :ensure t
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.4)
  (setq company-tooltip-limit 15)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (add-to-list 'company-backends 'company-dabbrev))

;; (add-to-list 'company-backends 'company-dabbrev-code)
;; (add-to-list 'company-backends 'company-yasnippet)
;; (add-to-list 'company-backends 'company-files)

(global-display-line-numbers-mode)

;; Enable mode, and whitespace cleanup on save.
(setq prelude-whitespace t)
(setq prelude-clean-whitespace-on-save t)

;; Mark lines exceeding 120 columns.
(setq whitespace-line-column 120)
;; Set whitespace style: cleanup empty lines / trailing whitespace, show whitespace characters.
(setq whitespace-style '(empty trailing face lines-tail indentation::space tabs newline tab-mark newline-mark))
;; Use spaces instead of tabs by default.
(setq-default indent-tabs-mode nil)

;; dotenv-mode
(use-package dotenv-mode
  :ensure t)

;; Also apply to .env with extension such as .env.local
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; Use swiper for search.
(use-package swiper
  :ensure t)

;; Swiper do-what-I-mean
;; When text is marked, search for that.
;; When nothing is marked, search for input.
(defun swiper-dwim ()
  "Use current region if active for swiper search"
  (interactive)
  (if (not (use-region-p))
      (swiper)
    (deactivate-mark)
    (swiper (format "%s" (buffer-substring (region-beginning) (region-end))))))

(global-set-key (kbd "C-s") 'swiper-dwim)

(with-eval-after-load 'helm
  ;; Use tab to expand stuff in helm. (Sorry)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (bind-key* "C-r" 'helm-resume)
  ;; Fuzzy matching everywhere
  (setq
   helm-candidate-number-limit 50
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-imenu-fuzzy-match t
   helm-recentf-fuzzy-match t
   helm-locate-fuzzy-match nil
   helm-M-x-fuzzy-match t
   helm-semantic-fuzzy-match t

   helm-case-fold-search 'smart
   helm-ff-transformer-show-only-basename nil
   helm-ff-newfile-prompt-p nil

   ;; Autoresize helm buffer depending on match count
   helm-autoresize-max-height 0
   helm-autoresize-min-height 40))

(helm-autoresize-mode 1)

;; Use git grep for helm.
(setq projectile-use-git-grep 1)

;; Additional Helm-related packages
(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
        helm-flx-for-helm-locate t) ;; nil by default
  )
(setq helm-ag-base-command "ag -U --vimgrep")

(use-package dot-mode
  :ensure t
  :config
  (global-dot-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Not yet working!!
;; (use-package rainbow-csv
;;   :load-path "~/projects/rainbow-csv/"
;;   :init
;;   (add-hook 'csv-mode-hook #'rainbow-csv-mode))

(when window-system
  (use-package pretty-mode
    :ensure t
    :after
    (global-pretty-mode t)))

(global-prettify-symbols-mode +1)

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/personal/snippets")
  (use-package yasnippet-snippets
    :ensure t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/personal/snippets" t)
  (yas-reload-all))

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :bind
  ("C-o" . aya-open-line)
  :config
  (setq aya-persist-snippets-dir "~/.emacs.d/personal/snippets"))

(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'latex-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)

;; Adapted from abo-abo/function-args
(defun moo-javadoc ()
  "Generate a javadoc yasnippet and expand it with `aya-expand'.
The point should be inside the method to generate docs for"
  (interactive)
  (move-beginning-of-line nil)
  (let ((tag (semantic-current-tag)))
    (unless (semantic-tag-of-class-p tag 'function)
      (error "Expected function, got %S" tag))
    (let* ((name (semantic-tag-name tag))
           (attrs (semantic-tag-attributes tag))
           (args (plist-get attrs :arguments))
           (ord 1))
      (setq aya-current
            (format
             "/**
* $1
*
%s
* @return $%d
*/"
             (mapconcat
              (lambda (x)
                (format "* @param %s $%d"
                        (car x) (incf ord)))
              args
              "\n")
             (incf ord)))
      (senator-previous-tag)
      (crux-smart-open-line-above)
      (aya-expand))))

(defun cc-end-line-with-semicolon ()
  (interactive)
  (move-end-of-line nil)
  (insert-char ?\; 1)
  (crux-smart-open-line nil))

(use-package magit
  :ensure t
  :defer t
  ;; :hook
  ;; (after-save . magit-after-save-refresh-status)
  :config
  (define-key magit-status-mode-map (kbd "Q") 'magit-toggle-whitespace))

(use-package forge
  :ensure t
  :after magit)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(use-package vimish-fold
  :ensure t
  :config (add-hook 'prog-mode-hook 'vimish-fold-mode))

(bind-key "s-a" (defhydra hydra-vimish-fold
                  (:color blue
                          :columns 3)
                  "fold"
                  ("a" vimish-fold-avy "avy")
                  ("d" vimish-fold-delete "del")
                  ("D" vimish-fold-delete-all "del-all")
                  ("u" vimish-fold-unfold "undo")
                  ("U" vimish-fold-unfold-all "undo-all")
                  ("s" vimish-fold "fold")
                  ("r" vimish-fold-refold "refold")
                  ("R" vimish-fold-refold-all "refold-all")
                  ("t" vimish-fold-toggle "toggle" :exit nil)
                  ("T" vimish-fold-toggle-all "toggle-all" :exit nil)
                  ("j" vimish-fold-next-fold "down" :exit nil)
                  ("k" vimish-fold-previous-fold "up" :exit nil)
                  ("q" nil "quit")))

;; (use-package hideshow-org
;;   :ensure t
;;   :config
;;   ()
;;   (add-hook 'prog-mode-hook 'hs-org/minor-mode))

(with-eval-after-load 'god-mode
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat))

(use-package sx
  :ensure t
  :config
  (bind-keys :prefix "C-c q"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

(use-package nhexl-mode
  :ensure t
  :defer t)

;;;; This is currently disabled because of a compilation error in pdf-tools.
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (custom-set-variables
;;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;; (pdf-tools-install)

(use-package outshine
  :defer t
  :ensure t
  :hook
  ((emacs-lisp-mode . outshine-mode)
   (LaTeX-mode . outshine-mode)
   (picolisp-mode . outshine-mode)
   (clojure-mode . outshine-mode)
   (ess-mode . outshine-mode)
   (ledger-mode . outshine-mode)
   (python-mode . outshine-mode)))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 50
        treemacs-indentation 2))

;; Global semantic mode
(semantic-mode 1)
(global-semantic-highlight-func-mode 1)

;; Enable subword-mode for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; I never got smartparens to work properly with cc-mode (formatting etc). So I use the builtins instead, which work nicely.
(defun disable-smartparens ()
  (smartparens-mode 0)
  (electric-pair-mode 1))

(add-hook 'c-mode-common-hook 'disable-smartparens)

;; String-edit: Edit strings in separate buffer to avoid escape nightmares
(use-package string-edit
  :ensure t
  :bind
  (:map c-mode-base-map
        ("C-c '" . string-edit-at-point)))

;; Unfill - opposite to M-q (fill-paragraph)
(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

;; Source: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(defun me/eval-region-and-kill-mark (beg end)
  "Execute the region as Lisp code.
    Call `eval-region' and kill mark. Move back to the beginning of the region."
  (interactive "r")
  (eval-region beg end)
  (setq deactivate-mark t)
  (goto-char beg))

(global-set-key (kbd "C-:") 'me/eval-region-and-kill-mark)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-mode
  :ensure t
  :hook
  ((c++-mode
    c-mode
    objc-mode
    java-mode) . lsp)
  :bind
  (:map lsp-mode-map
        ("C-;" . cc-end-line-with-semicolon)
        ("C-c l j" . moo-javadoc)
        ("C-c l o" . lsp-organize-imports)
        ("C-c l r" . lsp-rename)
        ("C-c l x" . lsp-restart-workspace)
        ("C-c l d" . lsp-describe-thing-at-point)
        ("C-c l h" . lsp-treemacs-call-hierarchy))
  :init
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-eldoc-render-all nil
   lsp-enable-on-type-formatting nil
   lsp-enable-indentation nil
   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-text-document-color nil
   lsp-enable-semantic-highlighting nil
   lsp-enable-links nil
   lsp-signature-auto-activate nil
   lsp-prefer-capf t)
  :config
  (setq-local read-process-output-max (* 1024 1024))
  (setq-local gcmh-high-cons-threshold (* 2 gcmh-high-cons-threshold)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :demand t
  :hook
  ((c++-mode
    c-mode
    objc-mode
    python-mode
    java-mode) . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)
        ("C-c l ." . lsp-ui-peek-find-definitions)
        ("C-c l ?" . lsp-ui-peek-find-references)
        ("C-c l w" . lsp-ui-peek-find-workspace-symbol)
        ("C-c l i" . lsp-ui-peek-find-implementation)
        ("M-#"     . lsp-ui-doc-show)
        ("C-c l m" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-update-mode 'line
        lsp-ui-peek-enable nil
        lsp-ui-peek-always-show nil
        lsp-ui-doc-enable nil))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :demand t
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Some C/C++ settings
(require 'lsp-mode)
(require 'lsp-clients)
(use-package clang-format
  :ensure t)

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (progn
                (when (locate-dominating-file "." ".clang-format")
                  (clang-format-buffer))
                ;; Continue to save.
                nil))
            nil
            ;; Buffer local hook.
            t))

;; (setq lsp-clients-clangd-executable "c:/Program Files/LLVM/bin/clangd.exe")

(add-hook 'c++-mode-hook 'lsp)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "c:/prj/ccls/Release/ccls.exe")
(setq lsp-prefer-flymake nil)
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
(setq ccls-args '("--log-file=c:/prj/ccls/ccls.log"))

;; Use clang for formatting and flycheck in C/C++.
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(global-set-key (kbd "C-c x f") 'clang-format-region)
(global-set-key (kbd "C-c x F") 'clang-format-buffer)

(setq-default c-default-style "bsd")

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-hungry-state 1) (c-toggle-auto-newline 1) (c-set-style "bsd")))

;; yasnippet
(add-hook 'python-mode-hook 'yas-minor-mode)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (setq-default tab-width 4)
                         (require 'lsp-python-ms)
                         (lsp))))

;; virtualenvwrapper
(use-package virtualenvwrapper
  :hook python-mode
  :ensure t
  :demand t
  :config
  ;; virtualenvwrapper init for eshell and interactive shell.
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq projectile-switch-project-action
        '(lambda()
           (venv-projectile-auto-workon)
           (projectile-find-file))))

;; py-isort
(use-package py-isort
  :defer t
  :hook
  (python-mode . (lambda () (add-hook 'before-save-hook 'py-isort-before-save)))
  :ensure t)

;; yapf
(use-package yapfify
  :defer t
  :ensure t
  :hook
  (python-mode . yapf-mode))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-export-with-smart-quotes t)
(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(use-package htmlize
  :ensure t)

(defalias 'perl-mode 'cperl-mode)

(defun c-set-cperl-style ()
  (interactive)
  ;; Indentation
  (setq cperl-indent-level 4)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-brace-offset -4)
  (setq cperl-close-paren-offset -4)
  (setq cperl-extra-newline-before-brace t)
  (setq cperl-merge-trailing-else nil)
  (setq cperl-tab-always-indent t)
  ;; Use font lock but disable invalid face
  (setq cperl-font-lock t)
  (setq cperl-invalid-face nil)
  ;; Auto-newline and electric parens
  (setq cperl-auto-newline t)
  (setq cperl-electric-parens nil))

(add-hook 'cperl-mode-hook '(lambda ()
                              (disable-smartparens)
                              (c-set-cperl-style)
                              (c-toggle-hungry-state 1)
                              (c-toggle-auto-newline 1)))

(use-package lsp-java
  :ensure t
  :demand t
  :config
  (setq lsp-java-format-enabled nil
        lsp-java-signature-help-enabled nil
        lsp-java-completion-overwrite t
        lsp-java-autobuild-enabled nil))

(add-hook 'java-mode-hook '(lambda () (c-set-java-style)))

(defun c-set-java-style ()
  (interactive)
  (c-set-style "bsd")
  (setq c-default-style "bsd")
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (add-to-list 'c-hanging-braces-alist '(substatement-open before after)))

(load "~/.emacs.d/personal/zz-overrides")
