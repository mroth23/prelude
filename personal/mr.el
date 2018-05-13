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


(global-set-key [C-M-\\] 'clang-format-region)

;; Disable guru mode
(setq prelude-guru nil)

;; Python
;; Set tab with to 4.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default tab-width 4)))

(key-chord-define-global "xf" 'iy-go-to-char)
(key-chord-define-global "xd" 'iy-go-to-char-backward)
(key-chord-define python-mode-map  ";;" "\C-e;")
