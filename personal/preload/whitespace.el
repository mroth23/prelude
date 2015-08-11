;; Remove trailing whitespace before saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn off tabs.
(setq-default indent-tabs-mode nil)
