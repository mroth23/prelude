;; personal-font.el - Chooses a suitable font from a list of font preferences

(require 'cl)

(set-face-attribute 'default nil :height 110)

(defun personal-font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

(defun personal-font-avail (fonts)
  "Finds the available fonts."
  (remove-if-not 'personal-font-existsp fonts))

(defvar personal-font-preferences
      '("PragmataPro"
        "Inconsolata"
        "DejaVu Sans Mono"
        "Bitstream Vera Sans Mono"
        "Anonymous Pro"
        "Menlo"
        "Consolas"))

(unless (eq window-system nil)
  (let ((fonts (personal-font-avail personal-font-preferences)))
    (unless (null fonts)
      (set-face-attribute
       'default nil :font
       (car fonts))))) 
