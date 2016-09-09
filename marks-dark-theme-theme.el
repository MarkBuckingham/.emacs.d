(deftheme marks-dark-theme
  "Created 2016-09-08.")




(custom-theme-set-faces
 'marks-dark-theme
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(cursor ((t (:background "bisque"))))
 '(fixed-pitch ((t (:family "Consolas"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#c4a000"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#b4fa70"))))
 '(highlight ((t (:inherit nil :background "#3d4753"))))
 '(region ((t (:background "#555753"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:background "#204a87"))))
 '(trailing-whitespace ((t (:foreground "#f0c674" :background "#a40000"))))
 '(font-lock-builtin-face ((t (:foreground "#e090d7"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "#969896" :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#73d216"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "#e9b2e3"))))
 '(font-lock-doc-face ((t (:foreground "#969896" :inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#fce94f"))))
 '(font-lock-keyword-face ((t (:foreground "#b4fa70"))))
 '(font-lock-negation-char-face ((t (:foreground "#b5bd68"))))
 '(font-lock-preprocessor-face ((t (:foreground "#b294bb" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f0c674" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#b294bb" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#e9b96e"))))
 '(font-lock-type-face ((t (:foreground "#8cc4ff"))))
 '(font-lock-variable-name-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#cc6666" :inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#729fcf"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "#3465a4"))))
 '(fringe ((t (:background "#212526"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#2e3436" :background "#d3d7cf"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#b294bb"))))
 '(mode-line-emphasis ((t (:weight bold :slant italic :foreground "#c5c8c6"))))
 '(mode-line-highlight ((t (:weight bold :box (:line-width 2 :color "grey40" :style released-button) :foreground "#b294bb"))))
 '(mode-line-inactive ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#eeeeec" :background "#555753"))))
 '(isearch ((t (:inverse-video t :foreground "#eeeeec" :background "#ce5c00"))))
 '(isearch-fail ((t (:inverse-video t :background "red4" :inherit (font-lock-warning-face)))))
 '(lazy-highlight ((t (:inverse-video t :foreground "#8abeb7" :background "#8f5902"))))
 '(match ((t (:inverse-video t :foreground "#81a2be" :background "RoyalBlue3"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(set-face-attribute 'hl-line nil
                    :background "#3d4753"
                    :foreground nil
                    :inherit t)

(provide-theme 'marks-dark-theme)
