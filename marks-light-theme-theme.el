(deftheme marks-light-theme
  "Created 2016-09-08.")

(custom-theme-set-faces
 'marks-light-theme
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "#2e3436" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(cursor ((t (:background "#204a87"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#a40000"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#204a87"))))
 '(highlight ((t (:background "NavajoWhite"))))
 '(region ((t (:background "#babdb6"))))
 '(shadow ((t (:foreground "#969896"))))
 '(secondary-selection ((t (:background "#8cc4ff"))))
 '(trailing-whitespace ((t (:foreground "#f0c674" :background "#ef2929"))))
 '(font-lock-builtin-face ((t (:foreground "#75507b"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "#969896"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#5f615c"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "#204a87"))))
 '(font-lock-doc-face ((t (:foreground "#969896"))))
 '(font-lock-function-name-face ((t (:foreground "#a40000"))))
 '(font-lock-keyword-face ((t (:foreground "#346604"))))
 '(font-lock-negation-char-face ((t (:foreground "#b5bd68"))))
 '(font-lock-preprocessor-face ((t (:foreground "#b294bb"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f0c674"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#b294bb"))))
 '(font-lock-string-face ((t (:foreground "#5c3566"))))
 '(font-lock-type-face ((t (:foreground "#204a87"))))
 '(font-lock-variable-name-face ((t (:foreground "#b35000"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#cc6666"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#204a87"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "#3465a4"))))
 '(fringe ((t (:background "#d3d7cf"))))
 '(header-line ((t (:foreground "#b294bb" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#2e3436" :background "#d3d7cf"))))
 '(mode-line-buffer-id ((t (:foreground "#b294bb"))))
 '(mode-line-emphasis ((t (:slant italic :foreground "#c5c8c6"))))
 '(mode-line-highlight ((t (:weight bold :box nil :foreground "#b294bb"))))
 '(mode-line-inactive ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#2e3436" :background "#888a85"))))
 '(isearch ((t (:inverse-video t :foreground "#ffffff" :background "#ce5c00"))))
 '(isearch-fail ((t (:inverse-video t :background "#1d1f21" :inherit (font-lock-warning-face)))))
 '(lazy-highlight ((t (:inverse-video t :foreground "#8abeb7" :background "#e9b96e"))))
 '(match ((t (:inverse-video t :foreground "#81a2be" :background "#1d1f21"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(set-face-attribute 'hl-line nil
                    :background "#A2B5CD"
                    :foreground nil
                    :inherit t)

(provide-theme 'marks-light-theme)