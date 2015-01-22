(message "mark.el start")

;; note: we can figure out if we're on vnc by looking at (getenv "DISPLAY")

(add-hook 'after-init-hook
          `(lambda () 
             (if (window-system)
                 (progn
                   ;; change font size based on os & display type
                   (if (eq system-type 'darwin)
                       ;; mac
                       (setq my-face-height 140)
                     (if (string= (getenv "DISPLAY") ":0.0")
                         ;; linux root display
                         (setq my-face-height 130)
                       ;; linux over vnc
                       (setq my-face-height 140)
                       )
                     )
                   (set-face-attribute 'default nil :height my-face-height)

                   (setq my-pixel-width (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
                   (setq my-pixel-height (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

                   ;; subtract 5 cols / rows off of computed value to add room for window chrome
                   (let (
                         ;(cols (min 132 (- (/ my-pixel-width (frame-char-width (selected-frame))))))
                         ;(rows (min 50 (- (/ my-pixel-height (frame-char-height (selected-frame))))))
                         ;(cols (- (/ my-pixel-width (frame-char-width (selected-frame)))))
                         ;(rows (- (/ my-pixel-height (frame-char-height (selected-frame)))))
                         
                         ;(cols (- (/ my-pixel-width (frame-char-width (selected-frame))) 5))
                         ;(rows (- (/ my-pixel-height (frame-char-height (selected-frame))) 5))
                         (cols 
                          (min 132 
                               (/ my-pixel-width (frame-char-width (selected-frame))))
                          )

                         (rows 
                          (min 50 
                               (/ my-pixel-height (frame-char-height (selected-frame))))
                          )
                         )
                     (message "cols: %d, rows: %d\n" cols rows)
                     (set-frame-size (selected-frame) cols rows)

                     (add-to-list 'initial-frame-alist '(width . cols))
                     (add-to-list 'initial-frame-alist '(height . rows))

                     (let ((xpos (- (/ my-pixel-width 2) (/ (frame-pixel-width) 2)))
                           (ypos (- (/ my-pixel-height 2) (/ (frame-pixel-height) 2))))
                       (set-frame-position (selected-frame) xpos ypos)
                       ;(add-to-list 'initial-frame-alist '(width . cols))
                       ;(add-to-list 'initial-frame-alist '(height . rows))
                       )
                     )
                   (xterm-mouse-mode 0)
                   )
               (progn
                 (message "tty")
                 ;; add tty-appropriate things here
                 (xterm-mouse-mode 1)
                 )
               )
             
             )
          )

;; eclim stuff
(setq eclim-executable "~/eclipse/eclim")
(global-set-key (kbd "C-S-r") 'eclim-file-locate)
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;; may want to move this to a different file
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down)

;; special (non-minor-mode) keybindings
(if (eq system-type 'darwin)
    (progn
      (nil)
      )
  (progn
    ;; darwin compatible "Super" key bindings
    (global-set-key (kbd "s-&") 'kill-this-buffer)
    (global-set-key (kbd "s-'") 'next-multiframe-window)
    (global-set-key (kbd "s-,") 'customize)
    (global-set-key (kbd "s--") 'center-line)
    (global-set-key (kbd "s-:") 'ispell)
    (global-set-key (kbd "s-?") 'info)
    (global-set-key (kbd "s-D") 'dired)
    (global-set-key (kbd "s-E") 'edit-abbrevs)
    (global-set-key (kbd "s-L") 'shell-command)
    (global-set-key (kbd "s-M") 'manual-entry)
    (global-set-key (kbd "s-^") 'kill-some-buffers)
    (global-set-key (kbd "s-`") 'other-frame)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-d") 'isearch-repeat-backward)
    (global-set-key (kbd "s-e") 'isearch-yank-kill)
    (global-set-key (kbd "s-f") 'isearch-forward)
    (global-set-key (kbd "s-g") 'isearch-repeat-forward)
    (global-set-key (kbd "s-j") 'exchange-point-and-mark)
    (global-set-key (kbd "s-k") 'kill-this-buffer)
    (global-set-key (kbd "s-l") 'goto-line)
    (global-set-key (kbd "s-m") 'iconify-frame)
    (global-set-key (kbd "s-n") 'make-frame)
    (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
    (global-set-key (kbd "s-s") 'save-buffer)
    (global-set-key (kbd "s-u") 'revert-buffer)
    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-w") 'delete-frame)
    (global-set-key (kbd "s-x") 'kill-region)
    (global-set-key (kbd "s-z") 'undo)
    (global-set-key (kbd "s-|") 'shell-command-on-region)
    )
)

;; keybindings for everybody
(global-set-key (kbd "<s-next>") 'next-multiframe-window) ; Super-pgDn
(global-set-key (kbd "<s-prior>") 'previous-multiframe-window) ; Super-pgUp
(global-set-key (kbd "<M-next>") 'next-buffer) ; Alt-pgDn
(global-set-key (kbd "<M-prior>") 'previous-buffer) ; Alt-pgUp 



(message "mark.el end")
