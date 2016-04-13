(message "mark.el start")

;;;;;;;;;;;;;;;;;;
;;; my functions
;;;;;;;;;;;;;;;;;

;; center screen
(defun marks-center-screen (arg)
  (if (window-system)
      (progn

        (if (not (boundp 'my-face-height))
            (setq my-face-height 140)
          )        

        (set-face-attribute 'default nil :height my-face-height)

        (setq my-pixel-width (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
        (setq my-pixel-height (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

        (let (
              (cols 
               (min 132 
                    (/ my-pixel-width (frame-char-width (selected-frame))))
               )

              (rows 
               (min 50 
                    (/ my-pixel-height (frame-char-height (selected-frame))))
               )
              )
          (set-frame-size (selected-frame) cols rows)

          (add-to-list 'initial-frame-alist '(width . cols))
          (add-to-list 'initial-frame-alist '(height . rows))

          (let ((xpos (- (/ my-pixel-width 2) (/ (frame-pixel-width) 2)))
                (ypos (- (/ my-pixel-height 2) (/ (frame-pixel-height) 2))))
            (set-frame-position (selected-frame) xpos ypos)
            )
          )
        (xterm-mouse-mode 0)
        )
    ))

;; <s-kp-subtract> - make font smaller
(defun marks-make-font-smaller (arg)
  (interactive "p")
  (setq my-face-height (- my-face-height 10))
  (set-face-attribute 'default nil :height my-face-height)
  (marks-center-screen nil))

;; <s-kp-add> - make font larger
(defun marks-make-font-larger (arg)
  (interactive "p")
  (setq my-face-height (+ my-face-height 10))
  (set-face-attribute 'default nil :height my-face-height)
  (marks-center-screen nil))

;; <s-f1> - dark color theme
(defun marks-dark-theme (arg)
  (interactive "p")
<<<<<<< HEAD
  (load-theme 'tango-dark))

;; <s-f2> - light color theme
(defun marks-light-theme (arg)
  (interactive "p")
  (load-theme 'tango))
=======
   (load-theme 'tomorrow-night)
   )

;; <s-f2> - light color theme
 (defun marks-light-theme (arg)
   (interactive "p")
   (load-theme 'tomorrow-daytime)
   )
>>>>>>> e464e63f6e401171d1ba26343fae7153b77dfe1c

;; super+9 jump to matching paren or brace
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((looking-at "\\s{") (forward-char 1) (backward-list 1))
        ((looking-at "\\s}") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff to run after initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 
 'after-init-hook
 `(lambda ()
	(message "before marks-center-screen")
    (marks-center-screen nil)
	(message "after marks-center-screen")

    (if window-system
        (progn 
          (xterm-mouse-mode 0)
          (scroll-bar-mode 1)
        )
      (xterm-mouse-mode 1)
      )

    ;; tabbar configuration

    (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
      "Returns the name of the tab group names the current buffer belongs to.
       There are two groups: Emacs buffers (those whose name starts with '*', plus
       dired buffers), and the rest.  This works at least with Emacs v24.2 using
       tabbar.el v1.7."
      (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                  ((eq major-mode 'dired-mode) "emacs")
                  (t "user"))))
    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
    
    ;; eclim stuff
    ;; note that there are a bunch of eclim key shortcuts already defined
	(message "eclim config start")
    (setq eclim-executable "~/Applications/Eclipse.app/Contents/Eclipse/eclim")
    (global-set-key (kbd "C-S-r") 'eclim-file-locate)
    (require 'company)
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)
    (global-company-mode t)
	(message "eclim config end")

    ;; may want to move this to a different file
    (require 'ido-vertical-mode)
    (ido-mode 1)
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down)

    ;; make emacs save backup files in /tmp
    (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

    ;; purge old backup files
    (message "Deleting old backup files...")
    (let ((week (* 60 60 24 7))
          (current (float-time (current-time))))
      (dolist (file (directory-files temporary-file-directory t))
        (when (and (backup-file-name-p file)
                   (> (- current (float-time (fifth (file-attributes file))))
                      week))
          (message "%s" file)
          (delete-file file)))
      )

    ;; columns
    (column-number-mode 1)

    ;; rainbows
    ;;(global-rainbow-delimiters-mode t)

    ;; special (non-minor-mode) keybindings
    (if (eq system-type 'darwin)
        (progn
          (message "running on darwin")
          )
      (progn
        ;; darwin compatible "Super" key bindings
        (message "not running on darwin")
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
        (global-set-key (kbd "s-f") 'isearch-forward-regexp)
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
    (global-set-key (kbd "<s-right>") 'next-buffer)
    (global-set-key (kbd "<s-left>") 'previous-buffer)
    (global-set-key (kbd "<home>") 'move-beginning-of-line)
    (global-set-key (kbd "<end>") 'move-end-of-line)
    (global-set-key (kbd "s-9") 'goto-match-paren)  ; super + 9 ['(' is a shift+9]
    (global-set-key (kbd "<s-kp-subtract>") 'marks-make-font-smaller) ; super + keypad '-'
    (global-set-key (kbd "<s-kp-add>") 'marks-make-font-larger) ; super + keypad '+'
    (global-set-key (kbd "s-=")  'marks-make-font-smaller) ; super + '='
    (global-set-key (kbd "s-+") 'marks-make-font-larger) ; super + '+'
    (global-set-key (kbd "<s-f1>") 'marks-dark-theme) ; super + f1
    (global-set-key (kbd "<s-f2>") 'marks-light-theme) ; super + f2
    (global-set-key (kbd "<C-next>") 'next-buffer) ; ctrl + pgDn
    (global-set-key (kbd "<C-prior>") 'previous-buffer) ; ctrl + pgUp
    (global-set-key (kbd "<C-M-next>") 'tabbar-forward-tab) ; ctrl + alt + pgDn
    (global-set-key (kbd "<C-M-prior>") 'tabbar-backward-tab) ; ctrl + alt + pgUp
   
;;    (color-theme-gnome2)
    )
 )

;; org mode stuff

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)
            (auto-fill-mode t)
            ))

;; rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(menu-bar-mode t)
(tabbar-mode t)
(cua-mode t)

(message "mark.el end")
