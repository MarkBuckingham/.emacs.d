;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    auto-complete
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    color-theme
    color-theme-solarized
    company
    eieio
    eclim
    enh-ruby-mode
    exec-path-from-shell
    flycheck
    fringe-helper
    groovy-mode
    ido-ubiquitous
    ido-vertical-mode
    javap-mode
    magit
    markdown-mode+
    multi-web-mode
    nlinum
    osx-clipboard
    paredit
    projectile
    ps-ccrypt
    rainbow-delimiters
    rings
    smex
    sr-speedbar
    tabbar
    tagedit
))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'ps-ccrypt)

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "mark.el")

;; (let (
;;       (filename (concat user-real-login-name ".el")))
;;   (if (file-exists-p filename)
;;       (progn
;;        (message "loading %s" filename)
;;        (load filename)
;;        (message "done loading %s" filename)
;;        )))
       
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   [zenburn-bg zenburn-red zenburn-green zenburn-yellow zenburn-blue zenburn-magenta zenburn-cyan zenburn-fg])
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("bcf948801f26e0fd8457ba89381374ecdb48e5e49b9e3d2a0a12429f1d28f61b" "efbc527f591bc7100e940be0aa8c21a605c23ebe8d27c6409403b55e5f019358" "268012755e212cf096e332494ac9d83b1c968fd1f5020ffe7fd27767bec77f86" "1d4595de36040fb4ea0e34ab3d2d8e479136bc0961c9c3fea6749c44837f4489" "afbf2dfa71ebabd97c39367950801c6a408882674f0005410e65114fe3114e89" "6f0466d00faab424d591f591d367071f1c94ea1aefbf50419a90883428a02683" "217104e8de713f385a641ee57689d27a9dfd9682278620348d4e2bbed33e22d7" "d23c41c946376baf32433d75a26a1444154ce13ff5c454dcda5a6033c023d0d0" "b8e55c735eda9170a5a97c523d99d4a23f7e3cc3052f7ad912e812784e8223c4" "7d1fbf96f08bf306e3780f947555731b506cc530fdd1e81b4f6a6a863626131c" "f19229e3e30a302182e858bd2dceb0e4bf6cdbbeb5f18b8575805098ff5cea2c" "c97ae711eb582b9b3690464b50da7dee40b0672c26a9b282c1b7c028252b7251" "57a2dc126653663e325c4aca063c9c80b3de4eb20d4fae56efa1dad780fea5b4" "c65a18ef8be0d774f2c4e9d4285bf60aec78505bfe1288f8587d23e1ff67b4b0" "bff8bceb6d99a12d1964cdc9f23f039208bf0e53e88a184f8df62f3acf8cf194" "f5427af913f37170d585bcd61231f0c9ab75d61f38b458dd79d6c4a2f6515ebb" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" default)))
 '(fci-rule-color "#00346e")
 '(global-eclim-mode t)
 '(package-selected-packages
   (quote
    (folding company-emacs-eclim tagedit tabbar sr-speedbar smex rings rainbow-delimiters ps-ccrypt projectile paredit osx-clipboard nlinum multi-web-mode markdown-mode+ magit javap-mode ido-vertical-mode ido-ubiquitous groovy-mode fringe-helper flycheck exec-path-from-shell enh-ruby-mode eclim company color-theme-solarized color-theme clojure-mode-extra-font-locking cider auto-complete)))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-default ((t (:height 1.0 :width normal)))))
(put 'downcase-region 'disabled nil)
