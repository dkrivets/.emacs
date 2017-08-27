;;; .emacs --- options to drive emacs
;;; Commentary:
;;; Code:
(setq user-full-name "DKrivets")

(setq debug-on-error t)
;;; Package
(require 'package)

;; Path to plugins installed by hand
(add-to-list 'load-path "~/.emacs.d/plugins/")
;; Repos
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))


;; Function to install all my packages
(defun pkg-install (&rest packages)
  "Function to install list of packages."
  (interactive )
  (package-refresh-contents)
  (mapcar (lambda(pkg)
	    (message (concat "--------" (symbol-name pkg) "-------"  )
	    (if (not (eq nil pkg))
		(progn
		  (message "2:")
		  (if (eq t (package-installed-p pkg))
		      (progn
			(message "3:")
			(message "%s: has been already installed" (symbol-name pkg) ))
		    (progn
		      (message "4:")
		      (package-install pkg)
		      (message "%s has been installed on init" (symbol-name pkg)))))
	      (message (concat "nil package " (symbol-name pkg)) )) ))
	  (delete-dups packages)))

;; Initialize
(package-initialize)

;; Install nessesary packages 
(pkg-install
 'bs
 'hl-defined
 'magit
 'rainbow-delimiters
 'ido
 'linum
 'hlinum
 'auto-complete
 'clojure-mode
 'cider
 'ac-cider
 'python-mode
 'ipython
 'flycheck
 'meghanada
 'twilight-bright-theme
 'soft-morning-theme)

;; Display the name of buffer
(setq frame-title-format "GNU Emacs: %b")

;; Add to exec-path system path to run
; system progs
(add-to-list 'exec-path "/usr/local/bin")

;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
(setq-default explicit-bash-args (list "--login" "-i"))

;; fix the PATH variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx] 
(defun my:set-exec-path-from-shell-path ()
  "Fix path variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx]."
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (my:set-exec-path-from-shell-path))


;;;; Emacs standard options
;; Highlight select region
(setq-default transient-mark-mode t)

;; Syntax highlight
(setq font-lock-maximum-decoration t)

;; Width of the screen
(setq fill-column 20)

;; Don't open startup message
(setq inhibit-startup-message t)

;;;; Tab as Space
(setq indent-tabs-mode -1)
;; Set tabs as 2 char
(setq tab-width 2)
(setq-default c-basic-offset 2)

;; Previous window
(defun my:previous-window()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'my:previous-window)

;; dont use line wrap
;(setq toggle-trancate-lines t)
;(setq word-wrap -1)
;(setq toggle-trancate-lines 1)
;(setq global-visual-line-mode t)
;(setq trancate-lines t)
;(setq default-truncate-lines t)
(setq-default truncate-lines t)
;(setq global-visual-line-mode t)


;; highlight lisp expression
(setq-default show-paren-style 'expression)
(show-paren-mode 2)

;; Auto indent brackets
(electric-indent-mode 1)
(electric-pair-mode 1)

;; Set font Ubuntu Mono
;(set-frame-font "Ubuntu Mono derivative Powerline 13")
;(set-frame-font "Menlo 11")
(set-frame-font "Ubuntu Mono 13")
;; Don't create backup
(setq make-backup-files -1)

;; Don't save auto
(setq auto-save-default -1)

;; Debug on error
(setq debug-on-error t)

;; Don't ask about loading theme
(setq sml/no-confirm-load-theme t)
(setq custom-safe-themes t)

;; Mode-line(status-line)
; No box border
(set-face-attribute 'mode-line nil :box nil)
;; Set default theme
;(load-theme 'leuven)
;(load-theme 'twilight-bright t)
;(if (display-graphic-p)
;    (progn 
;      (load-theme 'atom-dark-theme))
;  (load-theme 'flatui))
;(load-theme 'flatui)
(if (display-graphic-p)
    (load-theme 'twilight-bright t)
    (load-theme 'soft-morning t))

;; Scroll
;; scroll step
(setq scroll-step 1)
;; hl-line
(global-hl-line-mode 1)

;; Change buffer mode
;;(iswitchb-mode t)

;(setq display-time-format "%I:%M:%S")
(setq-default display-time-24hr-format t)
(setq-default display-time-day-and-date t)
(display-time)

;; tab-bar
(tool-bar-mode -1)
;; scroll bar (scroll-bar-mode -1)
;; menu bar (menu-bar-mode -1) 

;; Make emacs ask for abbrev
(fset 'yes-or-no-p 'y-or-n-p)

;; Column number
(column-number-mode t)

;; Show battery 
(display-battery-mode t)

;; File-size
(size-indication-mode t)

;; For MacOs
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Resize window only for MacOS
(if (eq system-type 'darwin)
  (global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-c <down>") 'shrink-window)
  (global-set-key (kbd "C-c <up>") 'enlarge-window)
)
;;; Packages

;; Dired
(defun my:dired-mode-setup ()
  "To be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my:dired-mode-setup)

;; Powerline and themes: airline-themes
;(require 'cl)
;(require 'powerline)
;(require 'airline-themes)
;(setq ns-use-srgb-colorspace nil) 
;(setq powerline-height 19)
;(setq powerline-default-separator-dir '(left . right))
;(setq powerline-default-separator 'arrow)
;(load-theme 'airline-papercolor t)


;; BS Show buffers
(require 'bs)
(global-set-key (kbd "<f2>") 'bs-show)

;; hl-defined
(require 'hl-defined)
(hdefd-highlight-mode 1)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

;; Rainbow
; highlight brackets
(require 'rainbow-delimiters)
(rainbow-delimiters-mode t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Ido
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Linum mode
(require 'linum)
(setq linum-format "%4d")
(global-linum-mode 1)

;; hlinum
; highline cursor line 
(require 'hlinum)
(hlinum-activate)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Auto-completion for Clojure
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun my:set-auto-complete-as-completion-at-point-function ()
  "Hook for autocomplete of cider."
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'my:set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'my:set-auto-complete-as-completion-at-point-function)


;; Cider
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(setq-default nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
;; TabBar and TabBar-ruler
;(require 'tabbar)
;(require 'tabbar-ruler)
;(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;;(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;;(setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the



;(require 'xah-elisp-mode)
;(with-eval-after-load 'xah-elisp-mode
;  (add-hook 'emacs-lisp-mode-hook 'xah-elisp-mode))

;; Flycheck. Check syntax on-the-fly
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'emacs-lisp-checkdoc 'xah-elisp-mode)
  (flycheck-add-mode 'emacs-lisp 'xah-elisp-mode))

(require 'meghanada)
(add-hook 'java-mode-hook 'kotlin-mode-hook (lambda()
			    ;; meghanada-mode on
			    (meghanada-mode t)
			    (setq indent-tabs-mode nil)
			    (setq-default c-basic-offset 2)
			    (setq tab-width 4)
			    ;; use code format
			    (add-hook 'before-save-hook
				      'meghanada-code-beautify-before-save)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(kotlin-tab-width 4)
 '(package-selected-packages
   (quote
    (groovy-mode gradle-mode kotlin-mode flycheck-kotlin meghanada chess flycheck-clojure color-theme-heroku color-theme-molokai color-theme-monokai clojure-mode soft-morning-theme atom-dark-theme solarized-theme yasnippet xah-find xah-elisp-mode vlf ubuntu-theme twilight-bright-theme smart-mode-line s rainbow-delimiters python-mode paper-theme mode-icons material-theme magit ipython hlinum hl-defined hemisu-theme flycheck flatui-theme evil emacsql-sqlite emacsql-psql ein cider-decompile ac-nrepl ac-helm ac-cider))))

;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
