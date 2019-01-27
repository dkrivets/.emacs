;;; .emacs --- options to drive emacs
;;; Commentary:
;;; Code:
(setq user-full-name "DKrivets")

(defvar my:num-version "26.0.50" "Emacs version.")
;;; Package
(require 'package)

;; Path to plugins installed by hand
(add-to-list 'load-path "~/.emacs.d/plugins/")
;(add-to-list 'load-path "~/Documents/PROJECTS/text-buffer")
;(add-to-list 'load-path "~/Documents/PROJECTS/pretty-print")
(add-to-list 'load-path "~/Documents/PROJECTS/org-parallel")
;; Repos
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Initialize
(package-initialize)

(with-no-warnings (require 'cl))

;; Function to install all my packages
(defun pkg-install (&rest packages)
  "Function to install list of PACKAGES."
  (interactive)
  (message "Start pkg-install!")
  ; Exclude built-in packages list
  (let ((exclude (list 'ido 'linum)))
    ; Delete dups in list
    (let ((pkgs (set-difference (delete-dups packages) exclude)))
    
      ; Check installed packages and delete it from list
      (let ((install-pkgs (remove-if
			   (lambda(item)
			     (package-installed-p item))
			   pkgs)))
        ; If we have an element in list
	(if (> (length install-pkgs) 0)
	    (progn
	      ; Refresh
	      (package-refresh-contents)
	      (mapc (lambda(pkg)
			(message (concat "--------" (symbol-name pkg) "-------"  ))
			(package-install pkg)
			(message "%s has been installed on init" (symbol-name pkg))
			)
		      install-pkgs)
	      (package-initialize))
	  (message "No packages to install!"))))))



;; Install nessesary packages
(pkg-install
 'bs
 'hl-defined
 'magit
 'rainbow-delimiters
 'ido
 'smex
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
 'lua-mode
 'twilight-bright-theme
 'soft-morning-theme
 'all-the-icons
 'all-the-icons-dired
 'imenu-list
 'which-key
 'kaolin-themes
 'apropospriate-theme
 'enh-ruby-mode)

;; Display the name of buffer
(setq frame-title-format "GNU Emacs: %b")

;; Add to exec-path system path to run
;;; system progs
(add-to-list 'exec-path "/usr/local/bin")

;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
(setq-default explicit-bash-args (list "--login" "-i"))

;(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])

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
(defun my:next-window()
  "Go to previous window."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") #'my:next-window)

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

;; Set font
(if (eq system-type 'darwin)
    ;; (set-frame-font "Ubuntu Mono derivative Powerline 13")
    ;; (set-frame-font "Menlo 11")
    ;; (set-frame-font "Ubuntu Mono 13")
		(set-frame-font "Source Code Pro for Powerline 12")
  (set-frame-font "Ubuntu Mono 12"))
(set-frame-font "Source Code Pro for Powerline 12")

;; Don't create backup
(setq make-backup-files -1)

;; Don't save auto
(setq auto-save-default -1)

;; Create backup and autosave
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `((".*" . "~/.emacs.d/backup/"))    ; don't litter my fs tree
	 auto-save-file-name-transforms `((".*" . "~/.emacs.d/auto-save/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Debug on error
(setq debug-on-error t)

;; Don't ask about loading theme
; For SML
(setq-default sml/no-confirm-load-theme t)
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
;(if (display-graphic-p)
;    (load-theme 'twilight-bright t)
;    (load-theme 'soft-morning t))
(if (display-graphic-p)
    ;(load-theme 'kaolin-aurora t)
    (load-theme 'spacemacs-light t)
    ;(load-theme 'apropospriate-light t))
  (load-theme 'monokai t))


;; Scroll
;; scroll step
(setq scroll-step 1)

;; hl-line
;;(global-hl-line-mode 1)

;; Change buffer mode
;;(iswitchb-mode t)

;(setq display-time-format "%I:%M:%S")
(setq-default display-time-24hr-format t
							display-time-day-and-date t)
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

;; Clipboard setting
(setq select-enable-clipboard t
			x-select-enable-clipboard t)

;; For MacOs
(if (eq system-type 'darwin)
    (setq-default mac-option-key-is-meta  nil
									mac-command-key-is-meta t
									mac-command-modifier    'meta
									mac-option-modifier     'super
									mac-allow-anti-aliasing t)    ;; turn on anti-aliasing (default)
  )

;; Resize window only for MacOS
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-c <left>")  'shrink-window-horizontally)   ;; It's also C-x {
      (global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally) ;; It's also C-x }
      (global-set-key (kbd "C-c <down>")  'shrink-window)
      (global-set-key (kbd "C-c <up>")    'enlarge-window)))



;;; Functions
(defun my:hide-line-num ()
  "Hide line number in buffer."
  (interactive)
  (message "my:hide-linum-num START")
  (if (version< emacs-version my:num-version)
      (linum-mode -1)
    (display-line-numbers-mode -1))
  (message "my:hide-linum-num FINISH"))

(defun my:dired-mode-setup ()
  "To be run as hook for `dired-mode'."
  (interactive)
  (message "my:dired-mode-setup START")
  (dired-hide-details-mode 1)
  ;; Don't want show lines.
  (my:hide-line-num)
  ;; ERROR: dired-use-ls-dired
  (when (string= system-type "darwin")
    (setq-default dired-use-ls-dired nil))
    (message "my:dired-mode-setup FINISH"))

;;; Packages
;(require 'text-buffer)
;(text-buffer)

;; Dired
(require 'dired)
(add-hook 'dired-mode-hook 'my:dired-mode-setup)
;; Open file/dir without creating a new buffer
;; Enable function
(put 'dired-find-alternate-file 'disabled nil)
;; Old/default variant is dired-advertised-find-file
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; Old/default variant is dired-up-directory
(define-key dired-mode-map (kbd "^")
	(lambda()
		(interactive)
		(find-alternate-file "..")))


;; Shell
; Hide line numbers
(add-hook 'shell-mode-hook (lambda()
			     (message "shell-mode-hook START")
			     (my:hide-line-num)
			     (message "shell-mode-hook FINISH")))
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;(add-hook 'shell-mode-hook 'ansi-color-process-output)
; Using bash
(setq-default shell-file-name "/bin/bash")
; Change prompt
(setenv "PS1" ">")
(setenv "PROMPT" ">")


;; Org
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c a") 'org-agenda)

;; all-the-icons-dired
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

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
(setq ido-virtual-buffers      t
      ido-enable-flex-matching t
      ido-case-fold            nil)

;; Line numbers
(if (version< emacs-version my:num-version)
    ;; we will use linum
    (progn
      ;; Linum mode
      (require 'linum)
      (setq-default linum-format "%4d")
      (global-linum-mode 1))
  ;; Else we can use build-in mode
  (progn
    ;(display-line-numbers-mode t)
    (global-display-line-numbers-mode t)))


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


;;; Ruby
;; Enh-ruby-mode
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))


;; Robe
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)


;;; Cider
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
(require 'flycheck-package)
(eval-after-load 'flycheck
  '(flycheck-package-setup))


(require 'meghanada)
;(add-hook 'java-mode-hook 'kotlin-mode-hook (lambda()
(add-hook 'java-mode-hook
					(lambda()
						;; meghanada-mode on
						(meghanada-mode t)
						(setq indent-tabs-mode nil
									tab-width        4)
						(setq-default c-basic-offset 4)
						(local-set-key (kbd "C-c /") #'imenu-list-smart-toggle)
						;; use code format
						(add-hook 'before-save-hook
											'meghanada-code-beautify-before-save)))
(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
  (setq meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn")))

;; Projectile
(require 'projectile)
;;; etag
(defvar my:etag "/usr/bin/etags" "The etag full path.")
;;; Projects dir
(setq projectile-project-search-path
			'("~/Documents/PROJECTS/c/"
				"~/Documents/PROJECTS/ciCD/"
				"~/Documents/PROJECTS/clj/"
				"~/Documents/PROJECTS/cpp/"
				"~/Documents/PROJECTS/dev-mode/"
				"~/Documents/PROJECTS/emacs/"
				"~/Documents/PROJECTS/java/"
				"~/Documents/PROJECTS/kotlin/"
				"~/Documents/PROJECTS/org-parallel/"
				"~/Documents/PROJECTS/rsl-mode/"
				"~/Documents/PROJECTS/python/"
				"~/Documents/PROJECTS/text-buffer/"
				"~/Documents/PROJECTS/pretty-print/"))
;;; Dired in project root dir
(setq projectile-switch-project-action #'projectile-dired)
;;;
;(message projectile-tags-command)

;; imenu-list
(require 'imenu-list)
(global-set-key (kbd "C-c /") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t
			imenu-list-auto-resize t
			imenu-list-mode-line-format nil)
;; Auto-refresh list of function in buffer
(setq imenu-auto-rescan t)

;(add-hook 'imenu-list-major-mode-hook 'my:hide-line-num)
;(add-hook 'imenu-list-after-jump-hook 'my:hide-line-num)
;(add-hook 'imenu-list-update-hook #'my:hide-line-num)


;; All the icons (all-the-icons)
(require 'all-the-icons)

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)


;;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;(defvar ruby-imenu-generic-expression
;;    '(("Methods" "^\\(*\\(def\\) +.+\\)"   1))
;;    "The imenu regex to parse an outline of the ruby file")
;;
;;(defun ruby-set-imenu-generic-expression()
;;    (make-local-variable 'imenu-generic-expression)
;;    (make-local-variable 'imenu-create-index-function)
;;    (setq imenu-create-index-function 'imenu-default-create-index-function)
;;    (setq imenu-generic-expression ruby-imenu-generic-expression))
;;
;;(add-hook 'ruby-mode-hook 'ruby-set-imenu-generic-expression)

;(eval-after-load "ruby-mode"
;  '(progn
;     (defun ruby-imenu-create-index-in-block (prefix beg end)
;       (let ((index-alist '()) (case-fold-search nil)
;             name next pos decl sing)
;         (goto-char beg)
;         (while (re-search-forward "^\\s *\\(\\(class\\s +\\|\\(class\\s *<<\\s *\\)\\|module\\s +\\)\\([^\(<\n ]+\\)\\|\\(def\\|alias\\|get\\|post\\|describe\\|context\\)\\s +\\([^\(\n]+\\)\\)" end t)
;           (setq sing (match-beginning 3))
;           (setq decl (match-string 5))
;           (setq next (match-end 0))
;           (setq name (or (match-string 4) (match-string 6)))
;           (setq pos (match-beginning 0))
;           (cond
;            ((or (string= "get" decl) (string= "post" decl)
;                (string= "context" decl) (string= "describe" decl))
;             (setq name (concat decl " " (replace-regexp-in-string "['\"]" "" name)))
;             (if prefix (setq name (concat prefix name)))
;             (push (cons name pos) index-alist))
;            ((string= "alias" decl)
;             (if prefix (setq name (concat prefix name)))
;             (push (cons name pos) index-alist))
;            ((string= "def" decl)
;             (if prefix
;                 (setq name
;                       (cond
;                        ((string-match "^self\." name)
;                         (concat (substring prefix 0 -1) (substring name 4)))
;                        (t (concat prefix name)))))
;             (push (cons name pos) index-alist)
;             (ruby-accurate-end-of-block end))
;            (t
;             (if (string= "self" name)
;                 (if prefix (setq name (substring prefix 0 -1)))
;               (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
;               (push (cons name pos) index-alist))
;             (ruby-accurate-end-of-block end)
;             (setq beg (point))
;             (setq index-alist
;                   (nconc (ruby-imenu-create-index-in-block
;                           (concat name (if sing "." "#"))
;                           next beg) index-alist))
;             (goto-char beg))))
;         index-alist))))




(message "Configure has been loaded!")
;;; .emacs ends here
