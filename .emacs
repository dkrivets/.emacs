;;; .emacs --- options to drive emacs
;;; Commentary:
;;; Code:
(setq user-full-name "DKrivets")

(setq debug-on-error t)

(setq-default my:num-version "26.0.50")
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
; system progs
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
(defun my:previous-window()
  "Go to previous window."
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

;; Set font
(if (eq system-type 'darwin)
    ;(set-frame-font "Ubuntu Mono derivative Powerline 13")
    ;(set-frame-font "Menlo 11")
    (set-frame-font "Ubuntu Mono 13")
  (set-frame-font "Ubuntu Mono 12"))
;; Don't create backup
(setq make-backup-files -1)

;; Don't save auto
(setq auto-save-default -1)

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

;; Clipboard setting
(setq x-select-enable-clipboard t)

;; For MacOs
(if (eq system-type 'darwin)
    (progn
      (setq-default mac-option-key-is-meta nil)
      (setq-default mac-command-key-is-meta t)
      (setq-default mac-command-modifier 'meta)
      (setq-default mac-option-modifier nil)
      (setq mac-allow-anti-aliasing t)    ;; turn on anti-aliasing (default)
      ))

;; Resize window only for MacOS
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
      (global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
      (global-set-key (kbd "C-c <down>") 'shrink-window)
      (global-set-key (kbd "C-c <up>") 'enlarge-window)))



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
    (setq dired-use-ls-dired nil))
    (message "my:dired-mode-setup FINISH"))

;;; Packages

;; Dired
(require 'dired)
(add-hook 'dired-mode-hook 'my:dired-mode-setup)
;; Open file/dir without creating a new buffer
;; Enable function
(put 'dired-find-alternate-file 'disabled nil)
;; Old/default variant is dired-advertised-find-file
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; Old/default variant is dired-up-directory
(define-key dired-mode-map (kbd "^") (lambda()
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

;; Line numbers
(if (version< emacs-version my:num-version)
    ;; we will use linum
    (progn
      ;; Linum mode
      (require 'linum)
      (setq linum-format "%4d")
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

;; imenu-list
(require 'imenu-list)
(global-set-key (kbd "C-c .") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)
(setq imenu-list-mode-line-format nil)

(add-hook 'imenu-list-major-mode-hook 'my:hide-line-num)
(add-hook 'imenu-list-after-jump-hook 'my:hide-line-num)
(add-hook 'imenu-list-update-hook #'my:hide-line-num)


;; All the icons (all-the-icons)
(require 'all-the-icons)

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(beacon-color "#F8BBD0")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "4455435a66dba6e81d55a843c9c7e475a7a935271bf63a1dfe9f01ed2a4d7572" "fc524ddf651fe71096d0012b1c34d08e3f20b20fb1e1b972de4d990b2e793339" "c4d3cbd4f404508849e4e902ede83a4cb267f8dff527da3e42b8103ec8482008" "f72ccaa311763cb943de5f9f56a0d53b0009b772f4d05f47835aa08011797aa8" "7e362b29da8aa9447b51c2b354d8df439db33b3612ddd5baa34ad3de32206d83" "b8bb8a91752c68df1de3b790fe5bbe5e39441488d19851654ee0d2875bc6f94b" "9076ed00a3413143191cb9324d9426df38d83fb6dba595afbd43983db1015ef4" "f9567e839389f2f0a1ede73d1c3e3bd2c9ed93adaf6bb7d13e579ea2b15fcef8" "b7d967c53f4e3dfc1f847824ffa3f902de44d3a99b12ea110e0ec2fcec24501d" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "663a653b805b97978c624687b67861f80dddceffc3ae434aa4c60bd22d12e70b" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(hl-sexp-background-color "#efebe9")
 '(kotlin-tab-width 4)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (robe spaceline gnugo spacemacs-theme enh-ruby-mode apropospriate-theme kaolin-themes color-theme-github monokai-theme which-key imenu-list rsense command-log-mode all-the-icons all-the-icons-dired espresso-theme lua-mode groovy-mode gradle-mode kotlin-mode flycheck-kotlin meghanada chess flycheck-clojure color-theme-heroku color-theme-molokai color-theme-monokai clojure-mode soft-morning-theme atom-dark-theme solarized-theme yasnippet xah-find xah-elisp-mode vlf ubuntu-theme twilight-bright-theme smart-mode-line s rainbow-delimiters python-mode paper-theme mode-icons material-theme magit ipython hlinum hl-defined hemisu-theme flycheck flatui-theme evil emacsql-sqlite emacsql-psql ein cider-decompile ac-nrepl ac-helm ac-cider)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tabbar-background-color "#ffffffffffff")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Configure has been loaded!")

;;; .emacs ends here
