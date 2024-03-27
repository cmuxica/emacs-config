;; TODO: SET-UP LSP FOR haskell
;; TODO: SET-UP LSP FOR python
;; TODO: set up LSP for elisp
;; TODO: set up LSP for web
;; TODO: add thesaurus
;; TODO: latex snippets

;;;; PACKAGE MANAGEMENT ;;;;
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;;; LOAD CUSTOM ELISP FILES ;;;;
(load (expand-file-name "cm-funcs.el" user-emacs-directory))

;;;; SOME SENSIBLE DEFAULTS ;;;;
(use-package emacs
  :ensure nil
  :config
  (setq frame-resize-pixelwise t ;; let frames fill the screen
    default-directory "~/" ;; set home as default directory
    ns-pop-up-frames nil ;; put files opened outside of emacs into existing frame
    initial-frame-alist '((fullscreen . maximized)) ;; maximize frame on start-up
    tab-width 4 ;; indent are 4 spaces long
    initial-scratch-message ";; DIGITAL SCRATCHPAD \n" ;; no comment in scratch buffer
    backup-directory-alist '((".*" . "~/.Trash"))) ;; hide ugly back-up files

  (setq-default word-wrap t ;; wrap lines at word boundary
                split-width-threshold 1 ;; split vertically
                indent-tabs-mode nil) ;; indents are spaces not tabs

  ;; better scrolling experience
  (setq scroll-margin 0
    scroll-conservatively 101 ;; > 100
    scroll-preserve-screen-position t
    auto-window-vscroll nil)

  ;; suppress various UI elements
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  ;; add column number to modeline
  (column-number-mode t))

;; hide custom variables
(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "custom.el")))

;; hide ugly auto-save files
(use-package no-littering
  :ensure t)

;; use osx trash when deleting files
(use-package osx-trash
  :ensure t
  :commands osx-trash-move-file-to-trash
  :init
  (setq delete-by-moving-to-trash t))

;;;; BINDINGS ;;;;
;; vim bindings with evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil) ;; do not load default bindings
  (setq evil-undo-system 'undo-fu) ;; use undo-fu for undo/redo
  (setq evil-want-C-u-scroll t) ;; C-u is scroll up
  :preface
  (defun cm/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  ;; vim-like quitting/saving
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'cm/save-and-kill-this-buffer)
  (evil-mode 1))

;; vim bindings everywhere
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; general for leader-based keybindings
(use-package general
  :ensure t
  :config
  (general-define-key
   :states 'motion
   "s-/" '(comment-line :which-key "comment line"))
  ;; set leader key
  (general-create-definer cm/leader
    :prefix "SPC")
  ;; set local leader key
  (general-create-definer cm/local-leader
    :prefix ",")
  ;; set leader bindings
  (cm/leader
    :states 'normal
    :keymaps 'override
    ;; misc quick access
    "," '(counsel-M-x :which-key "M-x")
    "<" '(eval-expression :which-key "evaluate expression")
    "." '(counsel-find-file :which-key "find file")
    "SPC" '(counsel-find-file :which-key "find file")
    ">" '((lambda() (interactive)(counsel-find-file "~/")) :which-key "find file")
    "/" '(swiper :which-key "swiper")
    ";" '(cm/switch-to-scratch-buffer :which-key "scratch buffer")
    ":" '(cm/new-empy-buffer :which-key "empty buffer")
    "'" '(project-find-file :which-key "p-find file")
    "RET" '(counsel-bookmark :which-key "bookmarks")
    "[" '(calendar :which-key "calendar")
    "]" '(vterm :which-key "vterm")
    ;; l is for latex
    "l" '(:ignore n :which-key "latex")
    "lh" '(cm/copy-handout-template :which-key "handout")
    "lb" '(cm/copy-beamer-template :which-key "beamer")
    "lp" '(cm/copy-paper-template :which-key "paper")
    ;; h is for help
    "h" '(:ignore h :which-key "help")
    "hf" '(counsel-describe-function :which-key "describe function")
    "hb" '(counsel-descbindings :which-key "describe bindings")
    "hv" '(counsel-describe-variable :which-key "describe variable")
    "hm" '(describe-mode :which-key "describe mode")
    "hc" '(helpful-command :which-key "describe command")
    "hk" '(helpful-key :which-key "describe key")
    ;; d is for deft
    "d" '(deft-find-file :which-key "deft find")
    "D" '(deft :which-key "deft")
    ;; c is for capture
    "c" '(cm/capture-md :which-key "capture")
    ;; o is for open
    "o" '(:ignore o :which-key "open")
    "ot" '(vterm :which-key "vterm")
    "oc" '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :which-key "config")
    "oo" '(cm/open-buffer-file-name :which-key "open in OSX")
    ;; b is for buffer
    "b" '(:ignore b :which-key "buffer")
    "bb" '(counsel-ibuffer :which-key "ibuffer")
    "bq" '(kill-this-buffer :which-key "kill buffer")
    "bQ" '(cm/kill-other-buffers :which-key "kill other buffers")
    ;; previous buffer in a given window
    "-" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "=" '(cm/toggle-maximize-buffer :which-key "maximize buffer")
    ;; w is for window
    "w" '(:ignore w :which-key "window")
    "wn" '(make-frame :which-key "make frame")
    "wm" '(cm/toggle-maximize-buffer :which-key "maximize buffer")
    "wq" '(evil-window-delete :which-key "delete window")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "wh" '(evil-window-left :which-key "left")
    "wj" '(evil-window-down :which-key "down")
    "wk" '(evil-window-up :which-key "up")
    "wl" '(evil-window-right :which-key "right")
    "wv" '(evil-window-vsplit :which-key "vsplit")
    "ws" '(evil-window-exchange :which-key "swap")
    "wH" '(evil-window-move-far-left :which-key "move left")
    "wJ" '(evil-window-move-very-bottom :which-key "move down")
    "wK" '(evil-window-move-very-top :which-key "move up")
    "wL" '(evil-window-move-far-right :which-key "move right")
    ;; t is for toggle
    "t"  '(:ignore t :which-key "toggle")
    "tt" '(cm/toggle-theme :which-key "light/dark theme")
    "th" '(counsel-load-theme :which-key "load theme")
    "tv" '(visual-line-mode :which-key "visual line mode")
    "tl" '(display-line-numbers-mode :which-key "display line numbers")
    "tc" '(display-fill-column-indicator-mode :which-key "fill column indicator")
    "tm" '(hide-mode-line-mode :which-key "hide modeline mode")
    "tw" '(writeroom-mode :which-key "writeroom-mode")
    )
  ;; set emacs lisp bindings
  (cm/local-leader
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(eval-last-sexp :which-key "evaluate sexp")
    "p" '(check-parens :which-key "check parens"))
  ;; set latex bindings
  (cm/local-leader
    :states 'normal
    :keymaps '(tex-mode-map latex-mode-map TeX-mode-map LaTeX-mode-map)
    "c" '(TeX-command-run-all :which-key "compile")
    "b" '(tex-bibtex-file :which-key "bibtex")))

;;;; AESTHETICS ;;;;
(set-frame-font "Roboto Mono 16" nil t) ;; set font family/size

(defvar dark-theme 'doom-tomorrow-night) ;; set dark-theme
(defvar light-theme 'doom-tomorrow-day) ;; set light-theme

;; themes from doom emacs
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme dark-theme t) ;; set dark theme by default
  (doom-themes-visual-bell-config)) ;; modeline flash on error

;; modeline from doom emacs
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq
   doom-modeline-bar-width 5
   doom-modeline-height 30
   doom-modeline-buffer-files-name-style 'file-name ;; just show file name (no path)
   doom-modeline-enable-word-count t))

;; useful to hide modeline sometimes
(use-package hide-mode-line
  :ensure t
  :hook (vterm-mode . hide-mode-line-mode))

;; automatically match titlebar to current theme
(use-package ns-auto-titlebar
  :ensure t
  :init (ns-auto-titlebar-mode t))

;; custom welcome page
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
    dashboard-bannes-logo-title "Welcome to Emacs!"
    dashboard-items nil ;; TODO: add bookmarks/recent-files/projects
    dashboard-set-footer nil
    dashboard-set-init-info t))

;; enhance various emacs commands
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
     :map minibuffer-local-map
     ("C-r" . 'counsel-minibuffer-history)))

;; improved descriptions of functions/variables/etc
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;; COMPLETION ;;;;
;; completion engine
(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)
     :map ivy-minibuffer-map
     ("TAB" . ivy-alt-done)
     ("C-l" . ivy-alt-done)
     ("C-j" . ivy-next-line)
     ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))

;; add documentation to ivy
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

;; incrementally display bindings
(use-package which-key
  :ensure t
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.25)) ;; wait 1/4 of a second

;;;; TERMINAL ;;;;
;; terminal emulator based on libvterm
(use-package vterm
  :ensure t
  :config
  ;; buffer is useless after exit
  (setq vterm-kill-buffer-on-exit t)
  (setq confirm-kill-processes nil)
  (setq vterm-max-scrollback 10000))

;;;; TEXT EDITING ;;;;
;; better undo system
(use-package undo-fu
  :ensure t)

;; auto-pair quotes/parens/etc
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode)) ;; hook for prog-mode

;; colors for matching delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)) ;; hook in prog-mode

;; whitespace settings
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)) ;; clean up on save

;; line numbers
(use-package display-line-numbers
  :ensure nil
  :config
  (setq display-line-numbers-type 'relative) ;; set relative
  (global-display-line-numbers-mode t) ;; display globally

  ;; disable line numbers for various modes
  (dolist (mode '(vterm-mode-hook
      pdf-view-mode-hook
      image-mode-hook
      deft-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  )

;;;; PROGRAMMING LANGUAGES ;;;;
;; language server protocol
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymaps-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration))

;;;; WRITING/NOTES ;;;;
(use-package flyspell
  :config
  ;; spellcheck for various modes
  (dolist (mode '(markdown-mode-hook
                  tex-mode-hook
                  mu4e-compose-mode-hook
                  text-mode-hook))
    (add-hook mode (lambda () (flyspell-buffer))))

  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  :general
  ;; switch correct word from middle click to right click
  (general-define-key :keymaps 'flyspell-mouse-map
                      "<mouse-3>" #'ispell-word
                      "<mouse-2>" nil)
  (general-define-key :keymaps 'evil-motion-state-map
                      "zz" #'ispell-word))

;; for wrapping text
(use-package visual-fill-column
  :ensure t
  :hook ((markdown-mode . cm/writing-visual-fill)))

;; the superior markup language
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground ,(doom-color 'red)))))
  :hook
  (markdown-mode . abbrev-mode))

;; organize/search/edit md files
(use-package deft
  :ensure t
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/notes/"
    deft-extensions '("md")
    deft-use-filename-as-title t))

;; more immersive writing
(use-package writeroom-mode
  :ensure t)

;;;; EVERYTHING LATEX ;;;;
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :config
   ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
    #'TeX-revert-document-buffer))

;; compile and preview latex fragments (e.g., math)
(use-package preview
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
      (lambda () (* 1.25 (funcall (preview-scale-from-face)))))))

;; compile and preview latex fragments (e.g., math)
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
      ("<tab>" . cdlatex-tab)))

;; (use-package yasnippet
;;   :ensure t
;;   :hook ((LaTeX-mode . yas-minor-mode)
;;          (post-self-insert . my/yas-try-expanding-auto-snippets))
;;   :config
;;   (use-package warnings
;;     :config
;;     (cl-pushnew '(yasnippet backquote-change)
;;                 warning-suppress-types
;;                 :test 'equal))
;;   (setq yas-triggers-in-field t)
;;   ;; Function that tries to autoexpand YaSnippets
;;   ;; The double quoting is NOT a typo!
;;   (defun my/yas-try-expanding-auto-snippets ()
;;     (when (and (boundp 'yas-minor-mode) yas-minor-mode)
;;       (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;         (yas-expand)))))
;; (use-package cdlatex
;;   :hook ((cdlatex-tab . yas-expand)
;;          (cdlatex-tab . cdlatex-in-yas-field))
;;   :config
;;   (use-package yasnippet
;;     :bind (:map yas-keymap
;;            ("<tab>" . yas-next-field-or-cdlatex)
;;            ("TAB" . yas-next-field-or-cdlatex))
;;     :config
;;     (defun cdlatex-in-yas-field ()
;;       ;; Check if we're at the end of the Yas field
;;       (when-let* ((_ (overlayp yas--active-field-overlay))
;;                   (end (overlay-end yas--active-field-overlay)))
;;         (if (>= (point) end)
;;             ;; Call yas-next-field if cdlatex can't expand here
;;             (let ((s (thing-at-point 'sexp)))
;;               (unless (and s (assoc (substring-no-properties s)
;;                                     cdlatex-command-alist-comb))
;;                 (yas-next-field-or-maybe-expand)
;;                 t))
;;           ;; otherwise expand and jump to the correct location
;;           (let (cdlatex-tab-hook minp)
;;             (setq minp
;;                   (min (save-excursion (cdlatex-tab)
;;                                        (point))
;;                        (overlay-end yas--active-field-overlay)))
;;             (goto-char minp) t))))
;;     (defun yas-next-field-or-cdlatex nil
;;       (interactive)
;;       "Jump to the next Yas field correctly with cdlatex active."
;;       (if
;;           (or (bound-and-true-p cdlatex-mode)
;;               (bound-and-true-p org-cdlatex-mode))
;;           (cdlatex-tab)
;;         (yas-next-field-or-maybe-expand)))))

;; ;;;; PDF ;;;;
;; improved PDF reader
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  ;; hide useless evil cursor
  (add-hook 'pdf-view-mode-hook (lambda ()
                  (set (make-local-variable
                    'evil-normal-state-cursor) (list nil)))))
