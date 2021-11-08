;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(let ((file-name-handler-alist nil))
  ;; Startup time and garbage collector threshold
  (defun startup ()
    (message "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)
    (setq gc-cons-threshold (expt 2 26)))
  (add-hook 'emacs-startup-hook #'startup)

  ;; Install straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Install use-package
  (straight-use-package 'use-package)

  ;; Configure use-package to use straight.el by default
  (use-package straight
    :custom (straight-use-package-by-default t))

  ;; Disable package.el in favor of straight.el
  (setq package-enable-at-startup nil)

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(TeX-expand-list
     '(("%(masterdir)"
        (lambda nil
          (file-truename
           (TeX-master-directory))))))
   '(TeX-source-correlate-method 'synctex)
   '(TeX-source-correlate-mode t)
   '(TeX-source-correlate-start-server t)
   '(TeX-view-program-list
     '(("Okular"
        ("okular --unique %o#src:%n%(masterdir)./%b"))))
   '(TeX-view-program-selection
     '((output-pdf "Okular")
       ((output-dvi has-no-display-manager)
        "dvi2tty")
       ((output-dvi style-pstricks)
        "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "Evince")
       (output-html "xdg-open")))
   '(inhibit-startup-screen t)
   '(python-shell-interpreter "python3"))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )

  ;; Loads emacs fullscreened
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; No menu bar
  (menu-bar-mode nil)

  ;; Set Hack font
  (set-frame-font "Hack" nil t)

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Change yes/no dialog to y/n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Disable line wrapping
  (setq-default truncate-lines t)

  ;; Force disable line wrapping even in horizontal splits
  (setq truncate-partial-width-windows t)

  ;; Display line-max-length bar
  (global-display-fill-column-indicator-mode t)

  ;; Change line-max-length bar length, all modes, 86 characters
  (setq-default fill-column 86)

  ;; Change line-max-length bar length, python 79 characters
  (add-hook 'python-mode-hook (lambda ()
                                (setq fill-column 79)))

  ;; Change color of line-max-length bar
  (set-face-attribute 'fill-column-indicator nil :foreground "#424270")

  ;; Change window separator line color
  (face-spec-set 'vertical-border '((t (:foreground "#af5fff"))))

  ;; Show column number
  (setq column-number-mode t)

  ;; Don't show line number if buffer is too big; value in bytes
  (setq line-number-display-limit (* 1024 1024 64))

  ;; Highlight trailing whitespaces in lines
  (setq-default show-trailing-whitespace t)

  ;; Highlight current line
  (global-hl-line-mode t)

  ;; Delete selected region on write
  (delete-selection-mode 1)

  ;; Load newer .elc files
  (setq load-prefer-newer t)

  ;; Enable paren-mode
  (show-paren-mode t)

  ;; Highlight bracket if visible or expression
  (setq show-paren-style 'mixed)

  ;; Disable menubar
  (menu-bar-mode -1)

  ;; Disable toolbar
  (tool-bar-mode -1)

  ;; Disable scrolbar
  (scroll-bar-mode -1)

  ;; Ignore case when completing
  (setq completion-ignore-case t)

  ;; Ignore filenames case completion
  (setq read-file-name-completion-ignore-case t)

  ;; Tab to 4 spaces, global
  (setq-default tab-width 4)

  ;; Tab to 4 spaces, local
  (setq tab-width 4)

  ;; always use spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Disable creation of "backup~" files
  (setq make-backup-files nil)

  ;; Disable creation of "#autosave#" files
  (setq auto-save-default nil)

  ;; Keep cursor at same position on scroll
  (setq scroll-conservatively 101)

  ;; Short scroll 1 line at a time, shift + scroll 5 lines
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

  ;; Long scroll use the same amount as short scroll
  ;; (setq mouse-wheel-progressive-speed nil)

  ;; Silence emacs can't guess python indent offset message
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Show tab line
  (global-tab-line-mode 1)

  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  ;; Close the selected tab, https://andreyorst.gitlab.io/posts/2020-05-07-making-emacs-tabs-work-like-in-atom/
  ;; If tab is presented in another window, close the tab by using `bury-buffer` function.
  ;; If tab is unique to all existing windows, kill the buffer with `kill-buffer` function.
  ;; If no tabs left in the window, it is deleted with `delete-window` function.
  (defun tab-line-close-tab (&optional e)
    (interactive "e")
    (let* ((posnp (event-start e))
           (window (posn-window posnp))
           (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
      (with-selected-window window
        (let ((tab-list (tab-line-tabs-window-buffers))
              (buffer-list (flatten-list
                            (seq-reduce (lambda (list window)
                                          (select-window window t)
                                          (cons (tab-line-tabs-window-buffers) list))
                                        (window-list) nil))))
          (select-window window)
          (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
              (progn
                (if (eq buffer (current-buffer))
                    (bury-buffer)
                  (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                  (set-window-next-buffers window (delq buffer (window-next-buffers))))
                (unless (cdr tab-list)
                  (ignore-errors (delete-window window))))
            (and (kill-buffer buffer)
                 (unless (cdr tab-list)
                   (ignore-errors (delete-window window)))))))
      (force-mode-line-update)))

  (set-face-attribute 'tab-line nil ;; background behind tabs
                      :background "#292a44"
                      :distant-foreground "#666699" ;; current window, inactive text color
                      :family "Hack" :height 0.85 :box nil)
  (set-face-attribute 'tab-line-tab nil ;; active tab in another window
                      :foreground "#663399" :background "#383a62" :box nil)
  (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
                      :background "#663399" :foreground "#f1eff8" :box nil)
  (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
                      :background "#292a44" :foreground "#53495d" :box nil)
  (set-face-attribute 'tab-line-highlight nil ;; mouseover
                      :background "#ff5faf" :foreground "#663399")

  ;; Keybinds to navigate between paragraphs
  (global-set-key (kbd "C-S-n") 'forward-paragraph)
  (global-set-key (kbd "C-S-p") 'backward-paragraph)

  ;; Smart completion with find file
  (use-package ido
	:custom
	(ido-mode 1)
	(ido-everywhere 1)
	:config
	(setq ido-ignore-buffers '("\\` " "^\*")))

  ;; Try to use ido everywhere
  (use-package ido-completing-read+
	:requires ido
	:custom
	(ido-ubiquitous-mode 1))

  ;; Find declared functions in buffer
  (use-package imenu
	:bind
    ("M-i" . imenu))

  ;; Move line/region, M-Up M-Down
  (use-package move-text
	:config
	(move-text-default-bindings))

  ;; Indent region functions, https://stackoverflow.com/a/35183657
  (defun indent-region-custom(spaces)
    (progn
      (setq region-start (line-beginning-position)) ; default to line-start and line-end of current line
      (setq region-end (line-end-position))

      ;; if there's a selection, use that instead of the current line
      (when (use-region-p)
        (setq region-start (region-beginning))
        (setq region-end (region-end))
        )

      (save-excursion ; restore the position afterwards
        (goto-char region-start) ; go to the line-start of region
        (setq line-start (line-beginning-position)) ; save the line-start of the line
        (goto-char region-end) ; go to the line-end of region
        (setq line-end (line-end-position)) ; save the line-end of the line

        (indent-rigidly line-start line-end spaces) ; indent between line-start and line-end
        (setq deactivate-mark nil) ; restore the selected region
        )
      )
    )

  (defun untab-region ()
    (interactive)
    (if (eq major-mode 'lua-mode)
        (indent-region-custom (* lua-indent-level -1))
      (indent-region-custom -4)))

  (defun tab-region ()
    (interactive)
    (if (eq major-mode 'lua-mode)
        (indent-region-custom lua-indent-level)
      (indent-region-custom 4)))

  (defun tab-untab-n (n)
    (interactive "nHow many tabs?: ")
    (indent-region-custom n))

  (global-set-key (kbd "M-<left>") 'untab-region)
  (global-set-key (kbd "M-<right>") 'tab-region)

  ;; Use S-arrow to move between panes
  (use-package windmove
	:config
	(windmove-default-keybindings))

  ;; Auto insert parens
  (use-package smartparens
	:custom
	(smartparens-global-mode t)
	:config
	(sp-pair "(" ")" :wrap "C-(")
	(sp-pair "[" "]" :wrap "C-M-[")
	(sp-pair "'" "'" :wrap "C-'")
	(sp-pair "\"" "\"" :wrap "C-\"")
	(sp-pair "{" "}" :wrap "C-{"))

  ;; Intelligent variable name edit
  (use-package iedit
	:bind
	("C-c e" . iedit-mode))

  ;; Show parentheses/brackets/braces colored
  (use-package rainbow-delimiters
	:hook
	(prog-mode . rainbow-delimiters-mode)
	:config
	;; makes rainbow-mode colors more contrasted
	(require 'cl-lib)
	(require 'color)
	(cl-loop
	 for index from 1 to rainbow-delimiters-max-face-count
	 do
	 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	   (cl-callf color-saturate-name (face-foreground face) 80)))
	)

  ;; Rebecca theme
  (use-package rebecca-theme
	:config
	(load-theme 'rebecca t))

  ;; Show undo as a tree
  (use-package undo-tree
	:custom
	(global-undo-tree-mode 1)
	:bind
	(("C-z" . undo-tree-undo)
	 ("C-S-z" . undo-tree-redo)))

  ;; Highlight line with F9
  (defun find-overlays-specifying (prop pos)
    (let ((overlays (overlays-at pos))
          found)
      (while overlays
        (let ((overlay (car overlays)))
          (if (overlay-get overlay prop)
              (setq found (cons overlay found))))
        (setq overlays (cdr overlays)))
      found))

  (defun toggle-highlight-line ()
    (interactive)
    (if (find-overlays-specifying
         'line-highlight-overlay-marker
         (line-beginning-position))
        (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
      (let ((overlay-highlight (make-overlay
                                (line-beginning-position)
                                (+ 1 (line-end-position)))))
        (overlay-put overlay-highlight 'face '(:background "HotPink"))
        (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
  (global-set-key [f9] 'toggle-highlight-line)

  ;; Better M-x menu
  (use-package smex
	:init
	(smex-initialize)
	:bind
    (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

  ;; Show current/total numbers in search
  (use-package anzu
	:init
	(global-anzu-mode 1)
	:bind
    (([remap query-replace] . anzu-query-replace)
	 ([remap query-replace-regexp] . anzu-query-replace-regexp))
	:custom
	(anzu-cons-mode-line-p nil))

  ;; Better powerline
  (use-package doom-modeline
	:init
	(doom-modeline-mode 1)
	:custom
    (doom-modeline-height 20)
    (doom-modeline-bar-width 0)
    (doom-modeline-enable-word-count t)
    (doom-modeline-buffer-file-name-style 'truncate-upto-root)
    (doom-modeline-env-version nil)
	(inhibit-compacting-font-caches t)
    (lsp-modeline-diagnostics-enable nil)
    (all-the-icons-scale-factor 1)
    (all-the-icons-default-adjust 0.2)
    (doom-modeline-major-mode-color-icon nil)
    :config
    (set-face-attribute 'mode-line nil :family "Hack" :height 93)
    (set-face-attribute 'mode-line-inactive nil :family "Hack" :height 93))

  ;; Performance package
  (use-package esup
	:defer t
	:custom
	(esup-depth 0))

  ;; Multiple cursors
  (use-package multiple-cursors
    :defer t
    :bind
    ("C-c c" . mc/edit-lines))

  ;; Python regexps
  (use-package visual-regexp
    :defer t)

  (use-package visual-regexp-steroids
    :demand
    :bind
    (("C-c r" . vr/replace)
     ("C-c q" . vr/query-replace)
     ("C-c m" . vr/mc-mark)
     ("C-r" . vr/isearch-backward)
     ("C-s" . vr/isearch-forward))
    :custom
    (isearch-message-prefix-add "(?i) ")
    :config
    ;; Make vr--isearch always case insensitive
    (defadvice vr--isearch (around add-case-insensitive (forward string &optional bound noerror count) activate)
      (setq string (concat "(?i)" string))
      ad-do-it))

  ;; Toggle comments with M-;
  (defun toggle-comment ()
    "Comments or uncomments the region/line."
    (interactive)
    (let (beg line-end)
      (if (region-active-p)
          (setq beg (region-beginning) line-end (region-end))
        (setq beg (line-beginning-position) line-end (line-end-position)))
      (comment-or-uncomment-region beg line-end)
      (next-line)))
  (global-set-key (kbd "M-;") 'toggle-comment)

  ;; LSP mode
  (use-package lsp-mode
	:custom
	(lsp-pylsp-plugins-pydocstyle-enabled nil)
	(lsp-pylsp-plugins-mccabe-enabled nil)
    (lsp-enable-snippet t)
    (lsp-lua-completion-call-snippet "Replace")
	:hook
	(sh-mode . lsp-deferred)
	(python-mode . lsp-deferred)
	(go-mode . lsp-deferred)
	(LaTeX-mode . lsp-deferred)
    (lua-mode . lsp-deferred))

  ;; LSP dependency
  (use-package lsp-ui
    :commands lsp-ui-mode)

  ;; Buffer completion
  (use-package company
	:custom
	(company-minimum-prefix-length 2)
	(company-selection-wrap-around t)
	(company-tooltip-align-annotations t)
    (global-company-mode t)
	:bind
	(:map company-active-map
		  ("<tab>" . company-complete-selection)
          ("TAB" . company-complete-selection)))

  ;; Code snippets
  (use-package yasnippet
    :bind
    (:map yas-minor-mode-map
          ("<tab>" . nil)
          ("TAB" . nil)
          ("<backtab>" . yas-expand))
    :config
    (yas-global-mode t))

  ;; Code snippets templates
  (use-package yasnippet-snippets)

  ;; Major mode for golang
  (use-package go-mode
    :defer t)

  ;; Major mode for lua
  (use-package lua-mode
    :mode "\\.\\(lua\\|nse\\)\\'"
    :defer t)

  ;; Major mode for json
  (use-package json-mode
    :defer t)

  ;; Better emacs sessions
  (use-package desktop+)

  ;; Change colors of whitespace-mode to be compatible with rebecca theme
  (use-package whitespace
    :defer t
    :config
    (set-face-attribute 'whitespace-space nil :background nil :foreground "#af5fff")
    (set-face-attribute 'whitespace-space-before-tab nil :background "#424270" :foreground "#af5fff")
    (set-face-attribute 'whitespace-newline nil :background nil :foreground "#8700d7")
    (set-face-attribute 'whitespace-empty nil :background "#ff79c6" :foreground nil)
    )

  ;; Duplicate lines with C-d
  (defun duplicate-line()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    )
  (global-set-key (kbd "C-d") 'duplicate-line)

  ;; Enable hideshow minor mode
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (global-set-key (kbd "M-[") 'hs-toggle-hiding)

  ;; Quick swap between windows configurations, https://emacs.stackexchange.com/a/2714
  (defvar winstack-stack '()
    "A Stack holding window configurations.
  Use `winstack-push' and
  `winstack-pop' to modify it.")

  (defun winstack-push()
    "Push the current window configuration onto `winstack-stack'."
    (interactive)
    (if (and (window-configuration-p (first winstack-stack))
             (compare-window-configurations (first winstack-stack) (current-window-configuration)))
        (message "Current config already pushed")
      (progn (push (current-window-configuration) winstack-stack)
             (message (concat "pushed " (number-to-string
                                         (length (window-list (selected-frame)))) " frame config")))))

  (defun winstack-pop()
    "Pop the last window configuration off `winstack-stack' and apply it."
    (interactive)
    (if (first winstack-stack)
        (progn (set-window-configuration (pop winstack-stack))
               (message "popped"))
      (message "End of window stack")))

  (global-set-key (kbd "<f7>") 'winstack-push)
  (global-set-key (kbd "<S-f7>") 'window-configuration-to-register)
  (global-set-key (kbd "<f8>") 'winstack-pop)
  (global-set-key (kbd "<S-f8>") 'jump-to-register)

  ;; LaTeX package
  (use-package auctex
    :defer t)
  )
