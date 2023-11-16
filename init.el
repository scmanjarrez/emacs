;; Emacs 29.x fix for native comp error on start
(defvar native-comp-deferred-compilation-deny-list nil)

;; Minimize garbage collection during startup
(setq gc-cons-threshold (* 1024 1024 100))

(setq read-process-output-max (* 1024 1024))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)


;; Startup time and garbage collector threshold
(defun startup ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'startup)
;; Reduce start up time from straight due to 'find' on start
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
 '(TeX-view-program-list '(("Okular" ("okular --unique %o#src:%n%(masterdir)./%b"))))
 '(TeX-view-program-selection
   '((output-pdf "Okular")
     ((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open")))
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 3)
 '(inhibit-startup-screen t)
 '(python-shell-interpreter "python3")
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((lsp-mode) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sedate-face ((t (:foreground "#adc8ff")))))

;; Loads emacs fullscreened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No menu bar
(menu-bar-mode nil)

;; Set Hack font
(set-frame-font "HackNerdFontMono 15" nil t)

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

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

;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable minimize on Ctrl-x Ctrl-z
(global-unset-key "\C-x\C-z")

;; Keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;; Scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

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

;; Enable mouse horizontal scrolling
(setq mouse-wheel-tilt-scroll t)

;; Invert horizon scrolling direction
(setq mouse-wheel-flip-direction t)

;; Silence emacs can't guess python indent offset message
(setq python-indent-guess-indent-offset-verbose nil)

;; Show tab line
(global-tab-line-mode 1)

;; Hide new tab button
(setq tab-line-new-button-show nil)

;; Hide close tab button
(setq tab-line-close-button-show nil)

;; Set default text directorio ltr
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable parent algorithm on bidirectional
(setq bidi-inhibit-bpa t)

;; Use so-long mode on files with long lines
(global-so-long-mode 1)

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
                    :family "HackNerdFontMono" :height 0.85 :box nil)
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

;; Disable transpose chars and words keybinds
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "M-t") nil)


;; Delete non-matching text or the last character
;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0
;; https://emacs.stackexchange.com/questions/10359/delete-portion-of-isearch-string-that-does-not-match-or-last-char-if-complete-m
(defun my-isearch-delete-something ()
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))
(define-key isearch-mode-map (kbd "<backspace>") #'my-isearch-delete-something)

;; ;; Find declared functions in buffer
;; (use-package imenu
;;   :bind
;;   ("M-i" . imenu))

;; Move line/region, M-Up M-Down
(use-package move-text
  :config
  (move-text-default-bindings))

;; Indent region functions, https://stackoverflow.com/a/35183657
(defun my-indent-region-custom(spaces)
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

(defun my-untab-region ()
  (interactive)
  (if (eq major-mode 'lua-mode)
      (my-indent-region-custom (* lua-indent-level -1))
    (my-indent-region-custom -4)))

(defun my-tab-region ()
  (interactive)
  (if (eq major-mode 'lua-mode)
      (my-indent-region-custom lua-indent-level)
    (my-indent-region-custom 4)))

(defun my-tab-untab-n (n)
  (interactive "nHow many tabs?: ")
  (my-indent-region-custom n))

(global-set-key (kbd "M-<left>") 'my-untab-region)
(global-set-key (kbd "M-<right>") 'my-tab-region)

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

;; https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
(defun my-iedit-dwim (arg)
"Starts iedit but uses \\[narrow-to-defun] to limit its scope."
(interactive "P")
(if arg
    (iedit-mode)
  (save-excursion
    (save-restriction
      (widen)
      ;; this function determines the scope of `iedit-start'.
      (if iedit-mode
          (iedit-done)
        ;; `current-word' can of course be replaced by other
        ;; functions.
        (narrow-to-defun)
        (iedit-start (current-word) (point-min) (point-max)))))))

;; Intelligent variable name edit
(use-package iedit
  :config
  (global-set-key (kbd "C-;") 'my-iedit-dwim)
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
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  ;; (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :bind
  (("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)))

;; Highlight line with F9
(defun my-find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun my-toggle-highlight-line ()
  (interactive)
  (if (my-find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face '(:background "HotPink"))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
(global-set-key [f9] 'my-toggle-highlight-line)

;; Custom function to display helm mini-frame in the center of the screen
;; original code: https://github.com/emacs-helm/helm/blob/master/helm-core.el
;; original modification: https://www.reddit.com/r/emacs/comments/jj269n/display_helm_frames_in_the_center_of_emacs/
(defun my-helm-display-buffer-in-own-frame (buffer &optional resume)
  "Display Helm buffer BUFFER in a separate frame.
Function suitable for `helm-display-function',
`helm-completion-in-region-display-function' and/or
`helm-show-completion-default-display-function'.
See `helm-display-buffer-height' and `helm-display-buffer-width'
to configure frame size.
Note that this feature is available only with emacs-25+.
Note also it is not working properly in helm nested session with emacs
version < emacs-28."
  (cl-assert (and (fboundp 'window-absolute-pixel-edges)
                  (fboundp 'frame-geometry))
             nil "Helm buffer in own frame is only available starting at emacs-25+")
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* (;; Custom code, center helm frame in screen
           (parent (selected-frame))
           (parent-left (car (frame-position parent)))
           (parent-top (cdr (frame-position parent)))
           ;;
           (pos (window-absolute-pixel-position))
           (half-screen-size (/ (display-pixel-height x-display-name) 2))
           tab-bar-mode
           (new-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((parent . ,parent)
                 (width . ,helm-display-buffer-width)
                 (height . ,helm-display-buffer-height)
                 (tool-bar-lines . 0)
                 ;; Custom code, center helm frame in screen
                 (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
                 (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 2)))
                 ;;
                 (title . "Helm")
                 (undecorated . ,helm-use-undecorated-frame-option)
                 (background-color . ,(or helm-frame-background-color
                                          (face-attribute 'default :background)))
                 (foreground-color . ,(or helm-frame-foreground-color
                                          (face-attribute 'default :foreground)))
                 (alpha . ,(or helm-frame-alpha 100))
                 (font . ,(assoc-default 'font (frame-parameters)))
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visibility . ,(null helm-display-buffer-reuse-frame))
                 (minibuffer . t))))
           display-buffer-alist)
      ;; Display minibuffer above or below only in initial session,
      ;; not on a session triggered by action, this way if user have
      ;; toggled minibuffer and header-line manually she keeps this
      ;; setting in next action.
      (unless (or helm--executing-helm-action resume)
        ;; Add the hook inconditionally, if
        ;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
        ;; will have anyway no effect so no need to remove the hook.
        (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
        (with-helm-buffer
          (setq-local helm-echo-input-in-header-line
                      (not (> (cdr pos) half-screen-size)))))
      (helm-display-buffer-popup-frame buffer new-frame-alist)
      ;; When frame size have been modified manually by user restore
      ;; it to default value unless resuming or not using
      ;; `helm-display-buffer-reuse-frame'.
      ;; This have to be done AFTER raising the frame otherwise
      ;; minibuffer visibility is lost until next session.
      (unless (or resume (not helm-display-buffer-reuse-frame))
        (set-frame-size helm-popup-frame
                        helm-display-buffer-width
                        helm-display-buffer-height)))
    (helm-log-run-hook "helm-display-buffer-in-own-frame" 'helm-window-configuration-hook)))

;; Better M-x menu
(use-package helm
  :defer t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-h f" . helm-apropos)
   ("C-h v" . helm-apropos)
   ("M-i" . helm-imenu))
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-boring-buffer-regexp-list '("\\*.*"))
  (helm-ff-skip-boring-files t)
  (helm-boring-file-regexp-list '("__pycache__"))
  (helm-display-function 'my-helm-display-buffer-in-own-frame)
  (helm-display-buffer-reuse-frame t)
  (helm-use-undecorated-frame-option t))

;; Helm git
(use-package helm-ls-git
  :defer t
  :bind
  ("C-x g" . helm-ls-git))

;; Better bindings information
(use-package helm-descbinds
  :defer t
  :bind
  ("C-h b" . helm-descbinds))

;; Show current/total numbers in search
(use-package anzu
  :init
  (global-anzu-mode 1)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :custom
  (anzu-cons-mode-line-p nil))

;; Nerd icons package
(use-package nerd-icons)

;; Better powerline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 0)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-env-version nil)
  (inhibit-compacting-font-caches t)
  (lsp-modeline-diagnostics-enable nil)
  (nerd-icons-scale-factor 1.3)
  (nerd-icons-default-adjust 0.0)
  (doom-modeline-major-mode-color-icon nil)
  :config
  (set-face-attribute 'mode-line nil :family "HackNerdFontMono" :height 95)
  (set-face-attribute 'mode-line-inactive nil :family "HackNerdFontMono" :height 95))

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
  :custom
  (vr/command-custom (format "python3 %s" (expand-file-name "regexp.py" (file-name-directory load-file-name))))
  (vr/engine 'custom)
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)
   ("C-r" . vr/isearch-backward)
   ("C-s" . vr/isearch-forward)))

;; Togle case sensitiveness for vr/isearch
(defun toggle-vr-case-insensitive ()
  "Toggle case-insensitive search for visual-regexp."
  (interactive)
  (setq advice-list (list))
  (advice-mapc (lambda (advice _props) (push advice advice-list)) 'vr--isearch)
  (message "advices %d" (length advice-list))
  (if (= (length advice-list) 0)
      (message "Enabling case-insensitive search"
               (defadvice vr--isearch (around add-case-insensitive (forward string &optional bound noerror count) activate)
                 (setq string (concat "(?i)" string))
                 ad-do-it))
    (message "Disabling case-insensitive search"
             (advice-mapc (lambda (advice _props) (advice-remove 'vr--isearch advice)) 'vr--isearch)))
  )
(global-set-key (kbd "C-c t") 'toggle-vr-case-insensitive)


;; Toggle comments with M-;
(defun my-toggle-comment ()
  "Comments or uncomments the region/line."
  (interactive)
  (let (beg line-end)
    (if (region-active-p)
        (setq beg (region-beginning) line-end (region-end))
      (setq beg (line-beginning-position) line-end (line-end-position)))
    (comment-or-uncomment-region beg line-end)
    (next-line)))
(global-set-key (kbd "M-;") 'my-toggle-comment)

;; ;; LSP mode
;; (use-package lsp-mode
;;   :custom
;;   (lsp-keymap-prefix "C-:")
;;   (lsp-use-plists t)
;;   (lsp-pylsp-plugins-pydocstyle-enabled nil)
;;   (lsp-pylsp-plugins-jedi-hover-enabled nil)
;;   (lsp-pylsp-plugins-mccabe-enabled nil)
;;   (lsp-ui-doc-show-with-mouse nil)
;;   (lsp-enable-snippet t)
;;   (lsp-lua-completion-call-snippet "Replace")
;;   (lsp-clangd-binary-path "~/.emacs.d/.cache/lsp/c-language-server/bin/clangd")
;;   (lsp-clients-texlab-executable "~/.emacs.d/.cache/lsp/latex-language-server/texlab")
;;   (lsp-clients-lua-language-server-bin "~/.emacs.d/.cache/lsp/lua-language-server/extension/server/bin/lua-language-server")
;;   (lsp-clients-lua-language-server-main-location (concat (getenv "HOME") "/.emacs.d/.cache/lsp/lua-language-server/extension/server/bin/main.lua"))
;;   (lsp-enable-file-watchers nil)
;;   ;; (lsp-log-io t)
;;   :hook
;;   (sh-mode . lsp-deferred)
;;   (python-mode . lsp-deferred)
;;   ;; (go-mode . lsp-deferred)
;;   (LaTeX-mode . lsp-deferred)
;;   (lua-mode . lsp-deferred)
;;   ;; (terraform-mode . lsp-deferred)
;;   (dockerfile-mode . lsp-deferred)
;;   (c-mode . lsp-deferred)
;;   (c++-mode . lsp-deferred)
;;   :bind
;;   ("<C-tab>" . company-complete))

;; ;; LSP dependency
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; ;; Buffer completion
;; (use-package company
;;   :custom
;;   (company-minimum-prefix-length 2)
;;   (company-selection-wrap-around t)
;;   (company-tooltip-align-annotations t)
;;   (global-company-mode t)
;;   :bind
;;   (("<C-iso-lefttab>" . company-files)
;;    :map company-active-map
;;    ("<tab>" . company-complete-selection)))

;; Code snippets
(use-package yasnippet
  :defer t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-<tab>" . yas-expand)))

;; Code snippets templates
(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))

(add-hook 'prog-mode-hook #'yas-minor-mode)

;; lsp-bridge to replace lsp-mode
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :custom
  (lsp-bridge-python-command "~/lsp-bridge/bin/python")
  (lsp-bridge-user-langserver-dir "~/lsp-bridge/configs-server")
  (lsp-bridge-user-multiserver-dir "~/lsp-bridge/configs-multiserver")
  :init
  (global-lsp-bridge-mode))

;; Use grip from emacs
(use-package grip-mode
  :defer t
  :custom
  (grip-update-after-change nil))

;; ;; Major mode for golang
;; (use-package go-mode
;;   :defer t)

;; Major mode for cmake
(use-package cmake-mode
  :defer t)

;; Major mode for C. Generate compile_command.json with cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1
(use-package cc-mode
  :defer t
  :bind
  (:map c-mode-map
        ("C-d" . nil)))

;; Major mode for lua
(use-package lua-mode
  :mode "\\.\\(lua\\|nse\\)\\'"
  :defer t)

;; Major mode for json
(use-package json-mode
  :defer t
  :custom
  (js-indent-level 2))

;; FIX: Only workins in emacs >27.2
;; Need to delete hierarchy.elc
;; Minor mode for json
(use-package json-navigator
  :ensure hierarchy
  :bind
  ("C-c g" . json-navigator-navigate-region))

;; Major mode for yaml
(use-package yaml-mode
  :defer t)

;; Minor mode for python/yaml
(use-package indent-tools
  :defer t
  :bind
  ("C-c f" . indent-tools-hydra/body)
  ("C-c F" . yafolding-toggle-element))

;; ;; Major mode for qml (pyqt)
;; (use-package qml-mode
;;   :defer t)

;; Major mode for Dockerfiles
(use-package dockerfile-mode
  :defer t)

;; Major mode for Docker compose
(use-package docker-compose-mode
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
(defun my-duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-d") 'my-duplicate-line)

;; Enable hideshow minor mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "M-[") 'hs-toggle-hiding)

;; ;; Github copilot
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :hook
;;   (prog-mode . copilot-mode)
;;   :bind
;;   ("M-q" . 'copilot-accept-completion)
;;   ("M-e" . 'copilot-accept-completion-by-word))

;; Quick swap between windows configurations, https://emacs.stackexchange.com/a/2714
(defvar winstack-stack '()
  "A Stack holding window configurations.
Use `my-winstack-push' and
`my-winstack-pop' to modify it.")

(defun my-winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (cl-first winstack-stack))
           (compare-window-configurations (cl-first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string
                                       (length (window-list (selected-frame)))) " frame config")))))

(defun my-winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (cl-first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))

(global-set-key (kbd "<f7>") 'my-winstack-push)
(global-set-key (kbd "<S-f7>") 'window-configuration-to-register)
(global-set-key (kbd "<f8>") 'my-winstack-pop)
(global-set-key (kbd "<S-f8>") 'jump-to-register)

(defun my-rename-window(name)
  (interactive "sNew name?: ")
  (setq-default frame-title-format (format "%s" name)))

;; LaTeX package
(use-package auctex
  :defer t)

(use-package flyspell
  :hook
  (LaTeX-mode . flyspell-mode)
  (flyspell-mode . flyspell-buffer))

;; ;; Terraform package
;; (use-package terraform-mode
;;   :defer t)

;; ;; Ansible package
;; (use-package ansible
;;   :defer t)

;; ;; Jinja2 package
;; (use-package jinja2-mode
;;   :defer t)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold (round (* gc-cons-threshold 0.3)))
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))
