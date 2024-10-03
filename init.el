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
 '(font-latex-sedate-face ((t (:foreground "#adc8ff"))))
 '(rainbow-delimiters-base-error-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#99FF99")))) ; Light Green
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#61C5FF")))) ; Light Blue
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#FFC64D")))) ; Light Yellow
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#12D912")))) ; Medium Green
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#0AA5FF")))) ; Medium Blue
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#F5A700")))) ; Medium Yellow
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#2AA72A")))) ; Dark Green
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#007EC7")))) ; Dark Blue
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#CC8B00")))) ; Dark Yellow
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))

;; Loads emacs fullscreened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No menu bar
(menu-bar-mode nil)

;; Set Hack font
;; (set-frame-font "HackNerdFontMono 14" nil t)
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

;; Fix delete-indentation not working
(global-set-key (kbd "M-<dead-circumflex>") 'delete-indentation)


;; Delete non-matching text or the last character
;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0
;; https://emacs.stackexchange.com/questions/10359/delete-portion-of-isearch-string-that-does-not-match-or-last-char-if-complete-m
(defun my/isearch-delete-something ()
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
(define-key isearch-mode-map (kbd "<backspace>") #'my/isearch-delete-something)

;; ;; Find declared functions in buffer
;; (use-package imenu
;;   :bind
;;   ("M-i" . imenu))

;; Move line/region, M-Up M-Down
(use-package move-text
  :config
  (move-text-default-bindings))

;; Indent region functions, https://stackoverflow.com/a/35183657
(defun my/indent-region-custom(spaces)
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

(defun my/untab-region ()
  (interactive)
  (if (eq major-mode 'lua-mode)
      (my/indent-region-custom (* lua-indent-level -1))
    (my/indent-region-custom -4)))

(defun my/tab-region ()
  (interactive)
  (if (eq major-mode 'lua-mode)
      (my/indent-region-custom lua-indent-level)
    (my/indent-region-custom 4)))

(defun my/tab-untab-n (n)
  (interactive "nHow many tabs?: ")
  (my/indent-region-custom n))

(global-set-key (kbd "M-<left>") 'my/untab-region)
(global-set-key (kbd "M-<right>") 'my/tab-region)

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
(defun my/iedit-dwim (arg)
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
  (global-set-key (kbd "C-;") 'my/iedit-dwim)
  :bind
  ("C-c e" . iedit-mode))

;; Show parentheses/brackets/braces colored
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

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
(defun my/find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun my/toggle-highlight-line ()
  (interactive)
  (if (my/find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face '(:background "HotPink"))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
(global-set-key [f9] 'my/toggle-highlight-line)

;; Custom function to display helm mini-frame in the center of the screen
;; original code: https://github.com/emacs-helm/helm/blob/master/helm-core.el
;; original modification: https://www.reddit.com/r/emacs/comments/jj269n/display_helm_frames_in_the_center_of_emacs/
(defun my/helm-display-buffer-in-own-frame (buffer &optional resume)
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
  (helm-boring-buffer-regexp-list '("\\*.*" "markdown-code-fontification.*"))
  (helm-ff-skip-boring-files t)
  (helm-boring-file-regexp-list '("__pycache__"))
  (helm-display-function 'my/helm-display-buffer-in-own-frame)
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
  (vr/command-python (format "python3 %s" (expand-file-name "regexp.py" (file-name-directory load-file-name))))
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
(defun my/toggle-comment ()
  "Comments or uncomments the region/line."
  (interactive)
  (let (beg line-end)
    (if (region-active-p)
        (setq beg (region-beginning) line-end (region-end))
      (setq beg (line-beginning-position) line-end (line-end-position)))
    (comment-or-uncomment-region beg line-end)
    (next-line)))
(global-set-key (kbd "M-;") 'my/toggle-comment)

;; LSP mode
(use-package lsp-mode
  :preface
  ;; documentation https://github.com/blahgeek/emacs-lsp-booster
  ;; and https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgc88562e
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(basic)))
  (setq lsp-use-plists t)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :hook
  (python-mode . lsp-deferred)
  (sh-mode . lsp-deferred)
  ;; (go-mode . lsp-deferred)
  ;; (LaTeX-mode . lsp-deferred)
  ;; (lua-mode . lsp-deferred)
  ;; (terraform-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  ;; (rust-mode . lsp-deferred)
  ;; (c-mode . lsp-deferred)
  ;; (c++-mode . lsp-deferred)
  (lsp-completion-mode . my/lsp-mode-setup-completion) ;; instructions from corfu install, this makes only trigger completions if match the start of the candidate
  :config
  (add-to-list 'exec-path (expand-file-name "~/.config/lsp-bridge/bin"))  ;; allow loading pylsp in custom path
  :custom
  (lsp-keymap-prefix "C-c l")
  ;; (lsp-pylsp-plugins-black-enabled t)
  (lsp-pylsp-plugins-ruff-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-lua-completion-call-snippet "Replace")
  ;; (lsp-clangd-binary-path "~/.emacs.d/.cache/lsp/c-language-server/bin/clangd")
  ;; (lsp-clients-texlab-executable "~/.emacs.d/.cache/lsp/latex-language-server/texlab")
  ;; (lsp-clients-lua-language-server-bin "~/.emacs.d/.cache/lsp/lua-language-server/extension/server/bin/lua-language-server")
  ;; (lsp-clients-lua-language-server-main-location (concat (getenv "HOME") "/.emacs.d/.cache/lsp/lua-language-server/extension/server/bin/main.lua"))
  :commands
  (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t) ; show diagnostics messages in sideline, eg type errors.
  (lsp-ui-sideline-show-hover t) ; show hover messages in sideline. Often type info.
  (lsp-ui-sideline-show-code-actions t) ; show code actions in sideline. Example??
  (lsp-ui-sideline-update-mode "point") ; When set to 'line' the information will be updated when
  ;; user changes current line otherwise the information will be updated when user changes current point.
  (lsp-ui-sideline-delay 0.02) ; seconds to wait before showing sideline
  (lsp-ui-doc-enable t) ; docstrings on hover.
  (lsp-ui-peek-enable t) ; peek at definition or matches, instead of a big context switch
  (lsp-ui-peek-always-show t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))


(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (completion-cycle t)
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (completion-styles '(basic))
  (corfu-quit-no-match 'separator)
  (corfu-echo-documentation nil)
  (corfu-popupinfo-mode t)
  (corfu-echo-mode nil)
  (corfu-history-mode t)
  :bind
  ("M-q" . corfu-quick-complete)
  ("C-q" . corfu-quick-insert))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  (kind-icon-default-style `(:padding -1 :stroke 0 :margin 0 :radius 0 :scale 1.0 :height 0.55)) ; make sure icons fit with scaled text
  (svg-lib-icons-dir (expand-file-name "svg-lib/cache/" user-emacs-directory)) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

;; Code snippets
(use-package yasnippet
  :defer t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-<tab>" . yas-expand))
  :hook
  (prog-mode . yas-minor-mode))

;; Code snippets templates
(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))

(use-package helm-c-yasnippet
  :defer t
  :custom
  (helm-yas-space-match-any-greedy t)
  :bind
  ("M-s" . helm-yas-complete))

;; ;; lsp-bridge to replace lsp-mode
;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :custom
;;   (lsp-bridge-python-command "~/.config/lsp-bridge/bin/python")
;;   (lsp-bridge-user-langserver-dir "~/.config/lsp-bridge/configs-server")
;;   (lsp-bridge-user-multiserver-dir "~/.config/lsp-bridge/configs-multiserver")
;;   ;; (lsp-bridge-python-lsp-server "pylsp")
;;   ;; (lsp-bridge-python-amulti-lsp-server "basedpyright_ruff")
;;   (lsp-bridge-enable-hover-diagnostic t)
;;   ;; (lsp-bridge-enable-debug t)
;;   :bind
;;   ("C-: r" . lsp-bridge-rename)
;;   ("M-." . lsp-bridge-find-def)
;;   ("M-," . lsp-bridge-find-def-return)
;;   :init
;;   (global-lsp-bridge-mode))

;; Use grip from emacs
(use-package grip-mode
  :defer t
  :bind
  ("C-c l g" . grip-mode)
  :custom
  (grip-update-after-change nil))

;; Major mode for golang
(use-package go-mode
  :defer t)

;; Major mode for cmake
(use-package cmake-mode
  :defer t)

;; Major mode for C. Generate compile_command.json with cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1
(use-package cc-mode
  :defer t
  :bind
  (:map c-mode-map
        ("C-d" . nil))
  (:map c++-mode-map
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
(defun my/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-d") 'my/duplicate-line)

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
Use `my/winstack-push' and
`my/winstack-pop' to modify it.")

(defun my/winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (cl-first winstack-stack))
           (compare-window-configurations (cl-first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string
                                       (length (window-list (selected-frame)))) " frame config")))))

(defun my/winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (cl-first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))

(global-set-key (kbd "<f7>") 'my/winstack-push)
(global-set-key (kbd "<S-f7>") 'window-configuration-to-register)
(global-set-key (kbd "<f8>") 'my/winstack-pop)
(global-set-key (kbd "<S-f8>") 'jump-to-register)

(defun my/rename-window(name)
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
