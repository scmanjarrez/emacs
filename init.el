(defvar my-start-time (current-time)
  "Start time")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-pdf "Evince")
     (output-html "xdg-open")))
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-django elpy-module-sane-defaults))
 '(elpy-rpc-python-command "python3")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-display-limit 67108864)
 '(lsp-file-watch-ignored
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]\\.py$" "[/\\\\]\\.pyc$"))
 '(package-selected-packages
   '(company-box company-try-hard company-web company-quickhelp company-statistics multiple-cursors visual-regexp-steroids yaml-mode company-auctex company-lsp lsp-ui lsp-mode move-text go-mode lua-mode auto-compile all-the-icons anzu undo-tree spinner latex-preview-pane latex-math-preview latex-extra flycheck-cstyle elpy auto-complete-c-headers auto-complete-auctex ac-math))
 '(python-shell-interpreter "python3")
 '(show-trailing-whitespace t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Loads emacs fullscreened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(global-set-key (kbd "M-:") nil)
(global-set-key (kbd "C-t") nil)

;; ignore case when completing, filenames too
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; remove cl warning
(setq byte-compile-warnings '(cl-functions))

(setq initial-scratch-message ";; Draft file.\n")

;; Set some variables in order to make below functions work
(setq desktop-dirname             (getenv "PWD")
      desktop-base-file-name      ".emacs.desktop"
      desktop-base-lock-name      ".lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$"
      desktop-load-locked-desktop nil)

;; Look for .emacs.desktop file
(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; Use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; Use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
  Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files


(defun wrap-text (b e txt)
  "simple wrapper"
  (interactive "r\nMEnter text to wrap with: ")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert txt)
    (insert "(")
    (goto-char (point-max))
    (insert ")")))

(global-set-key (kbd "C-x M-w") 'wrap-text)

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs.d/init.el"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))


(set-frame-font "Hack" nil t)
(set-face-attribute 'default nil :height 100)

;; Delete trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

;; disable python guest offset warning
(setq python-indent-guess-indent-offset-verbose nil)

(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Don't show line number if buffer is too big; value in bytes
(customize-set-variable 'line-number-display-limit (* 1024 1024 64))

;; Show buffer size in the mode line
(size-indication-mode 1)

;; Highlight trailing whitespaces in lines
(customize-set-variable 'show-trailing-whitespace t)

;; No menu bar
(menu-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Show current line and column in the mode line
(line-number-mode t)
(column-number-mode t)

;; Delete selected region on write
(delete-selection-mode 1)

;; (setq load-prefer-newer t)

(package-initialize)

;; (setq use-package-verbose t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-ignore-buffers '("\\` " "^\*")))


(use-package move-text
  :config
  (move-text-default-bindings))

;; https://stackoverflow.com/a/35183657 thanks :)
(defun indent-region-custom(numSpaces)
  (progn
                                        ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

                                        ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )
    )
  )

;; https://emacs.stackexchange.com/a/16407

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))


(defun untab-region (N)
  (interactive "p")
  (indent-region-custom -4)
  )

(defun tab-region (N)
  (interactive "p")
  (indent-region-custom 4)
  )

(global-set-key (kbd "M-<left>") 'untab-region)
(global-set-key (kbd "M-<right>") 'tab-region)

(use-package paren
  :init
  (progn
    (show-paren-mode 1)
    (setq show-paren-style 'mixed)))

(use-package tool-bar
  :defer t
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :config (scroll-bar-mode -1))

;; use Shift+arrow_keys to move cursor around split panes
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-M-[")
  (sp-pair "'" "'" :wrap "C-'")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "{" "}" :wrap "C-{"))

                                        ;(use-package yasnippet
                                        ;             :bind
                                        ;             (:map yas-minor-mode-map
                                        ;                   ("C-c k" . yas-expand))
                                        ;                   ([(tab)] . nil)
                                        ;                   ("TAB" . nil))
                                        ;             :config
                                        ;             (yas-global-mode 1))


(use-package iedit
  :bind ("C-c ;" . iedit-mode))

;;turn on Semantic
                                        ;(use-package semantic
                                        ;             :config
                                        ;             (add-hook 'c-mode-hook (lambda () (progn
                                        ;                                                 (semantic-mode 1)
                                        ;                                                 (global-semantic-idle-scheduler-mode 1))))
                                        ;             (defun my:add-semantic-to-autocomplete()
                                        ;               (add-to-list 'ac-sources 'ac-source-semantic))
                                        ;             (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete))
                                        ;

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ciao-mode-hook 'rainbow-delimiters-mode))

(if (display-graphic-p)
    (load-theme 'zenburn t)  ;;enable zenburn-theme only GUI
  (set-face-attribute 'region nil :background "#e3cf71" :foreground "#000") ;; change bg color terminal
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])

  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down]))

;; makes rainbow-mode colors more contrasted
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 80)))

(use-package flycheck
  :config
  (add-hook 'c-mode-hook 'global-flycheck-mode)
  (progn
    (require 'flycheck-cstyle)
    (flycheck-cstyle-setup)
    ;; chain after cppcheck since this is the last checker in the upstream
    ;; configuration
    (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))));enable flycheck cppcheck style

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :bind
  (("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)))

(use-package imenu
  :bind ("M-s" . imenu)
  :init (setq imenu-auto-rescan t)
  :config (add-hook 'c-mode-hook 'imenu-add-menubar-index))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package python
  :ensure t
  :mode ("\\.py" . python-mode)
  :config
  (defun scm/python-hook ()
    (setq python-indent-offset 4)
    (make-local-variable 'auto-indent-assign-indent-level)
    (setq auto-indent-assign-indent-level 4)
    (setq tab-width 4))
  (add-hook 'python-mode-hook #'scm/python-hook)
  (use-package elpy
    :init (elpy-enable)
    :ensure t
    :config
    (setq python-shell-completion-native-enable nil)
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-timeout nil)
    (when (require 'flycheck nil t)
      (setq elpy-modules
            (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))))

;; (use-package elpy
;;     :ensure t
;;     :defer t
;;     :init
;;     (advice-add 'python-mode :before 'elpy-enable)
;;     :hook ((elpy-mode . flycheck-mode)
;;            (elpy-mode . (lambda ()
;;             (set (make-local-variable 'company-backends)
;;             '((elpy-company-backend :with company-yasnippet))))))
;;     :config
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (setq elpy-rpc-python-command "python3")
;;     (setq elpy-rpc-timeout 2))

(global-set-key (kbd "C-S-s") 'shrink-window)
(global-set-key (kbd "C-S-w") 'enlarge-window)
(global-set-key (kbd "C-S-a") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-d") 'enlarge-window-horizontally)

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun highlight-or-dehighlight-line ()
  (interactive)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face '(:background "honeydew4"))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))


(global-set-key [f9] 'highlight-or-dehighlight-line)

(use-package xclip
  :config
  (xclip-mode 1))

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package anzu
  :init (global-anzu-mode +1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (setq anzu-cons-mode-line-p nil))


(use-package all-the-icons)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))


(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line

(use-package smartscan
  :config
  (global-smartscan-mode 1))

(use-package neotree
  :config
  (progn
    (setq-default neo-smart-open t)
    (setq neo-theme
          (if window-system 'icons 'arrow)) ; 'classic, 'nerd, 'ascii, 'arrow
    (setq neo-vc-integration '(face char))
    (setq neo-show-hidden-files t)
    (setq neo-toggle-window-keep-p t)
    ;; (setq neo-force-change-root t)
    (add-hook 'neotree-mode-hook
              (lambda () (setq-local mode-line-format nil)))
    (set-face-attribute 'neo-vc-edited-face nil
                        :foreground "#E2C08D")
    (set-face-attribute 'neo-vc-added-face nil
                        :foreground "green4")
    (set-face-attribute 'neo-vc-removed-face nil
                        :foreground "indianred1")
    (global-set-key [f5] 'neotree-toggle)))


(use-package visual-regexp
  :defer) ; prevent loading this package before visual-regexp-steroids!

(use-package visual-regexp-steroids
  :demand ; load this package immediately, regardless of :bind
  :bind (("C-c p" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-c r" . vr/isearch-backward)
         ("C-c s" . vr/isearch-forward)))

;; Save all buffers on focus out
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<menu>") nil)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (LaTeX-mode . lsp-deferred))

(use-package yasnippet
  :hook
  (LaTeX-mode . yas-minor-mode))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :init
;;   (global-company-mode)
;;   :config
;;   ;; set default `company-backends'
;;   (setq company-backends
;;         '((company-files          ; files & directory
;;            company-keywords       ; keywords
;;            company-capf)  ; completion-at-point-functions
;;           (company-abbrev company-dabbrev)))
;;   (use-package company-statistics
;;     :ensure t
;;     :init
;;     (company-statistics-mode))
;;   (use-package company-web
;;     :ensure t)
;;   (use-package company-try-hard
;;     :ensure t
;;     :bind
;;     (("C-<tab>" . company-try-hard)
;;      :map company-active-map
;;      ("C-<tab>" . company-try-hard)))
;;   (use-package company-quickhelp
;;     :ensure t
;;     :config
;;     (company-quickhelp-mode))
;;   (use-package company-lsp
;;              :ensure t
;;              :commands company-lsp))
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (use-package company-statistics
    :ensure t
    :init
    (company-statistics-mode))
  (use-package company-web
    :ensure t)
  (use-package company-try-hard
    :ensure t
    :bind
    (("C-<tab>" . company-try-hard)
     :map company-active-map
     ("C-<tab>" . company-try-hard))))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; (use-package company-statistics
;;              :ensure t
;;              :init
;;              (company-statistics-mode))

;; (use-package company-quickhelp
;;              :ensure t
;;              :config
;;              (company-quickhelp-mode))

;; Optional - provides snippet support.
                                        ;(use-package yasnippet
                                        ;  :ensure t
                                        ;  :commands yas-minor-mode
                                        ;  :hook (go-mode . yas-minor-mode))
                                        ;
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook
          (lambda() (setq indent-tabs-mode nil)))

(add-hook 'text-mode-hook 'flyspell-mode)
(setq TeX-save-query nil)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
                                        ;(defvar company-mode/enable-yas t
                                        ;  "Enable yasnippet for all backends.")
                                        ;
                                        ;(defun company-mode/backend-with-yas (backend)
                                        ;  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                                        ;      backend
                                        ;    (append (if (consp backend) backend (list backend))
                                        ;            '(:with company-yasnippet))))
                                        ;
                                        ;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8

;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )
(company-tng-configure-default)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

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
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "M-[") 'hs-toggle-hiding)

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

(global-set-key (kbd "<f7>") 'winstack-push)
(global-set-key (kbd "<S-f7>") 'window-configuration-to-register)

(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))

(global-set-key (kbd "<f8>") 'winstack-pop)
(global-set-key (kbd "<S-f8>") 'jump-to-register)
(add-hook 'after-init-hook 'global-company-mode)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(defun tag-word-or-region (text-begin text-end)
  "Surround current word or region with given text."
  (interactive "sStart tag: \nsEnd tag: ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert text-end)
          (goto-char (region-beginning))
          (insert text-begin))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin)))))

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))

