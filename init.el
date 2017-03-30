(defvar my-start-time (current-time)
  "Start time")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(line-number-display-limit 67108864)
 '(package-selected-packages
   (quote
    (all-the-icons-dired all-the-icons neotree smooth-scrolling auto-package-update telephone-line anzu company-quickhelp zenburn-theme xclip web-mode undo-tree spinner smartparens scratches rainbow-delimiters markdown-mode latex-preview-pane latex-math-preview latex-extra iedit hydra flycheck-cstyle elpy auto-complete-c-headers auto-complete-auctex ac-math)))
 '(show-trailing-whitespace t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.emacs_backup;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assembler for 68k uses *, not ;
;; (add-hook 'asm-mode-set-comment-hook
;; 	  '(lambda ()
;; 	     (setq asm-comment-char ?*)))

;; (require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my:ciao-config ()
;;   (when (and (stringp buffer-file-name)
;;              (string-match "\\.pl\\'" buffer-file-name))
;;     (load "/usr/lib/ciao/ciao-mode-init");;loads ciao emacs-config
;;     (ciao-mode)))

;; (add-hook 'find-file-hook 'my:ciao-config)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;;loads emacs maximized

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; ignore case when completing, filenames too
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq initial-scratch-message ";; I am your editor. Please describe your program.\n")

;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
(ido-everywhere 1)
(setq magit-completing-read-function 'magit-ido-completing-read)

;;ignore all *xxx* buffers except scratch, ciao and eshell
;; (defvar ido-dont-ignore-buffer-names '("*scratch*" "*Ciao*" "*eshell*" "*Python*"));"*Messages*"))
;; (defun ido-ignore-most-star-buffers (name)
;;   (and
;;    (string-match-p "^*" name)
;;    (not (member name ido-dont-ignore-buffer-names))))

;; (setq ido-ignore-buffers (list "\\` " #'ido-ignore-most-star-buffers))

;; Automatically save and restore sessions
(setq desktop-dirname             (getenv "PWD");;"~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)

;;look for emacs.desktop file
(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; ;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))

;; ;; ask user whether to save desktop at exit
;; (add-hook 'kill-emacs-hook
;; 	  '(lambda ()
;; 	     (if (y-or-n-p "Save desktop? ")
;; 		 (session-save))))



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
;;; desktop-override-stale-locks.el ends here



;; Put backup files neatly away
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/autosaves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ;; Don't delink hardlinks
      delete-old-versions t  ;; Clean up the backups
      version-control t      ;; Use version numbers on backups,
      kept-new-versions 5    ;; keep some new versions
      kept-old-versions 2)   ;; and some old ones, too

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


;; Enable source code pro font and set its size to 110
;; https://github.com/adobe-fonts/source-code-pro
(set-default-font "Source Code Pro" nil t)
(set-face-attribute 'default nil :height 100)

;; enable y/n answers to yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; See matching pairs of parentheses and other characters
(show-paren-mode 1)

;; Highlight brackets if visible, else entire expression
(setq show-paren-style 'mixed)

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; truncate lines, don't break lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Don't show line number if buffer is too big; value in bytes
(customize-set-variable 'line-number-display-limit (* 1024 1024 64))

;; Show buffer size in the mode line
(size-indication-mode 1)

;; Highlight trailing whitespaces in lines
(customize-set-variable 'show-trailing-whitespace t)

;; No toolbar - icons
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Toggle scroll-bar
(toggle-scroll-bar -1)

;; Show current line and column in the mode line
(line-number-mode t)
(column-number-mode t)

;; Delete selected region on write
(delete-selection-mode 1)


(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;enable smartparens global mode
(smartparens-global-mode t)

(sp-pair "(" ")" :wrap "C-(")
(sp-pair "[" "]" :wrap "C-M-[")
(sp-pair "'" "'" :wrap "C-'")
(sp-pair "\"" "\"" :wrap "C-\"")
(sp-pair "{" "}" :wrap "C-{")

(require 'auto-complete)
(global-auto-complete-mode t)  ;;enable global auto-complete
;; (add-hook 'ciao-mode-hook 'auto-complete-mode)  ;;enable auto-complete ciao-mode
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1))) ;;disable auto-complete in python-mode

(require 'auto-complete-config)  ;;load auto-complete config
(ac-config-default)

;; Activate auto-complete for latex modes (AUCTeX or Emacs' builtin one).
(add-to-list 'ac-modes 'latex-mode)

;; Activate ac-math.
(eval-after-load "latex"
  '(when (featurep 'auto-complete)
     ;; See https://github.com/vspinu/ac-math
     (require 'ac-math)
     (defun ac-latex-mode-setup ()       ; add ac-sources to default ac-sources
       (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
             ac-sources)))
     (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)))


(require 'yasnippet) ;;start yasnippet
(yas-global-mode 1)
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)


;;function that triggers on c/c++ mode
(defun my:ac-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/5/include"))
;;function call on c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-header-init)
(add-hook 'c-mode-hook 'my:ac-header-init)

(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;turn on Semantic
(semantic-mode 1)
(add-hook 'python-mode-hook (lambda () (semantic-mode -1))) ;;disable semantic-mode in python-mode

(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;;turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ;;activate rainbow-delimiter programming mode
(add-hook 'ciao-mode-hook 'rainbow-delimiters-mode) ;;activate rainbow-delimiter ciao-mode

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

;; enable hungry-delete
;; (require 'hungry-delete)
;; (add-hook 'c-mode-hook 'hungry-delete-mode)

(require 'flycheck) ;enable flycheck cppcheck style
(add-hook 'c-mode-hook 'global-flycheck-mode) ;(global-flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-cstyle)
     (flycheck-cstyle-setup)
     ;; chain after cppcheck since this is the last checker in the upstream
     ;; configuration
     (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))

(require 'undo-tree)
(global-undo-tree-mode) ;enable undo-tree package globally
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(global-set-key (kbd "M-s") 'imenu)
(setq imenu-auto-rescan t)
(add-hook 'c-mode-hook 'imenu-add-menubar-index)

(global-set-key (kbd "C-x g") 'magit-status)

(defun python-hook ()
  (setq python-indent-offset 4)
  (make-local-variable 'auto-indent-assign-indent-level)
  (setq auto-indent-assign-indent-level 4)
  (setq tab-width 4))

(add-hook 'python-mode-hook 'python-hook)

(elpy-enable)
(setq python-shell-completion-native-enable nil) ;; disable shell native warning
(setq elpy-rpc-backend "jedi")
(elpy-use-ipython)
(when (require 'flycheck nil t)
  (setq elpy-modules
	(delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook (lambda () (company-quickhelp-mode 1)))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)

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

(xclip-mode 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)

;; (sublimity-mode 1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode)
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
		  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)



;; From shackra dotemacs: https://github.com/shackra/.emacs.d
(use-package telephone-line
  :preface (defun shackra/vc-state ()
             (if vc-mode
                 (vc-state (buffer-file-name (current-buffer)))
               nil))
  :init
  (setf telephone-line-height 30)
  (custom-set-faces
   '(mode-line ((t (:box nil))))
   '(mode-line-inactive ((t (:box nil))))
   '(mode-line-highlight ((t (:box nil)))))
  :config
  (telephone-line-defsegment* shackra-buffer-vc-modified-segment
    (list (cond ((eq (shackra/vc-state) 'edited)
                 (propertize (format " %s" (all-the-icons-faicon "pencil")) 'face `(:height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "Modified buffer, changes not committed."))
                ((buffer-modified-p)
                 (propertize (format " %s" (all-the-icons-faicon "pencil")) 'face `(:foreground "tomato" :height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "Modified buffer.")))
          (cond ((eq (shackra/vc-state) 'missing)
                 (propertize (format " %s " (all-the-icons-faicon "trash")) 'face `(:height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "File on CVS, not local."))
                ((eq (shackra/vc-state) 'ignored)
                 (propertize (format " %s " (all-the-icons-faicon "ban")) 'face `(:height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "File ignored."))
                ((eq (shackra/vc-state) 'added)
                 (propertize (format " %s " (all-the-icons-faicon "plus")) 'face `(:height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "File added to be committed."))
                ((eq (shackra/vc-state) 'unregistered)
                 (propertize (format " %s " (all-the-icons-faicon "question")) 'face `(:height 1.3 :family ,(all-the-icons-faicon-family))
                             'display '(raise -0.1) 'help-echo "File unregistered.")))))

  (telephone-line-defsegment shackra-line-buffer-segment
    (telephone-line-raw mode-line-buffer-identification t))

  (telephone-line-defsegment* shackra-vc-info
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode)
             (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
               (concat
                (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:foreground "orange" :height 1.3) 'display '(raise -0.1))
                " · "
                (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                            'face `(:foreground "yellow" :height 1.3 :family ,(all-the-icons-octicon-family))
                            'display '(raise -0.1))
                (propertize (format " %s" branch) 'face `(:foreground "yellow" :height 0.9)))))
            ((string-match "SVN-" vc-mode)
             (let ((revision (cadr (split-string vc-mode "-"))))
               (concat
                (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.3) 'display '(raise -0.1))
                (propertize (format " · %s" revision) 'face `(:height 0.9)))))
            (t (format "%s" vc-mode)))))
  (telephone-line-defsegment* shackra-flycheck-status
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format " ✖ %s problems%s" count (if (> count 1) "s" "")) 'face `(:foreground "orange")))
                                (propertize " ✔ No problems." 'face `(:foreground "dark grey"))))
                   (`running     (propertize " ⟲ Running" 'face `(:foreground "deep sky blue")))
                   (`no-checker  (propertize " ⚠ No checker" 'face `(:foreground "dim grey")))
                   (`not-checked (propertize " ✖ Not checked" 'face `(:foreground "dim grey")))
                   (`errored     (propertize " ⚠ Errored" 'face `(:foreground "tomato")))
                   (`interrupted (propertize " ⛔ Interrupted" 'face `(:foreground "tomato")))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show errors detected by flycheck"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

  (setf telephone-line-lhs
        '((accent . (shackra-line-buffer-segment shackra-buffer-vc-modified-segment))
          (nil .  (shackra-vc-info shackra-flycheck-status))))

  (setf telephone-line-rhs '((nil . (telephone-line-misc-info-segment telephone-line-major-mode-segment))
                             (accent . (telephone-line-minor-mode-segment telephone-line-position-segment))))
  (telephone-line-mode 1))


;; (require 'telephone-line)

;; (setq telephone-line-height 20)

;; (setq telephone-line-lhs
;;       '((accent . (telephone-line-buffer-segment))
;; 	(evil    . (telephone-line-minor-mode-segment))))

;; (setq telephone-line-rhs
;;       '((evil   . (telephone-line-major-mode-segment))
;; 	(accent . (telephone-line-vc-segment))
;; 	(evil   . (telephone-line-airline-position-segment))))

;; (setq telephone-line-primary-left-separator
;;       telephone-line-cubed-left)

;; (setq telephone-line-secondary-left-separator
;;       telephone-line-cubed-hollow-left)

;; (setq telephone-line-primary-right-separator
;;       telephone-line-cubed-right)

;; (setq telephone-line-secondary-right-separator
;;       telephone-line-cubed-hollow-right)

(telephone-line-mode t)

(require 'auto-package-update)
(auto-package-update-maybe)
(setq auto-package-update-delete-old-versions t)

(add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))

(add-hook 'auto-package-update-after-hook
          (lambda () (message "Ok, packages update completed")))

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(require 'smartscan)
(global-smartscan-mode 1)

(require 'neotree)
(require 'all-the-icons)

(setq-default neo-smart-open t)

(setq neo-theme (if window-system 'icons 'arrow)) ; 'classic, 'nerd, 'ascii, 'arrow

(setq neo-vc-integration '(face char))

(setq neo-show-hidden-files t)

(setq neo-toggle-window-keep-p t)

;; (setq neo-force-change-root t)

(add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))

;; face customizations
(set-face-attribute 'neo-vc-edited-face nil
                    :foreground "#E2C08D")
(set-face-attribute 'neo-vc-added-face nil
		    :foreground "green4")
(set-face-attribute 'neo-vc-removed-face nil
		    :foreground "indianred1")

(global-set-key [f5] 'neotree-toggle)

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))

;; Save all buffers on focus out
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
