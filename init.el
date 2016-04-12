(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.emacs_backup;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assembler for 68k uses *, not ;
;; (add-hook 'asm-mode-set-comment-hook
;; 	  '(lambda ()
;; 	     (setq asm-comment-char ?*)))

;; (require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "/usr/lib/ciao/ciao-mode-init") ;;Activate ciao-prolog mode

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;;loads emacs maximized

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 )

(add-to-list
 'package-archives
 '("gnu" . "https://elpa.gnu.org/packages/")
 )

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)
(setq show-paren-style
      'mixed) ; highlight brackets if visible, else entire expression

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

(setq
 completion-ignore-case t   ;; ignore case when completing...
 read-file-name-completion-ignore-case t) ;; ...filenames too

(setq
 initial-scratch-message
 ";; I am your editor. Please describe your program.\n")

(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
;;ignore all *xxx* buffers except scratch
(defvar ido-dont-ignore-buffer-names '("*scratch*" "*Ciao*"));"*Messages*"))
(defun ido-ignore-most-star-buffers (name)
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))

(setq ido-ignore-buffers (list "\\` " #'ido-ignore-most-star-buffers))

;; deprecated
;; (iswitchb-mode 1) ;; enable fast switch buffer C-x b + C-s/C-r
;; (setq iswitchb-buffer-ignore '("^ " "*Completions*" "*Shell Command Output*"
;;                "*Messages*" "Async Shell Command"))

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

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))

;; ask user whether to save desktop at exit
(add-hook 'kill-emacs-hook
	  '(lambda ()
	     (if (y-or-n-p "Save desktop? ")
		 (session-save))))



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

(setq backup-by-copying t    ; Don't delink hardlinks                           
      delete-old-versions t  ; Clean up the backups                             
      version-control t      ; Use version numbers on backups,                  
      kept-new-versions 5    ; keep some new versions                           
      kept-old-versions 2)   ; and some old ones, too 



;; truncate lines, don't break lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

(tool-bar-mode -1)                 ; No toolbar - icons

(toggle-scroll-bar -1)             ; Toggle scroll-bar

(column-number-mode t)             ; Show column number in mode-line

(delete-selection-mode 1)          ; Delete selected region on write

(package-initialize)

(smartparens-global-mode t) ;;enable smartparens global mode

(require 'auto-complete)
(global-auto-complete-mode t)  ;;enable global auto-complete
(add-hook 'ciao-mode-hook
	  'auto-complete-mode)  ;;enable auto-complete ciao-mode

(require 'auto-complete-config)  ;;load auto-complete config
(add-to-list
 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160310.2248/dict")
(ac-config-default)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook
	  'rainbow-delimiters-mode) ;;activate rainbow-delimiter programming mode
(add-hook 'ciao-mode-hook
	  'rainbow-delimiters-mode) ;;activate rainbow-delimiter ciao-mode

(load-theme 'monokai t)  ;;enable monokai-theme

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
(add-hook 'after-init-hook #'global-flycheck-mode) ;(global-flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-cstyle)
     (flycheck-cstyle-setup)
     ;; chain after cppcheck since this is the last checker in the upstream
     ;; configuration
     (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))

(require 'undo-tree)
(global-undo-tree-mode) ;enable undo-tree package globally
