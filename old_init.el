;; Modify the comment symbol for asm 68k, '*' instead of ';'
;; (add-hook 'asm-mode-set-comment-hook '(lambda () (setq asm-comment-char ?*)))

;; If Ciao Prolog didn't get configured automatically, this function load ciao-mode-init.el
;; (defun scm/ciao-config ()
;;   (when (and (stringp buffer-file-name)
;;              (string-match "\\.pl\\'" buffer-file-name))
;;     (load "/usr/lib/ciao/ciao-mode-init")
;;     (ciao-mode)))

;; (add-hook 'find-file-hook 'my/ciao-config)


;; Ask user to restore desktop at start-up
;; (add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))

;; Ask user to save desktop at start-up
;; (add-hook 'kill-emacs-hook
;; 	  '(lambda ()
;; 	     (if (y-or-n-p "Save desktop? ")
;; 		 (session-save))))


;; ;; Put backup files neatly away
;; (let ((backup-dir "~/.emacs.d/backups")
;;       (auto-saves-dir "~/.emacs.d/autosaves/"))
;;   (dolist (dir (list backup-dir auto-saves-dir))
;;     (when (not (file-directory-p dir))
;;       (make-directory dir t)))
;;   (setq backup-directory-alist `(("." . ,backup-dir))
;;         auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
;;         auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
;;         tramp-backup-directory-alist `((".*" . ,backup-dir))
;;         tramp-auto-save-directory auto-saves-dir))

;; (setq backup-by-copying t    ;; Don't delink hardlinks
;;       delete-old-versions t  ;; Clean up the backups
;;       version-control t      ;; Use version numbers on backups,
;;       kept-new-versions 5    ;; keep some new versions
;;       kept-old-versions 2)   ;; and some old ones, too


;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; (add-hook 'ciao-mode-hook 'auto-complete-mode)

;; enable hungry-delete
;; (require 'hungry-delete)
;; (add-hook 'c-mode-hook 'hungry-delete-mode)


;; (use-package auto-package-update
;;   :config
;;   (progn
;;     (auto-package-update-maybe)
;;     (setq auto-package-update-delete-old-versions t)
;;     (add-hook 'auto-package-update-before-hook
;; 	      (lambda () (message "I will update packages now")))
;;     (add-hook 'auto-package-update-after-hook
;; 	      (lambda () (message "Ok, packages update completed")))))


