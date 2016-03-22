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
(desktop-save-mode 1)

;; truncate lines, don't break lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

(tool-bar-mode -1)                 ; No toolbar - icons

(toggle-scroll-bar -1)             ; Toggle scroll-bar

(column-number-mode t)             ; Show column number in mode-line

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
