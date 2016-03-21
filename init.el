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

;; (add-to-list 'load-path "~/.emacs.d/")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;; (ac-config-default)

;; (iswitchb-mode 1)

;; (setq iswitchb-buffer-ignore '("^ " "*Completions*" "*Shell Command Output*"
;;                "*Messages*" "Async Shell Command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "/usr/lib/ciao/ciao-mode-init") ;;Activate ciao-prolog mode

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

(package-initialize)

(smartparens-global-mode t)

(require 'auto-complete-config)
(add-to-list
 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160310.2248/dict")
(ac-config-default)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 80)))

;; (require 'hungry-delete)
;; (global-hungry-delete-mode)
