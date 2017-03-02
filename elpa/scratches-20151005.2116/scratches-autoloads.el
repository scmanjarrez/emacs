;;; scratches-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "scratches" "scratches.el" (22712 12166 321102
;;;;;;  678000))
;;; Generated autoloads from scratches.el

(autoload 'scratches-mode "scratches" "\
Multiple scratches in any language.

When called interactively, toggle `scratches-mode'.  With prefix
ARG, enable `scratches-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `scratches-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `scratches-mode'.
Otherwise behave as if called interactively.

\\{scratches-mode-map}

\(fn &optional ARG)" t nil)

(defvar scratches-global-mode nil "\
Non-nil if Scratches-Global mode is enabled.
See the command `scratches-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `scratches-global-mode'.")

(custom-autoload 'scratches-global-mode "scratches" nil)

(autoload 'scratches-global-mode "scratches" "\
Toggle Scratches mode in all buffers.
With prefix ARG, enable Scratches-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Scratches mode is enabled in all buffers where
`scratches-mode' would do it.
See `scratches-mode' for more information on Scratches mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; scratches-autoloads.el ends here
