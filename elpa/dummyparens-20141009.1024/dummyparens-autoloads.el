;;; dummyparens-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dummyparens" "dummyparens.el" (0 0 0 0))
;;; Generated autoloads from dummyparens.el

(put 'global-dummyparens-mode 'globalized-minor-mode t)

(defvar global-dummyparens-mode nil "\
Non-nil if Global Dummyparens mode is enabled.
See the `global-dummyparens-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dummyparens-mode'.")

(custom-autoload 'global-dummyparens-mode "dummyparens" nil)

(autoload 'global-dummyparens-mode "dummyparens" "\
Toggle Dummyparens mode in all buffers.
With prefix ARG, enable Global Dummyparens mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dummyparens mode is enabled in all buffers where
`turn-on-dummyparens-mode' would do it.

See `dummyparens-mode' for more information on Dummyparens mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dummyparens" '("dp-" "dummyparens-mode" "turn-on-dummyparens-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dummyparens-autoloads.el ends here
