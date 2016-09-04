;; copyright oleg keri (c) 2009-2016
;; ezhi99@gmail.com
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'company)
(require 'google-c-style)
(require 'cde)
(require 'yaml-mode)
(require 'qml-mode)
(require 'fish-mode)

(load "gdb-ok.elc")
(load "sabbrevs.elc")

;; vars
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings
(setq fill-column 80)
(setq indent-tabs-mode nil)
(setq comment-style 'indent)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq save-abbrevs nil)
(setq linum-format "%5.d|")
(setq column-number-mode t)
(setq company-backends '(company-cde company-capf company-files company-nxml
				     company-css company-cmake))
(setq company-async-timeout 3)
(setq compilation-scroll-output t)
(setq use-dialog-box nil)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq w32-get-true-file-atttributes nil)
(setq gud-key-prefix "\C-x\C-g")
(setq cde-debug t)
(setq cde-check 1)

;; init
(display-time)
(normal-erase-is-backspace-mode 0)
(show-paren-mode 1)
(ido-mode 1)
(xterm-mouse-mode)
(c-add-style "Google" google-c-style)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))

;; cp1251 support
(define-coding-system-alias 'windows-1251 'cp1251)
(define-coding-system-alias 'win-1251 'cp1251)
(set-input-mode nil nil 'We-will-use-eighth-bit-of-input-byte)
(set-language-info-alist
 "Cyrillic-CP1251" `((charset cyrillic-iso8859-5)
		     (coding-system cp1251)
		     (coding-priority cp1251)
		     (input-method . "cyrillic-jcuken")
		     (features cyril-util)
		     (unibyte-display . cp1251)
		     (sample-text . "Russian (Русский) Здравствуйте!")
		     (documentation . "Support for Cyrillic CP1251."))
 '("Cyrillic"))

;; bindings
(global-set-key [\C-f1] 'gdb-start)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'next-error)
(global-set-key [\C-f8] 'previous-error)
(global-set-key [f9] 'isearch-toggle-case-fold)
(global-set-key [\C-f9] 'toggle_write_triggers)
(global-set-key [f10] 'menu-bar-open)
(global-set-key [f11] 'switchcp1251)
(global-set-key [f12] 'kill-emacs)
(global-set-key [\C-/] 'undo)
(global-set-key [\C-_] 'undo)
(global-set-key [?\C-c left] 'uncomment-region)
(global-set-key [?\C-c right] 'comment-region)
(global-set-key [?\C-c ?d] 'vc-diff)
(global-set-key [?\C-c ?c] 'eshell)
(global-set-key [?\C-c ?\C-j] 'eval-print-last-sexp)
(global-set-key [?\C-c ?\t] 'untabify)
(global-set-key [?\C-x ?x] 'previous-multiframe-window)
(global-set-key [?\C-x ?\C-x] 'next-multiframe-window)
(global-set-key (kbd "ESC <left>") 'previous-multiframe-window)
(global-set-key (kbd "ESC <right>") 'next-multiframe-window)
(global-set-key [?\C-x ?f] 'ibuffer)
(global-set-key [?\C-x ?g] 'ibuffer-other-window)
(global-set-key [(meta /)] 'company-manual-begin)
(global-set-key [(control meta _)] 'company-files)
(global-set-key [(control j)] 'indent-region)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
(global-unset-key [?\C-x ?\C-e])

;; gdb init function
(defun gdb-start()
  (interactive)
  (if (or (not (boundp 'gud-comint-buffer))
	  (not (get-buffer-process gud-comint-buffer)))
      (progn
	(global-set-key [f1] 'gdb-switch)
	(global-set-key [f2] (lambda() (interactive) (gdb-go t nil)))
	(global-set-key [f3] (lambda() (interactive) (gdb-go nil nil)))
	(global-set-key [\C-f2] (lambda() (interactive) (gdb-go t t)))
	(global-set-key [\C-f3] (lambda() (interactive) (gdb-go nil t)))
	(global-set-key [f4] 'gdb-toggle-break)
	(global-set-key [\C-f4] 'gdb-var-clear)
	(global-set-key [f5] 'gud-cont)
	(global-set-key [f6] 'gdb-switch2)
	(global-set-key [\C-f5] 'gud-until)
	(global-set-key [?\M-\[ ?m] 'gud-watch)
	(global-set-key "\M-\r" 'gud-watch)
	(global-set-key [?\C-x ?w] (lambda() (interactive) (gud-watch 1)))
	(gdb "gdb -i=mi2 -quiet"))
    (if (y-or-n-p "Are you sure to quit debug ? ")
	(progn
	  (gdb-quit)
	  (global-unset-key [f1])
	  (global-unset-key [f2])
	  (global-unset-key [f3])
	  (global-unset-key [\C-f2])
	  (global-unset-key [\C-f3])
	  (global-unset-key [f4])
	  (global-unset-key [\C-f4])
	  (global-unset-key [f5])
	  (global-unset-key [f6])
	  (global-unset-key [\C-f5])
	  (global-unset-key [?\M-\[ ?m])
	  (global-unset-key "\M-\r")
	  (global-unset-key [?\C-x ?w])))))


;; extensions
(defun message-box(st &optional crap)
  (dframe-message st))

;; translate encoding
(defun switchcp1251 ()
  "functions switches encoding to cp1251"
  (interactive)
  (let ((coding-system-for-read 'cp1251))
    (revert-buffer t t)
    (message "Encoding changed")))

(defun toggle_write_triggers()
  (interactive)
  (if (member 'delete-trailing-whitespace write-file-functions)
      (prog1
	  (setq write-file-functions (delete 'delete-trailing-whitespace write-file-functions))
	(message "write triggers disabled"))
    (prog1
	(add-to-list 'write-file-functions 'delete-trailing-whitespace)
      (message "write triggers enabled"))))

;; hooks and etc...
(add-hook 'prog-mode-hook
	  (lambda()
	    (linum-mode 1)
	    (company-mode-on)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'c-mode-common-hook
	  (lambda()
	    (setq abbrev-mode t)
	    (c-set-style "Google")
	    (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'c++-mode-hook
	  (lambda()
	    (global-set-key [f7] 'cde-compile)
	    (local-set-key [?\C-x ?\C-l] 'cde-update-project)
	    (local-set-key [?\C-x ?\C-a] 'cde-symbol-back)
	    (local-set-key [?\C-x ?\C-r] 'cde-symbol-ref)
	    (local-set-key [?\C-x ?\C-d] 'cde-symbol-def)
	    (local-set-key [?\C-x ?d] 'cde-header-source)
	    (cde-mode)))

(add-hook 'python-mode-hook
	  '(lambda()
	     (require 'anaconda-mode)
	     (require 'company-anaconda)
	     (add-to-list 'company-backends 'company-anaconda)
	     (local-set-key [?\C-x ?\C-a] 'anaconda-mode-go-back)
	     (local-set-key [?\C-x ?\C-r] 'anaconda-mode-find-references)
	     (local-set-key [?\C-x ?\C-d] 'anaconda-mode-find-definitions)
	     (anaconda-mode)))

(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))


;; faces
(custom-set-faces
 '(font-lock-keyword-face ((t (:bold t :foreground "cyan"))))
 '(font-lock-comment-face ((t (:foreground "yellow"))))
 '(font-lock-doc-face ((t (:bold t :foreground "color-166"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-33"))))
 '(font-lock-string-face ((t (:foreground "color-33"))))
 '(font-lock-variable-name-face ((t (:weight normal :foreground "color-180"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "Blue"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-constant-face ((t (:foreground "color-56"))))
 '(linum ((t (:weight normal :foreground "grey40"))))
 '(error ((t (:foreground "red"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-removed ((t (:foreground "red"))))
 '(diff-header ((t (:foreground "grey45"))))
 '(diff-file-header ((t (:bold t :foreground "grey60"))))
 '(diff-context ((t (:foreground "white"))))
 '(company-tooltip ((t (:background "grey" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "color-23" :foreground "black")))))
