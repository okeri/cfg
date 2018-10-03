;; copyright Oleg Keri (c) 2009-2016
;; ezhi99@gmail.com


(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'google-c-style)
(require 'cde)
(require 'yaml-mode)
(require 'qml-mode)
(require 'fish-mode)
(require 'cmake-mode)

(load "gdb-ok.elc")
(load "sabbrevs.elc")
(load "rust.elc")
(load "clang-format.elc")

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
(setq column-number-mode t)
(setq company-backends '(company-cde company-capf company-files company-nxml
				      company-jedi company-css company-cmake company-dabbrev))
(setq company-async-timeout 5)
(setq compilation-scroll-output t)
(setq use-dialog-box nil)
(setq ido-enable-flex-matching t)
(setq vc-annotate-background "black")
(setq vc-annotate-background-mode nil)

(setq ido-everywhere t)
(setq w32-get-true-file-atttributes nil)
(setq gud-key-prefix "\C-x\C-g")
;(setq debug-on-error t)
;(setq cde-debug t)
(setq cde-check 3)
(setq cde-showdef-delay 1)
(setq cde-command "cde -C/home/okeri/cache")
(setq non-cde-exts '("cl" "sl" "glsl" "php"))

;; init
(display-time)
(normal-erase-is-backspace-mode 0)
(show-paren-mode 1)
(ido-mode 1)
(xterm-mouse-mode)
(c-add-style "Google" google-c-style)

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

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
(global-set-key [f10] 'menu-bar-open)
(global-set-key [f11] 'switchcp1251)
(global-set-key [f12] 'kill-emacs)
(global-set-key [\C-/] 'undo)
(global-set-key [\C-_] 'undo)
(global-set-key [?\C-c left] 'uncomment-region)
(global-set-key [?\C-c right] 'comment-region)
(global-set-key [?\C-c ?d] 'vc-diff)
(global-set-key [?\C-c ?w] 'vc-annotate)
(global-set-key [?\C-c ?l] 'vc-print-log)
(global-set-key [?\C-c ?c] 'eshell)
(global-set-key [?\C-c ?\C-j] 'eval-print-last-sexp)
(global-set-key [?\C-c ?\t] 'untabify)
(global-set-key [?\C-x ?x] 'previous-multiframe-window)
(global-set-key [?\C-x ?\C-x] 'next-multiframe-window)
(global-set-key [\C-left] 'previous-multiframe-window)
(global-set-key [\C-right] 'next-multiframe-window)
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

;; hooks and etc...
(defun stdprog()
  (company-mode-on)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'stdprog)
(add-hook 'qml-mode-hook 'stdprog)

(add-hook 'c-mode-common-hook
	  (lambda()
	    (setq abbrev-mode t)
	    (local-set-key [(control j)] 'clang-format-region)
	    (c-set-style "Google")
;	    (fset 'c-indent-region 'clang-format-region)
	    (local-set-key (kbd "TAB") 'clang-format-region)
	    (if (not (member (file-name-extension (buffer-file-name)) non-cde-exts))
		(cde-mode))))
(add-hook 'before-save-hook
	  (lambda()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'qml-mode)
	      (clang-format-buffer))))

(add-hook 'c-mode-hook
	  (lambda()
	    (setq-local indent-tabs-mode t)
	    (c-set-style "linux")))

(add-hook 'cde-mode-hook
	  (lambda()
	    (local-set-key [?\C-x ?\C-l] 'cde-update-project)
	    (local-set-key [?\C-x ?\C-a] 'cde-symbol-back)
	    (local-set-key [?\C-x ?\C-r] 'cde-symbol-ref)
	    (local-set-key [?\C-x ?\C-d] 'cde-symbol-def)
	    (local-set-key [?\C-x ?d] 'cde-header-source)
	    (local-set-key [f7] 'cde-compile)))

(add-hook 'python-mode-hook
	  '(lambda()
	     (require 'company-jedi)
	     (local-set-key [?\C-x ?\C-a] 'jedi:goto-definition-pop-marker)
	     (local-set-key [?\C-x ?\C-d] 'jedi:goto-definition)
	     (jedi:setup)))

(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:background "grey" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "color-23" :foreground "black"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-context ((t (:foreground "white"))))
 '(diff-file-header ((t (:bold t :foreground "grey60"))))
 '(diff-header ((t (:foreground "grey45"))))
 '(diff-removed ((t (:foreground "red"))))
 '(error ((t (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "yellow"))))
 '(font-lock-constant-face ((t (:foreground "color-135"))))
 '(font-lock-doc-face ((t (:bold t :foreground "color-166"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "color-39"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-39"))))
 '(font-lock-string-face ((t (:foreground "color-39"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:weight normal :foreground "color-180"))))
 '(minibuffer-prompt ((t (:foreground "color-39")))))
