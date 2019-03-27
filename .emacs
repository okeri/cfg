;; ezhi99@gmail.com

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'gdb-ok)

;; vars
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings
(setq fill-column 80
      indent-tabs-mode nil
      comment-style 'indent
      inhibit-startup-message t
      make-backup-files nil
      auto-save-list-file-name nil
      auto-save-default nil
      save-abbrevs nil
      column-number-mode t
      format-on-save t
      company-backends '(company-capf company-files company-nxml company-css
				      company-cmake)

      company-async-timeout 3
      project-find-functions '(project-try-ccj project-try-vc)
      lsp-enable-links nil
      lsp-eldoc-enable-hover nil
      lsp-eldoc-enable-signature-help nil
      lsp-eldoc-prefer-signature-help nil
      lsp-enable-folding nil
      lsp-imenu-container-name-separator "::"
      lsp-prefer-flymake t
      compilation-scroll-output t
      use-dialog-box nil
      vc-annotate-background "black"
      vc-annotate-background-mode nil
      display-line-numbers-width-start 4
      w32-get-true-file-atttributes nil
      gud-key-prefix "\C-x\C-g"
      recentf-max-saved-items 64
      ya-cppref-path-to-doc-root "/usr/share/cpp/reference/"
      ivy-height 16
      ivy-fixed-height-minibuffer t
      ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      enable-recursive-minibuffers t
      ivy-format-function 'ivy-format-function-line
      ivy-rich--display-transformers-list
      '(ivy-switch-buffer
	(:columns
	 ((ivy-rich-candidate (:width 0.2))
	  (ivy-rich-switch-buffer-size (:width 0.1))
	  (ivy-rich-switch-buffer-major-mode (:width 0.1 :face warning))
	  (ivy-rich-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.6))))))
	 :predicate
	 (lambda (cand) (get-buffer cand)))
	counsel-M-x
	(:columns
	 ((counsel-M-x-transformer (:width 40))
	  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	counsel-describe-function
	(:columns
	 ((counsel-describe-function-transformer (:width 40))
	  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	counsel-describe-variable
	(:columns
	 ((counsel-describe-variable-transformer (:width 40))
	  (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
	counsel-recentf
	(:columns
	 ((ivy-rich-candidate (:width 0.8))
	  (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

;; init
(normal-erase-is-backspace-mode 0)
(show-paren-mode 1)
(menu-bar-mode 0)
(xterm-mouse-mode)
(recentf-mode 1)
(ivy-mode 1)
(ivy-rich-mode 1)
(counsel-mode 1)

(add-to-list 'auto-mode-alist '("\\meson.build\\'" . meson-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

;; bindings
(global-set-key [\C-f1] 'gdb-start)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'my-next-error)
(global-set-key [\C-f8] 'my-prev-error)
(global-set-key [f9] 'toggle-format-on-save)
(global-set-key [f10] 'menu-bar-open)
(global-set-key [f12] 'kill-emacs)
(global-set-key [\C-/] 'undo)
(global-set-key [\C-_] 'undo)
(global-set-key [?\C-c left] 'uncomment-region)
(global-set-key [?\C-c right] 'comment-region)
(global-set-key [?\C-c ?d] 'vc-diff)
(global-set-key [?\C-c ?w] 'vc-annotate)
(global-set-key [?\C-c ?l] 'vc-print-log)
(global-set-key [?\C-c ?c] 'eshell)
(global-set-key [?\C-c ?\t] 'untabify)
(global-set-key [?\C-x ?x] 'previous-multiframe-window)
(global-set-key [?\C-x ?\C-x] 'next-multiframe-window)
(global-set-key [\C-left] 'previous-multiframe-window)
(global-set-key [?\C-x ?c] 'counsel-imenu)
(global-set-key [?\C-x ?e] 'counsel-git-grep)
(global-set-key [?\C-x ?\C-e] 'counsel-ag)
(global-set-key [?\C-r] 'swiper-at-point)
(global-set-key [?\C-s] 'swiper)
(global-set-key [\C-right] 'next-multiframe-window)
(global-set-key [?\C-x ?f] 'ivy-switch-buffer)
(global-set-key [?\C-x ?g] 'ivy-switch-buffer-other-window)
(global-set-key [(meta /)] 'company-manual-begin)
(global-set-key [?\C-j] 'eval-print-last-sexp)
(global-set-key [(control meta _)] 'company-files)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)


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

(defun project-try-template (dir id)
  (let ((f (locate-dominating-file dir id)))
    (when f (cons 'vc (file-name-directory f)))))

(defun project-try-ccj(dir)
  (project-try-template dir "compile_commands.json"))

(defun my-lsp-dispay() 
  "Alternative (minimalistic) UI to lsp-ui :)"
  (when (bound-and-true-p lsp-mode) 
    (let ((diags (lsp--cur-line-diagnotics)))
      (if (= (length diags) 0)
	  (when (lsp--capability "hoverProvider")
	    (lsp--send-request-async
	     (lsp--make-request "textDocument/hover"
				(lsp--text-document-position-params))
	     (lambda (info)
	       (when info 
  		 (let ((contents (gethash "contents" info)))
  		   (when (and (hash-table-p contents) (not (hash-table-empty-p contents)))
  		     (let* ((value (gethash "value" contents))
			    (npos (+ 2 (string-match "$" value))))
		       (if (>= npos (length value))
			   (message value)
			 (message (substring value npos))))))))))
  	(message (gethash "message" (aref diags 0)))))))

(defun my-compile()
  "Suggest to compile of project directory"
  (interactive)
  (when (or (not (boundp 'compile-history))
	    (= (length compile-history) 0))
    (setq compile-history '("make -k ")))
  (let ((curr (concat "make -k -C " (car (project-roots (project-current t))))))
    (unless (catch 'found
	      (dolist (v compile-history)
		(when (string-prefix-p curr v)
		  (throw 'found t)))
	      nil)
      (push curr compile-history)))
  (when (> (length compile-history) 0)
    (setq compile-command (car compile-history)))
  (execute-extended-command nil "compile"))

(defun my-header-source()
  (interactive)
  ;; TODO: think about strategy
  )

(defun my-next-error()
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (prog1
	  (flymake-goto-next-error)
	(my-lsp-dispay))
    (next-error)))

(defun my-prev-error()
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (prog1
	  (flymake-goto-prev-error)
	(my-lsp-dispay))
    (previous-error)))

(defun swiper-at-point()
  (interactive)
  (swiper--ivy (swiper--candidates) (thing-at-point 'symbol)))

(defun stdprog()
  (setq mode-line-format '("%e" mode-line-modified  mode-line-buffer-identification " " (vc-mode vc-mode) " " mode-line-modes mode-line-misc-info mode-line-end-spaces))
  (display-line-numbers-mode)
  (company-mode))

(defun setup-lsp()
  (lsp)
  (local-set-key [?\C-x ?\C-r] 'lsp-rename)
  (local-set-key [?\C-x ?\C-d] 'lsp-find-definition)
  (local-set-key [?\C-x ?\C-a] 'xref-pop-marker-stack))

(defun format-region(start end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (lsp-format-region start end)
  (back-to-indentation))

(defun ivy-rich-path (candidate)
  (let* ((buffer (get-buffer candidate))
	 (filename (when buffer
		     (buffer-local-value 'default-directory buffer))))
    (if filename filename "" )))

(defun toggle-format-on-save()
  (interactive)
  (if format-on-save
      (prog1
	  (setq format-on-save nil)
	(message "format on save disabled"))
    (prog1
	(setq format-on-save t)
      (message "format on save enabled"))))

(with-eval-after-load 'counsel
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
	(alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

(with-eval-after-load 'ivy
  (define-key ivy-switch-buffer-map [?\C-d] #'ivy-switch-buffer-kill))

(with-eval-after-load 'yasnippet
  (yas-load-directory (car yas-snippet-dirs) t))

;; override flymake legacy hook
(with-eval-after-load 'flymake-proc
  (setq flymake-diagnostic-functions '(lsp--flymake-backend)))

;; hooks and etc...
(add-hook 'lsp-eldoc-hook 'my-lsp-dispay)
(add-hook 'prog-mode-hook 'stdprog)
(add-hook 'qml-mode-hook 'stdprog)

(add-hook 'c-mode-common-hook
	  (lambda()
	    (fset 'c-indent-region 'lsp-format-region)
	    (local-set-key (kbd "TAB") 'format-region)
	    (local-set-key [f7] 'my-compile)
	    (local-set-key [?\C-x ?d] 'my-header-source)
	    (abbrev-mode 0)
	    (setup-lsp)))

(add-hook 'before-save-hook
	  (lambda()
	    (when format-on-save
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(lsp-format-buffer))
	      (when (derived-mode-p 'python-mode)
		(delete-trailing-whitespace)))))

(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'rust-mode-hook
	  (lambda()
	    (local-set-key [\C-f7] 'compile)
	    (local-set-key [f7] 'cargo-process-build)
	    (setup-lsp)))

(add-hook 'python-mode-hook 'setup-lsp)
(add-hook 'emacs-lisp-mode-hook
	  (lambda()
	    (local-set-key [?\C-x ?\C-d] 'find-function-at-point)))
 
;; faces
(set-face-attribute 'highlight nil :background "#303030")
(set-face-attribute 'region nil :background "#303030")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:background "grey" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "#005f5f" :foreground "black"))))
 '(diff-added ((t (:foreground "#00cd00"))))
 '(diff-context ((t (:foreground "white"))))
 '(diff-file-header ((t (:bold t :foreground "grey60"))))
 '(diff-header ((t (:foreground "grey45"))))
 '(diff-removed ((t (:foreground "#cd0000"))))
 '(error ((t (:foreground "#cd0000"))))
 '(font-lock-comment-face ((t (:foreground "#cdcd00"))))
 '(font-lock-constant-face ((t (:foreground "#af5fff"))))
 '(font-lock-doc-face ((t (:bold t :foreground "#d75f00"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#00afff"))))
 '(font-lock-keyword-face ((t (:foreground "#00cdcd"))))
 '(font-lock-preprocessor-face ((t (:foreground "#00afff"))))
 '(font-lock-string-face ((t (:foreground "#00afff"))))
 '(font-lock-type-face ((t (:foreground "#00cd00"))))
 '(font-lock-variable-name-face ((t (:weight normal :foreground "color-180"))))
 '(lsp-ui-doc-background ((t (:background "#00005f"))))
 '(minibuffer-prompt ((t (:foreground "#00afff"))))
 '(org-table ((t (:foreground "#00cdcd")))))

(put 'downcase-region 'disabled nil)
