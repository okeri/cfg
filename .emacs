(require 'package)
(add-to-list 'package-archives '("melpa" .  "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package emacs
  :init
  (setq fill-column 80
        xterm-query-timeout nil
        indent-tabs-mode nil
        commeBBnt-style 'indent
        inhibit-startup-message t
        make-backup-files nil
        auto-save-list-file-name nil
        auto-save-default nil
        save-abbrevs nil
        column-number-mode t
        custom-file "/tmp/custom.el"
        read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000
        compilation-scroll-output t
        use-dialog-box nil
        display-line-numbers-width-start 4
	format-on-save t
	interprogram-cut-function 'wl-clipboard
        gud-key-prefix "\C-x\C-g")

  (defalias 'yes-or-no-p 'y-or-n-p)
  (normal-erase-is-backspace-mode 0)
  (show-paren-mode 1)
  (menu-bar-mode 0)
  (xterm-mouse-mode)
  (put 'downcase-region 'disabled nil)

  :bind (([f7] . compile)
         ([f8] . next-error)
         ([\C-f8] . previous-error)
         ([\C-/] . undo)
         ([\C-_] . undo)
         ([?\C-c left] . uncomment-region)
         ([?\C-c right] . comment-region)
         ([?\C-c ?\t] . untabify)
         ([\C-left] . previous-multiframe-window)
         ([\C-right] . next-multiframe-window)
         ([?\C-x ?x] . previous-multiframe-window)
         ([?\C-x ?\C-x] . next-multiframe-window)
         ([?\C-j] . eval-print-last-sexp)
         ([mouse-4] . scroll-down)
         ([mouse-5] . scroll-up)
         ([f12] . kill-emacs)
	 ([f10] . toggle-format-on-save))

  :config
  (add-to-list 'term-file-aliases '("foot" . "xterm"))
  (setq-default auto-mode-alist
                (append auto-mode-alist '(("\\.cu\\'" . c++-mode)
                                          ("\\.cl\\'" . c-mode)
                                          ("\\.sl\\'" . c-mode)
                                          ("\\.qml\\'" . qml-mode)))))



(use-package project
  :ensure nil
  :config
  (defun project-update-compile-command ()
    (when-let ((pr (project-current)))
      (let* ((root (expand-file-name (project-root pr)))
             (build-cmd (cond
                         ((file-exists-p (concat root "build/build.ninja")) (concat "ninja -C " root "build"))
                         ((file-exists-p (concat root "build/Makefile")) (concat "make -C " root "build"))
                         ((file-exists-p (concat root "Makefile")) (concat "make -C " root))
                         (t "make -k "))))
        (setq-local compile-command build-cmd))))
  :hook (prog-mode . project-update-compile-command))


(use-package ivy
  :custom
  (ivy-height 16)
  (ivy-fixed-height-minibuffer t)
  :init
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        enable-recursive-minibuffers t
        ivy-format-function 'ivy-format-function-line)
  :config
  (ivy-mode 1)
  :bind (("C-s" . swiper)
         ("C-r" . swiper-thing-at-point)
         ("C-x f" . ivy-switch-buffer)
         ("C-x g" . ivy-switch-buffer-other-window)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)))

(use-package ivy-rich
  :after (ivy project)
  :init
  (defun my/ivy-rich-project-name (cand)
    (if-let* ((buf (get-buffer cand))
              (pr (with-current-buffer buf (project-current))))
        (file-name-nondirectory (directory-file-name (project-root pr)))
      "-"))

  (defun my/ivy-rich-rel-path (cand)
    (if-let* ((buf (get-buffer cand))
              (file (buffer-file-name buf))
              (pr (with-current-buffer buf (project-current))))
        (file-relative-name file (project-root pr))
      (or (buffer-file-name (get-buffer cand)) "")))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 0.15))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
            (my/ivy-rich-project-name (:width 15 :face success))
            (my/ivy-rich-rel-path (:width 115)))
           :predicate (lambda (cand) (get-buffer cand)))))
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :config
  (counsel-mode 1)
  (let ((done (where-is-internal #'ivy-done ivy-minibuffer-map t))
        (alt (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt #'ivy-done))
  :bind (("C-c C-o" . counsel-imenu)
         ("C-c e" . counsel-ag)
	 ("C-x e" . (lambda () (interactive) (counsel-git-grep (ivy-thing-at-point))))
         ("C-x C-e" . (lambda () (interactive) (counsel-ag (ivy-thing-at-point))))))


(use-package eglot
  :bind (("C-c o" . eglot-code-actions)
         ("C-c C-r" . eglot-rename))
  :custom
  (eglot-workspace-configuration '(:java (:configuration (:updateBuildConfiguration "interactive"))))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "--header-insertion=never" "--completion-style=detailed" "--pch-storage=memory" "--query-driver=/bin/g++")))
  ;; Your diagnostic cleanup hack
  (advice-add 'eglot--format-markup :filter-args
              (lambda (markup)
                (let ((elem (car markup)))
                  (plist-put elem :value (replace-regexp-in-string
                                          (regexp-quote "  \n") "\n"
                                          (plist-get elem :value) nil 'literal)))
                markup))
  :hook (eglot-managed-mode . (lambda ()
                               (setq eldoc-documentation-functions
                                     (cons #'flymake-eldoc-function
                                           (remove #'flymake-eldoc-function eldoc-documentation-functions))))))

(use-package flymake
  :bind (([f6] . flymake-show-buffer-diagnostics)
         ([f9] . flymake-goto-next-error)
         ([\C-f9] . flymake-goto-prev-error)))

(use-package company
  :init
  (setq company-backends '(company-capf company-files company-nxml company-cmake)
        company-async-timeout 3)
  :bind (([(meta /)] . company-manual-begin)))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package eldoc
  :init
  (setq eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-use-multiline-p 0.5))

(use-package rust-mode
  :hook (rust-mode . eglot-ensure)
  :bind (:map rust-mode-map ([f7] . cargo-process-build)))

(use-package cargo :after rust-mode)

(use-package fish-mode)
(use-package yaml-mode)
(use-package cmake-mode)
(use-package meson-mode)

(use-package cff
  :init
  (setq cff-source-regexps '(("\\.c$" . (lambda (base) (concat base ".c")))
                             ("\\.cc$" . (lambda (base) (concat base ".cc")))
                             ("\\.cxx$" . (lambda (base) (concat base ".cxx")))
                             ("\\.cpp$" . (lambda (base) (concat base ".cpp")))
                             ("\\.cu$" . (lambda (base) (concat base ".cu"))))))

(use-package xref
  :bind (("C-x C-r" . xref-find-references)
         ("C-x C-d" . xref-find-definitions)
         ("C-x C-a" . xref-pop-marker-stack)))


(use-package recentf
  :init (setq recentf-max-saved-items 512)
  :config (recentf-mode 1))

(use-package vc
  :init
  (setq vc-annotate-background "black"
        vc-annotate-background-mode nil)
  :bind (("C-c d" . vc-diff)
         ("C-c w" . vc-annotate)
         ("C-c l" . vc-print-log)
         ("C-c k" . vc-region-history)))

;; misc
(use-package pinentry
  :init (setq epa-pinentry-mode 'loopback)
  :config (pinentry-start))

(defun wl-clipboard(text)
  (let ((wl-copy-process (make-process :name "wl-copy" :buffer nil :command '("wl-copy" "-f" "-n") :connection-type 'pipe)))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process)))

(use-package google-translate
  :init
  (setq google-translate-default-target-language "ru"
        google-translate-default-source-language "de")
  :bind (("C-c C-g" . google-translate-at-point)
         ("C-c g" . google-translate-at-point-reverse))
  :config
  (with-eval-after-load 'google-translate-tk
    (defun google-translate--search-tkk () (list 430675 2721866130))))

;; claude.ai integration
(use-package eat :ensure t)
(use-package monet :ensure t
   :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))
(use-package claude-code
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

;; functions
(defun toggle-format-on-save()
  (interactive)
  (setq format-on-save (not format-on-save))
  (message "format on save %s" (if format-on-save "enabled" "disabled")))

(defun action-at-point(action)
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p) (deactivate-mark))
    (funcall action (regexp-quote thing))))

(defun hl-nonzero(sym hl)
  (if (eq sym 0) "0" (propertize (int-to-string sym) 'face hl)))

(defun flymake-diags-count(type)
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type) (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    count))

(defun check-status()
  (if (bound-and-true-p eglot--managed-mode)
      (let ((errors (flymake-diags-count :error))
            (warnings (flymake-diags-count :warning)))
        (concat "  check: "
                (if (or (< 0 warnings) (< 0 errors))
                    (format "%s|%s" (hl-nonzero (or errors 0) 'error) (hl-nonzero (or warnings 0) 'warning))
                  "ok")))
    ""))

(defun format-region(start end)
  (interactive (if (use-region-p) (list (region-beginning) (region-end)) (list (point) (point))))
  (eglot-format start end)
  (back-to-indentation))


;; hooks
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (company-mode)
                            (yas-minor-mode-on)))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key [?\C-x ?d] 'cff-find-other-file)
            (local-set-key (kbd "TAB") 'format-region)
            (abbrev-mode 0)
            (eglot-ensure)))

(add-hook 'java-mode-hook
          (lambda()
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
            (eglot-ensure)))

(add-hook 'python-mode-hook 'eglot-ensure)

(add-hook 'before-save-hook
          (lambda()
            (when format-on-save
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode) (eglot-format-buffer))
              (when (derived-mode-p 'prog-mode) (delete-trailing-whitespace)))))


(put 'check-mode-line 'risky-local-variable t)
(setq check-mode-line '(:eval (check-status)))

(advice-add 'vc-mode-line :after (lambda (&rest args)
  (when (stringp vc-mode)
    (setq vc-mode (concat " " (replace-regexp-in-string (format "^ %s[:-]" (vc-backend buffer-file-name)) " " vc-mode))))))

(add-hook 'window-configuration-change-hook
          (lambda()
            (setq mode-line-format
                  '("%e" mode-line-modified mode-line-buffer-identification
                    "  " mode-name (vc-mode vc-mode)
                    check-mode-line "  " mode-line-position
                    mode-line-end-spaces))
            (set-display-table-slot standard-display-table 5 ?│)))

;; theme
(deftheme okeri)
(custom-theme-set-faces 'okeri
 '(error ((t (:foreground "red"))))
 '(success ((t (:bold t :foreground "#228b22"))))
 '(shadow ((t (:foreground "#767676"))))
 '(highlight ((t (:background "#303030"))))
 '(region ((t (:inherit highlight))))
 '(company-tooltip ((t (:background "05090f" :foreground "#b2b2b2"))))
 '(company-tooltip-selection ((t (:foreground "#e5c07a"))))
 '(company-tooltip-annotation ((t (:foreground "#585858"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-context ((t (:foreground "white"))))
 '(diff-file-header ((t (:bold t :foreground "#585858"))))
 '(diff-header ((t (:foreground "#6c6c6c"))))
 '(diff-removed ((t (:foreground "red"))))
 '(diff-refine-added ((t (:background "#002000"))))
 '(diff-refine-changed ((t (:background "#202020"))))
 '(diff-refine-removed ((t (:background "#200000"))))
 '(font-lock-comment-face ((t (:inherit shadow))))
 '(font-lock-constant-face ((t (:foreground "#af87ff"))))
 '(font-lock-doc-face ((t (:bold t :foreground "#d75f00"))))
 '(font-lock-function-name-face ((t (:foreground "brightwhite"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "#20afff"))))
 '(font-lock-string-face ((t (:foreground "#3cb371"))))
 '(font-lock-preprocessor-face ((t (:foreground "#ff8070"))))
 '(font-lock-variable-name-face ((t (:inherit default))))
 '(font-lock-builtin-face ((t (:foreground "#5f5f87"))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit warning))))
 '(eglot-highlight-symbol-face ((t (:bold t :underline t))))
 '(minibuffer-prompt ((t (:foreground "#00afff"))))
 '(org-table ((t (:inherit font-lock-keyword-face))))
 '(line-number ((t (:foreground "#626262"))))
 '(mode-line ((t (:background "#1a1a1a" :foreground "#767676" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "#121212" :foreground "#444444" :box (:line-width -1 :color "#121212" :style nil))))))
(enable-theme 'okeri)
