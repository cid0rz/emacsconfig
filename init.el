;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;silence native-comp warnings
(setq native-comp-async-report-warnings-errors nil)

;;use shallow clones
(setq straight-vc-git-default-clone-depth 1)

;;no startup message
(setq inhibit-startup-message t)

;; if you want line numbers
;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; popular bindings
;;(global-set-key "\C-x\C-m" 'execute-extended-command) ;; add binding for M-x
;;(global-set-key "\C-c\C-m" 'execute-extended-command) ;; add binding for M-x

;;see the changes on disk
(global-auto-revert-mode t)

;;follow symlinks to files
(setq vc-follow-symlinks t)
;;save where you were working
(desktop-save-mode 1)
;;make dired show this in a nice way
(setq dired-listing-switches "-alh")

;; use utf-8
(set-default-coding-systems 'utf-8)

;;use ssh for tramp
(setq tramp-default-method "ssh")

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;remove menu-bar and scroll bars
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; remember where you were visiting buffers
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; some better built-in alternatives
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;show matching parenthesis
(show-paren-mode 1)
;;use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "~/.emacs.d/custom.el"))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (use-package bind-key)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Keep M-TAB for `completion-at-point'
            (define-key flyspell-mode-map "\M-\t" nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            (add-hook (make-local-variable 'after-save-hook)
                      (lambda ()
                        (byte-force-recompile default-directory)))
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(use-package slime
  :straight t
  :config (setq inferior-lisp-program "sbcl")
  )

(use-package elisp-slime-nav
  :hook
  (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  (lisp-interaction-mode . turn-on-elisp-slime-nav-mode)
  (ielm-mode . turn-on-elisp-slime-nav-mode))

;; async and await for emacs lisp

(use-package async-await)
(use-package bencode
  :straight '(bencode :host github
                      :repo "skeeto/emacs-bencode"
                      :branch "master")
  )

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package exwm
  :disabled
  
  :config
  (require 'exwm-config)
  (exwm-config-example)
  (require  'exwm-systemtray)
  (exwm-systemtray-enable)
  (display-time-mode 1)
  (setq layout-show-all-buffers t)
  (setq display-time-string-forms
      '((concat 24-hours ":" minutes " " day "/" month "/" year)))
  
  ;;(display-battery-mode 1)
  ;;(setq display-time-string-forms '((format-time-string "%H:%M " now)))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ;;to solve ediff issue
  (setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f]))))

;;     THEMES
(use-package material-theme
  ;:disabled
  :config
  (load-theme 'material t))

;;(setq modus-themes-scale-headings t)
;;(load-theme 'modus-operandi)


(use-package rainbow-delimiters

  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package ace-window
  :init
  (setq aw-scope 'global) ;; was frame
  (global-set-key (kbd "C-x O") 'other-frame)
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode)
)

(use-package smartparens
  
  :config
  (require 'smartparens-config)
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
  conses, where NAME is the function name that will be created and
  STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

  defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
  respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  :bind
  ("C-M-a" . sp-beginning-of-sexp)
  ("C-M-e" . sp-end-of-sexp)

  ("C-<down>" . sp-down-sexp)
  ("C-<up>"   . sp-up-sexp)
  ("M-<down>" . sp-backward-down-sexp)
  ("M-<up>"   . sp-backward-up-sexp)

  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)

  ("C-M-n" . sp-next-sexp)
  ("C-M-p" . sp-previous-sexp)

  ("C-S-f" . sp-forward-symbol)
  ("C-S-b" . sp-backward-symbol)

  ("C-<right>" . sp-forward-slurp-sexp)
  ("M-<right>" . sp-forward-barf-sexp)
  ("C-<left>"  . sp-backward-slurp-sexp)
  ("M-<left>"  . sp-backward-barf-sexp)

  ("C-M-t" . sp-transpose-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("C-k"   . sp-kill-hybrid-sexp)
  ("M-k"   . sp-backward-kill-sexp)
  ("C-M-w" . sp-copy-sexp)
  ("C-M-d" . delete-sexp)

  ("M-<backspace>" . backward-kill-word)
  ("C-<backspace>" . sp-backward-kill-word)
  ([remap sp-backward-kill-word] . backward-kill-word)

  ("M-[" . sp-backward-unwrap-sexp)
  ("M-]" . sp-unwrap-sexp)

  ("C-x C-t" . sp-transpose-hybrid-sexp)

  ("C-c ("  . wrap-with-parens)
  ("C-c ["  . wrap-with-brackets)
  ("C-c {"  . wrap-with-braces)
  ("C-c '"  . wrap-with-single-quotes)
  ("C-c \"" . wrap-with-double-quotes)
  ("C-c _"  . wrap-with-underscores)
  ("C-c `"  . wrap-with-back-quotes)
  :hook (prog-mode . smartparens-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package epa
    :config
    (progn
      (epa-file-enable)
      (setq epa-file-cache-passphrase-for-symmetric-encryption t)))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))


(use-package orderless
                                        ;:repo "oantolin/orderless"
                                        ;:branch "master"
  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (customize-set-variable 'completion-styles '(orderless))
  (customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
  (setq completion-category-defaults nil))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :init
  (vertico-mode)
  )

(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu"
                    :branch "main")
                                        ;:config
                                        ;(setq corfu-auto t)
  :custom
  (completion-styles '(orderless-fast))
  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


(use-package marginalia
  :straight '(marginalia :host github
                         :repo "minad/marginalia"
                         :branch "main")
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :straight '(consult :host github
                      :repo "minad/consult"
                      :branch "main"))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-]" . tempel-next)
         ("M-[" . tempel-previous))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package savehist
  :config
  (setq history-length 40)
  (savehist-mode 1))

(use-package htmlize)

(use-package org
      :after ob-hy
      :config
      (require 'ob-comint)
      ;;LOAD LANGUAGES FOR CODEBLOCKS

      (with-eval-after-load 'org
        (org-babel-do-load-languages
         'org-babel-load-languages '((emacs-lisp . t) (C . t) (python . t) (hy . t) (shell . t) (lisp . t)))
        (require 'org-tempo)

        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
      ))

;;for org-babel to know about hy
(use-package ob-hy
  :straight '(ob-hy   :host github
                      :repo "brantou/ob-hy"
                      :branch "master"))

;;  (setq org-html-htmlize-output-type 'css)

;;git flavoured markdown
(use-package ox-gfm
      :after org
      :config
      (require 'ox-gfm nil t)
      )

(use-package which-key
  :config (which-key-mode))

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :config
  (global-pyenv-mode))

(use-package switch-buffer-functions
  :disabled
  :config
  (defun pyenv-update-on-buffer-switch (prev curr)
    (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
        (pyenv-use-corresponding)))

  (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))

 (use-package elpy
   :straight (:host github :repo "jorgenschaefer/elpy")
   :init
   (elpy-enable)
   :config
   (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
   ;; backend to jedi for finding definitions
   :custom (elpy-rpc-backend "jedi"))


 (use-package p
   y-autopep8
   :disabled
   :hook (elpy-mode py-autopep8-enable-on-save))

(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))

(use-package magit
  :bind
  (("C-x g" . magit-status))
  (("C-x M-g" . magit-dispatch)))

(use-package git-timemachine
  :bind ("M-g M-t" . git-timemachine))
(use-package forge
  :after magit)

(defun ediff-copy-both-to-C ()
  "combine both buffers into the result buffer in order"
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package yaml-mode)

(use-package docker-compose-mode)

(use-package arduino-mode
  :disabled
	     )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vterm)
