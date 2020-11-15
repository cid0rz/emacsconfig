;;; init.el --- Initialization file for Emacs

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs


;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;;GLOBAL CONFIGURATION

;;(electric-pair-mode 1) replaced by smartparens

(setq inhibit-startup-message t)
;; (global-display-line-numbers-mode 1) ;; if you want line numbers
;;(global-set-key "\C-x\C-m" 'execute-extended-command) ;; add binding for M-x
;;(global-set-key "\C-c\C-m" 'execute-extended-command) ;; add binding for M-x
(global-auto-revert-mode t)
(setq vc-follow-symlinks t)
(desktop-save-mode 1)

;; Straight package manager setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package bind-key)
(setq tramp-default-method "ssh")


;; LISP CONFIGURATION

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

;; EXWM configuration

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

(use-package rainbow-delimiters
  
  :hook (prog-mode . rainbow-delimiters-mode))

;; ** PACKAGE LOADING **

(use-package better-defaults
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
     ,@(loop for (key . val) in pairs
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

(use-package elisp-slime-nav
  
  :hook
  (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  (lisp-interaction-mode . turn-on-elisp-slime-nav-mode)
  (ielm-mode . turn-on-elisp-slime-nav-mode))

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

;(use-package pyenv-mode
;  
;  :init
;  (add-to-list 'exec-path "~/.pyenv/shims")
;  (setenv "WORKON_HOME" "~/.pyenv/versions/")
;  :config
;  (pyenv-mode))

;(use-package pyenv-mode-auto
;  )

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package ob-hy )

(use-package org
  :config
  ;;LOAD LANGUAGES FOR CODEBLOCKS
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t) (python . t) (hy . t))))

(use-package elpy
  
  :init
  (elpy-enable)
  ;; backend to jedi for finding definitions
  :custom (elpy-rpc-backend "jedi"))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (require 'company-elisp)
  (push 'company-elisp company-backends))

(use-package company-jedi
  :config
  (push 'company-jedy company-backends))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package magit
  :bind
  (("C-x g" . magit-status))
  (("C-x M-g" . magit-dispatch-popup)))

(use-package py-autopep8
  :hook (elpy-mode py-autopep8-enable-on-save))

(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package docker-compose-mode)

(use-package arduino-mode)

(use-package which-key
  :config (which-key-mode))

(use-package git-timemachine
  :bind ("M-g M-t" . git-timemachine))

(use-package ace-window
  :init
  (setq aw-scope 'global) ;; was frame
  (global-set-key (kbd "C-x O") 'other-frame)
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package lua-mode)

(use-package vterm
  ;:disabled
  )

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-backend "jedi" t)
 '(package-selected-packages
   '(git-timemachine which-key web-mode use-package pyenv-mode-auto py-autopep8 material-theme magit flycheck elpy dockerfile-mode docker-compose-mode better-defaults arduino-mode ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
