;; init.el --- Emacs configuration

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


;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy
    flycheck
    material-theme
    py-autopep8
    magit
    arduino-mode
    use-package))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;;ORG-MODE

(require 'org)

;;LOAD LANGUAGES FOR CODEBLOCKS

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t) (python . t)))

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(global-set-key "\C-x\C-m" 'execute-extended-command) ;; add binding for M-x 
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; add binding for M-x
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)

;; use flycheck not flymake with available languages

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; backend to jedi for finding definitions
(setq elpy-rpc-backend "jedi")

;; allow easy switching between python envs
;; from conda

(setenv "WORKON_HOME" "/home/cid0rz/miniconda2/envs")


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

;; init.el ends here
