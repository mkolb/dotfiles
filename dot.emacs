; -*-Lisp-*-
;; Author: Matt Kolb <kolb722@gmail.com>
;; Used with: Emacs 24.2.1 on Ubuntu Linux 13.04

;; <packages>
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
;; </packages>

;; <programming>
(global-auto-complete-mode t)
(add-hook 'php-mode-hook
	  (lambda()
	    (linum-on)))
(require 'rainbow-delimiters)

(setq python-shell-interpreter "/usr/bin/ipython")
(require 'ipython)

(setq js-indent-level 2)
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))
;; </programming>

;; <text>
(add-hook 'text-mode-hook
	  'flyspell-mode)
;; </text>

;; <org>
(setq org-log-done 'time)
;; </org>

;; <appearance>
(require 'color-theme)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-rainbow-delimiters-mode)
(setq my-font "Droid Sans Mono-12")
(color-theme-solarized-dark)
(set-default-font my-font)
;; </appearance>

;; <eshell>
(setq eshell-cmpl-cycle-completions nil)
;; </eshell>

;; <erc>
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode t)
(setq erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords
      `((freenode (("mkolb" . ,freenode-nickone-pass))))
      erc-email-userid "mak"
      erc-nick "mkolb"
      erc-paranoid t
      erc-port 6667
      erc-prompt-for-password nil
      erc-public-away-p t
      erc-server "irc.freenode.net"
      erc-user-full-name "Matt Kolb")
;; </erc>

;; <functions>
(defun insert-password-salt ()
  "Insert a string of length 31 suitable for a new password salt in moodle."
  (interactive)
  (let ((mycharset "1234567890abcdefghijklmnopqrstyvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()"))
    (dotimes (i 31)
      (insert (elt mycharset (random (length mycharset)))))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
;; </functions>

;; <global set keys>
(global-set-key (kbd "C-c a")
		'(lambda () (interactive) (ansi-term "/bin/zsh")))
;; </global set keys>

;; <customize>
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(flymake-phpcs-standard "moodle")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; </customize>
