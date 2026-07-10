; -*-Lisp-*-
;; Author: Matt Kolb <kolb722@gmail.com>
;; Used with: Emacs 28.2 on Ubuntu Linux Ubuntu 22.04.2 LTS

;; <packages>
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
;; </packages>

;; <programming>
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(use-package company :ensure t)
(use-package flycheck :ensure t)

;; <lsp-mode>
(use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           (typescript-mode . lsp)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)


;; </lsp-mode>
(require 'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp)

(use-package prettier-js
    :ensure t
    :hook (typescript-mode . prettier-js-mode))
;; </programming>

;; <text>
(add-hook 'text-mode-hook
	  'flyspell-mode)
;; </text>

;; <org>
(setq org-log-done 'time)
;; </org>

;; <appearance>
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Intel One Mono-16" )
(set-frame-font "Intel One Mono-16" nil t)



;; <dracula>
;; Don't change the font size for some headings and titles (default t)
(setq dracula-enlarge-headings nil)

;; Adjust font size of titles level 1 (default 1.3)
(setq dracula-height-title-1 1.25)

;; Adjust font size of titles level 2 (default 1.1)
(setq dracula-height-title-2 1.15)

;; Adjust font size of titles level 3 (default 1.0)
(setq dracula-height-title-3 1.05)

;; Adjust font size of document titles (default 1.44)
(setq dracula-height-doc-title 1.4)

;; Use less pink and bold on the mode-line and minibuffer (default nil)
(setq dracula-alternate-mode-line-and-minibuffer t)

(load-theme 'dracula t)
;; </dracula>

;; <ligatures>
;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes                                                           
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)
;; </ligatures>
;; </appearance>

;; <eshell>
(setq eshell-cmpl-cycle-completions nil)
;(add-to-list 'exec-path "/home/mak/node/bin/")
;; </eshell>

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(impatient-showdown impatient-mode uuidgen yaml-mode flycheck-yamllint flymake-yaml flymake-yamllint yaml exec-path-from-shell prettier-js web-mode yasnippet typescript-mode company go-complete flymake-go magit dracula-theme go-eldoc go-mode rainbow-delimiters)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
