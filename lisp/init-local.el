;;; init-local.el --- Load custom modification
;;; Commentary:

;;; Code:
;;; Fonts setup:
(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 165
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :family "PragmataPro"
                    :height 165
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 165
                    :weight 'regular)

;; typography
(setq-default line-spacing nil)
(setq mac-allow-anti-aliasing 1)


;; Better scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;;; Make Emacs look in Stack/Cabal directory for binaries
(let ((my-local-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat my-local-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-local-path))

(let ((my-stack-path (expand-file-name "/usr/local/bin")))
  (setenv "PATH" (concat my-stack-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-stack-path))


;;Copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun arber/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already ther, move to the beginning of the line.
  Effectively toggle between the first non-whitespace charcater and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG -1 lines first.
  If point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Remap C-a to `arber/smarter-move-beginning-of-line`
(global-set-key [remap move-beginning-of-line] 'arber/smarter-move-beginning-of-line)

;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)


;; org mode
(require-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("•"))

(require-package 'easy-kill)
(cua-mode -1)
;; Use easy-kill in place of kill-ring-save
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap whole-line-or-region-kill-ring-save] 'easy-kill)

;; Use easy-mark in place of mark-sexp
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap counsel-apropos] 'apropos-command)


;; Diable lock files
(setq create-lockfiles nil)

;; scala configuration
(require-package 'scala-mode)
(add-to-list 'auto-mode-alist
             '("\.scala" . scala-mode)
             '("\.sbt\'" . scala-mode))

(add-hook 'scala-mode-hook
          (lambda ()
            ;; (show-paren-mode)
            ;; (smartparens-mode)
            ;; (yas-minor-mode)
            ;; (git-gutter-mode)
            ;; (company-mode)
            (scala-mode:goto-start-of-code)))

;; Add some whitespace padding depending on what you
;; type after `(` or `{` parenthesis
(require-package 'smartparens)
(require 'smartparens-config)
(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

;; restrict the smartparens navigation commands
;; to just the brackets (ignoring Emacs’ s-expression
;; interpretation of the Scala language as provided by scala-mode)
;; (defun sp-restrict-c (sym)
;;   "Smartparens restriction on `SYM' for C-derived parenthesis."
;;   (sp-restrict-to-pairs-interactive "{([" sym))
;; end of scala configuration


;; xml configuration
(require 'init-nxml)
(require 'nxml-mode)
(define-key nxml-mode-map (kbd "C-c m f") 'sanityinc/pp-xml-region)
(define-key nxml-mode-map (kbd "C-c m b") 'sanityinc/tidy-buffer-xml)
;;; (define-key nxml-mode-map (kbd "C-M d")   'nxml-down-element)

;; Enable hiding and showing of xml nodes using C-c h short cut
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; Where am i
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path from http://www.emacswiki.org/emacs/NxmlMode."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                 ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))


(add-hook 'which-func-functions 'nxml-where)
;; end of xml configuration

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
  (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
    (dotimes (_ (abs n))
      (delete-indentation (natnump n)))))

(global-set-key (kbd "C-^") 'join-lines)



;; Enable silver searcher for fast search
(require-package 'ag)

;; Proof General is a generic Emacs interface for interactive proof assistants
;; Open .v files with Proof General's Coq
(require-package 'proof-general)

(when (fboundp 'company-coq-initialize)
  (add-hook 'coq-mode-hook #'company-coq-initialize))

;; no splash screen please
(setq proof-splash-seen t)
(setq proof-splash-enable nil)

;;; I don't know who wants to evaluate comments
;;; one-by-one, but I don't.
(setq proof-script-fly-past-comments t)

(with-eval-after-load 'coq
  ;; Proof navigation didn't work for me. So please
  ;; stand aside for my paragraph navigation.
  ;; https://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
  (define-key coq-mode-map (kbd "M-e") nil)
  (define-key coq-mode-map (kbd "M-a") nil)
  )
;;

;; bind super to option key
(setq mac-option-modifier 'super)

(provide 'init-local)
;;; init-local ends here
