;;; init-coq.el --- Support Coq development -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'init-coq)
