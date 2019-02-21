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
(provide 'init-local)

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

;;; Themes
;;(require-package 'doom-themes)
;;(load-theme 'doom-tomorrow-night t)
;; Corrects (and improves) org-mode's native fontification.
;;(doom-themes-org-config)



;;; init-local ends here
