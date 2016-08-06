;; GRB: Don't show the startup screen
(setq inhibit-startup-message t)
;; Shut up compile saves
(setq compilation-ask-about-save nil)
;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))
;; C-u kills to beginning of line like a shell

(global-set-key "\C-u" '(lambda () (interactive) (kill-line 0)))
;; kill yank from system clipboard
(defun paste-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'copy-to-osx)
(setq interprogram-paste-function 'paste-from-osx)

;; Add melpa package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; add dependcy location
(add-to-list 'load-path (expand-file-name "~/.emacs.d/Fill-Column-Indicator"))

;; Set better defaults for a lot of emacs functions
(require 'better-defaults)
;; Better color theme
(load-theme 'monokai t)

;; better syntax highlighting for elisp
(require 'xah-elisp-mode)
(xah-elisp-mode)
;; Lighten parenthesis for less distraction
(require 'paren-face)
(add-hook 'xah-elisp-mode-hook 'paren-face-mode)
;; Until I figure out the colors rainbow delimiters displays better, I prefer paren-face
;; (require 'rainbow-delimiters)
;; (add-hook 'xah-elisp-mode-hook 'rainbow-delimiters-mode)

;; python autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
;; 100 character line
(require 'fill-column-indicator)
(add-hook 'python-mode-hook (lambda () (set-fill-column 100)))
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook (lambda ()
             (setq fci-rule-color "gray24")
             ))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
(global-set-key (kbd "M-/") 'comment-or-uncomment-line-or-region)

;; Many things below this point come from gary bernharts dotfiles github repo

;;
(setq window-min-height 12)
(defun maximize-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (frame-height)))
(global-set-key (kbd "C-c v") 'maximize-window-height)

;; GRB: split the windows
(progn
  (interactive)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 2)
  (split-window-vertically)
  (other-window 1)
  (shell)
  (other-window -3)
  (maximize-window-height)
  )

;; C-o switches to other window (same as C-x o)
(global-set-key (kbd "C-o") 'other-window)
;; M-o switches back to previous window
(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key "\M-o" 'prev-window)

;; ;; open temporary buffers in a dedicated window split
(setq special-display-regexps
      '("^\\*Completions\\*$"
        "^\\*Help\\*$"
        "^\\*grep\\*$"
        "^\\*Apropos\\*$"
        "^\\*elisp macroexpansion\\*$"
        "^\\*local variables\\*$"
        "^\\*Compile-Log\\*$"
        "^\\*Quail Completions\\*$"
        "^\\*Occur\\*$"
        "^\\*frequencies\\*$"
        "^\\*compilation\\*$"
        "^\\*Locate\\*$"
        "^\\*Colors\\*$"
        "^\\*tumme-display-image\\*$"
        "^\\*SLIME Description\\*$"
        "^\\*.* output\\*$"           ; tex compilation buffer
        "^\\*TeX Help\\*$"
        "^\\*Shell Command Output\\*$"
        "^\\*Async Shell Command\\*$"
        "^\\*Backtrace\\*$"
        "^\\*Python check: .*$"
        "^.*magit: .*$"))
(setq grb-temporary-window (nth 1 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function #'grb-special-display)
