;;
;;  Emacs configuration
;;  JP Addison and a bunch of helpful internet folks
;;

;;;; ---- Packages ---- ;;;;
;; Add melpa package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Initialize
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; Uncomment and use this to install packages used in this file
;; (defvar usedPackages
;;   '(better-defaults
;;     monokai-theme
;;     xah-elisp-mode
;;     rainbow-delimiters
;;     elpy
;;     py-autopep8
;;     yaml-mode
;;     multiple-cursors))
;; (mapc #'(lambda (package)
;;     (unless (package-installed-p package)
;;       (package-install package)))
;;       usedPackages)

;;;; ---- Basic settings ---- ;;;;
;; Set better defaults for a lot of emacs functions
(require 'better-defaults)
;; Better color theme
(load-theme 'monokai t)

;; GRB: Don't show the startup screen
(setq inhibit-startup-message t)
;; Shut up compile saves
(setq compilation-ask-about-save nil)
;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))
;; No auto-saves
(setq auto-save-default nil)

;; Fix ansi escapes, not working
;; (setq ansi-color-for-comint-mode t)

;; C-u kills to beginning of line like a shell
(global-set-key "\C-u" '(lambda () (interactive) (kill-line 0)))

;; Change word boundary definition
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; Kill yank from system clipboard
(defun paste-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(if (eq system-type 'darwin)
    (progn (setq interprogram-cut-function 'copy-to-osx)
           (setq interprogram-paste-function 'paste-from-osx)
           ))

;; Flexible comment / uncomment 
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
(global-set-key (kbd "M-/") 'comment-or-uncomment-line-or-region)

;; Mark word under cursor with M-@
(defun better-mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))
(global-set-key (kbd "M-@") 'better-mark-current-word)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c x") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

;;;; ---- ELisp ---- ;;;;
;; Better syntax highlighting for elisp
(require 'xah-elisp-mode)
(xah-elisp-mode)
;; Use rainbow colors to quickly see matching parens everywhere
(require 'rainbow-delimiters)
(add-hook 'xah-elisp-mode-hook 'rainbow-delimiters-mode)
(rainbow-delimiters-mode 1)

;;;; --- Python ---- ;;;;
;; python ide
(elpy-enable)
;; iPython shell is fantastic, use it as default not working cause ansi crap
;; (elpy-use-ipython)
;; Run tests using nosetests
(defun elpy-test-nose-runner-chdir-up (top file module test)
  "Test the project using the nose test runner after moving up a directory
This requires the nose package to be installed.
JPA: The cd .. is pretty hackish, TODO emulate vvvv and find out why top variable
doesn't set working directory
https://github.com/jorgenschaefer/elpy/blob/master/elpy.el#L2068"
  (interactive (elpy-test-at-point))
  (setq test-command
        (format "cd ../ && nosetests tests/%s"
                 (if module
                     (format "%s.py%s"
                             module
                             (if test (format ":%s" test) ""))
                   "")))
    (compile test-command))
(put 'elpy-test-nose-runner-chdir-up 'elpy-test-runner-p t)
(elpy-set-test-runner 'elpy-test-nose-runner-chdir-up)
;; Enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; (setq py-autopep8-options '("--max-line-length=100"))
;; First level of aggressive is still pretty safe
(setq py-autopep8-options '("--aggressive" "--max-line-length=100"))

;; 100 character line
(add-to-list 'load-path (expand-file-name "~/.emacs.d/Fill-Column-Indicator"))
(require 'fill-column-indicator)
(add-hook 'python-mode-hook (lambda () (set-fill-column 100)))
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook (lambda ()
             (setq fci-rule-color "gray24")
             ))

;;;; ---- Yaml ---- ;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;; ---- Org-Mode ---- ;;;;
(require 'org)
(define-key global-map (kbd "C-c g") 'org-agenda)
;; Record time when finished
(setq org-log-done t)

;;;; ---- Matlab/Octave ---- ;;;;
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;; ---- Windows ---- ;;;;
;; Many things in this section come from gary bernharts dotfiles github repo
;; C-o switches to other window (same as C-x o)
(global-set-key (kbd "C-o") 'other-window)
;; M-o switches back to previous window
(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key "\M-o" 'prev-window)
;; Function to set keys for overriding modes
(defun set-window-kbds ()
       (local-set-key (kbd "C-o") 'other-window)
       (local-set-key (kbd "M-o") 'prev-window))
;; Compilation and Ibuffer modes overrode C-o and M-o
(add-hook 'compilation-mode-hook 'set-window-kbds)
(add-hook 'ibuffer-mode-hook 'set-window-kbds)

;; Windows can resize to max but leaving room for neighbor
(setq window-min-height 12)
(defun maximize-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (frame-height)))
(global-set-key (kbd "C-c v") 'maximize-window-height)

;; Split the windows at startup
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

;; Open temporary buffers in a dedicated window split
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
        "^.*magit: .*$"
        "^.Process List.$"
        "^.Elpy Refactor.$"
        "^.Python Doc.$"))
(setq grb-temporary-window (nth 1 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function 'grb-special-display)

