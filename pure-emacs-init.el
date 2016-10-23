;;
;;  Emacs configuration
;;  JP Addison and a bunch of helpful internet folks
;;

;;;; ---- Packages ---- ;;;;
;; Add melpa package manager
(require 'package)
;; Milkbox appears faster and more reliable
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
;; Initialize
(package-initialize)
(package-refresh-contents)
;; Uncomment and use this to install packages used in this file
(defvar usedPackages
  '(better-defaults
    monokai-theme
    xah-elisp-mode
    rainbow-delimiters
    elpy
    py-autopep8
    yaml-mode
    multiple-cursors
    ac-octave
    ess
    restclient
    elixir-mode
    alchemist
    magit
    go-mode
    go-eldoc
    markdown-mode))
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      usedPackages)

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
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; C-u kills to beginning of line like a shell
(defun kill-backwords-or-del ()
  ( if (eq (current-column) 0)
      (delete-backward-char 1)
    (progn (kill-line 0))))
(global-set-key (kbd "C-u") '(lambda () (interactive) (kill-backwords-or-del)))

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

;; 100 Character Line
(add-to-list 'load-path (expand-file-name "~/.emacs.d/Fill-Column-Indicator"))
(require 'fill-column-indicator)
(defun custom-fci ()
  (setq fci-rule-color "gray24")
  (fci-mode)
  (set-fill-column 100))
;; Get fci-mode to play well with company-mode
;; Note that this turns on fci mode when company mode is on
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))
(advice-add 'company-call-frontends :before #'on-off-fci-before-company)
;; keyboard shortcut
(global-set-key (kbd "C-c f") 'fci-mode)

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;;; ---- ELisp ---- ;;;;
;; Better syntax highlighting for elisp
(require 'xah-elisp-mode)
;; Use rainbow colors to quickly see matching parens everywhere
(require 'rainbow-delimiters)
(add-hook 'xah-elisp-mode-hook 'rainbow-delimiters-mode)
;; 100 character line
(add-hook 'xah-elisp-mode-hook 'custom-fci)

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
;; 100 character line
(add-hook 'python-mode-hook 'custom-fci)
;; Enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; First level of aggressive is still pretty safe
(setq py-autopep8-options '("--aggressive" "--max-line-length=100"))

;; Get company mode to use the fuzzy matching in flx - Doesn't work
;; (with-eval-after-load 'company
;;   (company-flx-mode +1))

;;;; ---- Yaml ---- ;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;;;; ---- Org-Mode ---- ;;;;
(require 'org)
(define-key global-map (kbd "C-c g") 'org-agenda)
;; Record time when finished
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "WAIT" "DONE")))
(setq org-startup-truncated nil)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/hand-installed"))
(require 'ox-confluence)
(eval-after-load "org"
  '(require 'ox-md nil t))

;;;; ---- Matlab/Octave ---- ;;;;
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Octave autocomplete
(require 'ac-octave)
(defun ac-octave-mode-setup ()
  (setq ac-sources '(ac-source-octave)))
  (add-hook 'octave-mode-hook
    '(lambda () (ac-octave-mode-setup)))

;;;; ---- R ---- ;;;;
(require 'ess-site)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))

;;;; ---- Elixir ---- ;;;;
(require 'elixir-mode)
(require 'alchemist)
(add-hook 'elixir-mode-hook
  (lambda ()
    (setq company-backends '(alchemist-company))))
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook 'custom-fci)
(add-hook 'elixir-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))
;; iex mode
(add-hook 'alchemist-iex-mode-hook 'company-mode)
(add-hook 'alchemist-iex-mode-hook
  (lambda ()
    (setq company-backends '(alchemist-company))))
(add-hook 'alchemist-iex-mode-hook 'custom-fci)

;;;; ---- Go ---- ;;;;
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq tab-width 2)
  ; Call Gofmt before saving
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  ; autocomplete
  (auto-complete-mode 1)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && go install"))
  (setq compilation-read-command nil)
  (local-set-key (kbd "C-c c") 'compile)

  ; Go Oracle for introspection
  ;; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  )
(add-hook 'go-mode-hook 'go-mode-setup)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;;;; ---- Rest Client Mode ---- ;;;;
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.restcli\\'" . restclient-mode))

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
  (maximize-window-height)
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
        "^.*magit:.*$"
        "^.*magit-.*-popup.*$"
        "^.Process List.$"
        "^.Elpy Refactor.$"
        "^.Python Doc.$"
        "^.HTTP Response.$"
        "^.Python Check.$"
        "^.org .* Export.$"
        "^.alchemist test report.$"
        "^.alchemist mix.$"
        "^.alchemist help.$"
        "^.alchemist elixir.$"
        "^.alchemist elixirc.$"
        "^.Gofmt Errors.$"
        ))
(setq temp-window-second (nth 1 (window-list)))
(defun grb-special-display (buffer &optional data)
    (let ((window temp-window-second))
      (with-selected-window window
        (switch-to-buffer buffer)
        window)))
(setq special-display-function 'grb-special-display)
