(setq window-min-height 12)

;; Define temporary buffers by hand
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
;; Used in buffer in which to display temporary buffers
(setq temp-window-location (nth 1 (window-list)))
;; Tells emacs to use display-temporary-buffer to open previously defined
;; special buffers
(setq special-display-function 'display-temporary-buffer)
