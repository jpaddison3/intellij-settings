(defun maximize-window-height ()
  "Give user a function to maximize height of window, allowing more efficient
  viewing of files, window-min-height prevents other window from being too
  squished"
  (interactive)
  (enlarge-window (frame-height)))

;; ;; Split the windows at startup
;; (progn
;;   (interactive)
;;   (split-window-horizontally)
;;   (split-window-vertically)
;;   (other-window 2)
;;   (split-window-vertically)
;;   (other-window 1)
;;   (shell)
;;   (maximize-window-height)
;;   (other-window -3)
;;   (maximize-window-height)
;;   )

(defun display-temporary-buffer (buffer &optional data)
  "Open temporary buffers in a dedicated window split"
  (let ((window temp-window-location))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
