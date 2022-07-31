
;; Use Lexical Binding
;; -*- lexical-binding: t; -*-

(defun my/revert-buffer ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun my/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun my/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (my/kill-thing-at-point 'word))

(defun my/kill-sentence-at-point ()
  "Kill the sentence at point."
  (interactive)
  (my/kill-thing-at-point 'sentence))

(defun my/scroll-half-page-down ()
        "scroll down half the page"
        (interactive)
        (scroll-down (/ (window-body-height) 2)))

(defun my/scroll-half-page-up ()
        "scroll up half the page"
        (interactive)
        (scroll-up (/ (window-body-height) 2)))

(defun my/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end) (point))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my/toggle-highlight-trailing-whitespaces ()
  "Function toggles highlighting trailing whitespaces"
  (interactive)
  (if (bound-and-true-p show-trailing-whitespace)
      (progn  (message "Disable highlighting of trailing whitespaces")
              (setq-default show-trailing-whitespace nil))
  (progn (message "Enable highlighting of trailing whitespaces")
         (setq-default show-trailing-whitespace t))))

(defun my/toggle-idle-highlight-mode ()
  "Function toggles 'idle-highlight-mode'"
  (interactive)
  (if (bound-and-true-p dle-highlight-mode)
      (progn  (message "Disable 'idle-highlight-mode'")
              (setq-default idle-highlight-mode nil))
  (progn (message "Enable 'idle-highlight-mode'")
         (setq-default idle-highlight-mode t))))

(defun my/which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


(defun my/untabify-entire-buffer ()
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (message "Converting all TAB's to spaces")
  (keyboard-quit))

(defun my/open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (message "Init file opened"))

(defun my/open-custom-functions-file ()
  (interactive)
  (find-file "~/.emacs.d/custom/custom-functions.el")
  (message "Custom functions file opened"))

;; Function copied from Emacs Wiki (https://www.emacswiki.org/emacs/KillingBuffers)
(defun my/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; Function copied from Emacs Wiki (https://www.emacswiki.org/emacs/KillingBuffers)
(defun my/close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun my/other-window-kill-buffer ()
  "Function woks when there are multiple windows opened in the current frame.
   Kills the currently opened buffer in all the other windows"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the active buffer."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

;; TODO: prevent function from removing *Messages buffer
;; https://stackoverflow.com/questions/1687620/regex-match-everything-but-specific-pattern
(defun my/kill-asterisk-buffers ()
  "Kill all buffers whose names start with an asterisk (‘*’).
   By convention, those buffers are not associated with files."
  (interactive)
  (kill-matching-buffers "*" nil t)
  (message "All asterisk (*) buffers have been killed"))

(defun my/reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

(defun my/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(provide 'custom-functions)
