;;; flymake-childframe.el --- childframe frontend to display Flymake message -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.1.0
;; Package-requires: ((flymake "1.3.7"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'flymake)

(defgroup flymake-childframe nil
  "Group for customize flymake childframe."
  :group 'flymake
  :prefix "flymake-childframe-")

;; =============
;; customization
;; =============

(defcustom flymake-childframe-delay 1.5
  "Number of seconds before the childframe pops up."
  :group 'flymake-childframe
  :type 'number)

(defcustom flymake-childframe-timeout nil
  "Number of seconds to close the childframe."
  :group 'flymake-childframe
  :type 'integer)

(defcustom flymake-childframe-prefix
  '((note . "i")
    (warning . "?")
    (error . "!"))
  "Prefix to different messages types."
  :type '(alist :key-type symbol :value-type string)
  :group 'flymake-childframe)

(defcustom flymake-childframe-face
  '((note . default)
    (warning . compilation-warning)
    (error . compilation-error))
  "Faces for different messages types."
  :type '(alist :key-type symbol :value-type face)
  :group 'flymake-childframe)

(defcustom flymake-childframe-message-types
  '(((:note eglot-note) . note)
    ((:warning eglot-warning) . warning)
    ((:error eglot-error) . error))
  "Maps of flymake diagnostic types to message types."
  :type '(alist :key-type (repeat symbol) :value-type face)
  :group 'flymake-childframe)

(defcustom flymake-childframe-hide-childframe-hooks
  '(pre-command-hook post-command-hook focus-out-hook)
  "When one of these event happens, hide chlidframe buffer."
  :type '(repeat hook)
  :group 'flymake-childframe)

(defcustom flymake-childframe-show-conditions
  `(,(lambda () (null (evil-insert-state-p))))
  "Conditions under which `flymake-childframe' should pop error message.
Each element should be a function that takes no argument and return a boolean value."
  :type '(repeat function)
  :group 'flymake-childframe)

;; ==================
;; internal variables
;; ==================

(defconst flymake-childframe--buffer " *flymake-childframe-buffer*"
  "Buffer to store linter information.")

(defvar flymake-childframe--frame nil
  "Frame to display linter information.")

(defvar-local flymake-childframe--error-pos 0
  "The cursor position at which the error(s) are shown.
`flymake-childframe' will hide the childframe if `point' is different than this.")

(defvar flymake-childframe--timer nil
  "Timer object for the scheduled childframe show (from `run-at-time').")

(defconst flymake-childframe--init-parameters
  '((left . -1)
    (top . -1)
    (width  . 0)
    (height  . 0)

    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . 0)
    (min-height . 0)
    (child-frame-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (skip-taskbar . t)
    (minibuffer . nil))
  "The initial frame parameters for `flymake-childframe--frame'.")

;; ==========
;; minor mode
;; ==========

;;;###autoload
(define-minor-mode flymake-childframe-mode
  "A minor mode to display flymake error message in a childframe."
  :lighter nil
  :group flymake-childframe
  (if flymake-childframe-mode
      (add-hook 'post-command-hook #'flymake-childframe-show nil 'local)
    (remove-hook 'post-command-hook #'flymake-childframe-show 'local)
    ;; Cancel any pending timer when disabling the mode
    (when (timerp flymake-childframe--timer)
      (cancel-timer flymake-childframe--timer)
      (setq flymake-childframe--timer nil))))

(defun flymake-childframe-show ()
  "Show error information delaying for `flymake-childframe-delay' second."
  ;; Cancel previously scheduled timer (if any) to avoid multiple pending timers.
  (when (timerp flymake-childframe--timer)
    (cancel-timer flymake-childframe--timer)
    (setq flymake-childframe--timer nil))
  (let ((pos (point)))
    (setq flymake-childframe--timer
          (run-at-time flymake-childframe-delay nil
                       (lambda ()
                         ;; clear the timer var as it's firing now
                         (setq flymake-childframe--timer nil)
                         ;; Only show if point hasn't moved since the timer was scheduled.
                         (when (eq pos (point))
                           (flymake-childframe--show)))))))

(defun flymake-childframe-hide ()
  "Hide error information.  Only need to run once.  Once run, remove itself from the hooks."
  ;; if move cursor, hide childframe
  (unless (eq (point) flymake-childframe--error-pos)
    ;; cancel any pending show timer
    (when (timerp flymake-childframe--timer)
      (cancel-timer flymake-childframe--timer)
      (setq flymake-childframe--timer nil))

    (make-frame-invisible flymake-childframe--frame)

    ;; remove hook
    (dolist (hook flymake-childframe-hide-childframe-hooks)
      (remove-hook hook #'flymake-childframe-hide))))

;; =================
;; display mechanism
;; =================

(defun flymake-childframe--show ()
  "Show error information at point."
  ;; ensure any timer state is cleared (we may be invoked directly or from timer)
  (when (timerp flymake-childframe--timer)
    (cancel-timer flymake-childframe--timer)
    (setq flymake-childframe--timer nil))
  (let* ((error-list (flymake-childframe--get-error))
         (main-frame (selected-frame)))
    (when (and error-list
               (run-hook-with-args-until-failure 'flymake-childframe-show-conditions))

      ;; First update buffer information
      (with-current-buffer (get-buffer-create flymake-childframe--buffer)
        (unless (eq major-mode 'flymake-childframe-buffer-mode)
          (flymake-childframe-buffer-mode))
        (erase-buffer)
        (insert (flymake-childframe--format-info error-list))
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))

      ;; Then create frame if needed
      (unless (and flymake-childframe--frame (frame-live-p flymake-childframe--frame))
        (setq flymake-childframe--frame (make-frame flymake-childframe--init-parameters))
        (set-face-background 'child-frame-border (face-foreground 'default) flymake-childframe--frame))

      (with-selected-frame flymake-childframe--frame
        (delete-other-windows)
        (switch-to-buffer flymake-childframe--buffer))

      ;; move frame to desirable position
      (apply 'set-frame-size
             `(,flymake-childframe--frame ,@(flymake-childframe--set-frame-size)))
      (apply 'set-frame-position
             `(,flymake-childframe--frame ,@(flymake-chlidframe--set-frame-position)))
      (set-frame-parameter flymake-childframe--frame 'parent-frame main-frame)

      (redirect-frame-focus flymake-childframe--frame
                            (frame-parent flymake-childframe--frame))

      ;; update position info
      (setq-local flymake-childframe--error-pos (point))

      ;; setup remove hook
      (dolist (hook flymake-childframe-hide-childframe-hooks)
        (add-hook hook #'flymake-childframe-hide))

      ;; finally show frame
      (make-frame-visible flymake-childframe--frame))))

(define-derived-mode flymake-childframe-buffer-mode fundamental-mode "flymake-childframe"
  "Major mode to display the `flymake-childframe' buffer.")

(defun flymake-childframe--set-frame-size ()
  "Set `flymake-chldframe--frame' size based on `flymake-childframe--buffer'."
  (let ((max-width (/ (frame-width (frame-parent flymake-childframe--frame)) 2))
        (height 0)
        (width 0))

    (with-current-buffer flymake-childframe--buffer
      (dolist (error-msg (split-string (buffer-string) "\n"))
        (let ((current-width (length error-msg)))

          ;; if the current message is too long
          (when (> current-width max-width)
            (setq current-width max-width
                  height (1+ height)))

          ;; update width and height
          (setq width (max current-width width)
                height (1+ height))))
      `(,(1+ width) ,height))))

(defun flymake-chlidframe--set-frame-position ()
  (pcase-let* ((`(,win-left ,win-top ,win-right ,win-bottom) (window-inside-pixel-edges))
               (`(,cursor-x . ,cursor-y) (posn-x-y (posn-at-point)))
               (parent-frame (frame-parent flymake-childframe--frame))
               (char-width (frame-char-width parent-frame))
               (char-height (frame-char-height parent-frame))
               (cursor-left (+ win-left cursor-x))
               (cursor-top (+ win-top cursor-y))
               (cursor-right (+ cursor-left char-width))
               (cursor-bottom (+ cursor-top char-height))

               (frame-width (frame-pixel-width flymake-childframe--frame))
               (frame-height (frame-pixel-height flymake-childframe--frame))

               (fits-right (<= (+ cursor-right frame-width) win-right))
               (fits-bottom (<= (+ cursor-bottom frame-height) win-bottom)))

    (cond ((and fits-right fits-bottom) (list cursor-right cursor-bottom))
          ((and fits-right (not fits-bottom)) (list cursor-right (- cursor-top frame-height)))
          ((and fits-bottom (not fits-right)) (list (- cursor-left frame-width) cursor-bottom))
          (t (list (- cursor-left frame-width) (- cursor-top frame-height))))))

;; ==============================
;; get information from `flymake'
;; ==============================

(defun flymake-childframe--get-error (&optional beg end)
  "Get `flymake--diag' between BEG and END, if they are not provided, use `line-beginning-position' and `line-end-position'.  Return a list of errors found between BEG and END."
  (let* ((beg (or beg (save-excursion (beginning-of-visual-line) (point))))
         (end (or end (save-excursion (end-of-visual-line) (point))))
         (error-list (flymake-diagnostics beg end)))
    error-list))

(defun flymake-childframe--get-message-type (type property)
  "Get PROPERTY of flymake diagnostic type TYPE.  PROPERTY can be 'face or 'prefix."
  (let ((key (seq-some
              (lambda (cell)
                (when (memq type (car cell))
                  (cdr cell)))
              flymake-childframe-message-types)))
    (alist-get key (symbol-value
                    (intern (format "flymake-childframe-%s" (symbol-name property)))))))

(defun flymake-childframe--format-one (err)
  "Format ERR for display."
  (let* ((type (flymake-diagnostic-type err))
         (text (flymake-diagnostic-text err))
         (prefix (flymake-childframe--get-message-type type 'prefix))
         (face (flymake-childframe--get-message-type type 'face)))
    (concat (propertize (format "[%s]" prefix) 'face face) " " text)))

(defun flymake-childframe--format-info (error-list)
  "Format the information from ERROR-LIST."
  (thread-last error-list
               (mapcar #'flymake-childframe--format-one)
               (cl-reduce (lambda (a b) (format "%s\n%s" a b)))))

(provide 'flymake-childframe)
;;; flymake-childframe.el ends here
