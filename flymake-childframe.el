;;; flymake-childframe.el --- childframe frontend to display Flymake message -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.3.0
;; Package-requires: ((emacs "31") (flymake "1.3.7") (posframe "1.5.0"))

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
;;
;; Display `flymake' diagnostics in a childframe popup.  All childframe
;; management (frame creation, sizing, positioning, visibility, TTY
;; support) is delegated to `posframe', which is the upstream
;; recommended library for child frames on both GUI and TTY frames
;; (Emacs 31+ added TTY child frame support; see etc/NEWS).
;;
;; This delegation avoids terminal child frame crashes that arise when a
;; package rolls its own `make-frame' / `set-frame-size-and-position-pixelwise'
;; calls without handling TTY specifics (`tty-child-frames' feature,
;; `tty-non-selected-cursor', TTY border rules, etc.).

;;; Code:

(require 'flymake)
(require 'cl-lib)
(require 'seq)
(require 'posframe)

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
  "Number of seconds to close the childframe.
Maps directly to `posframe-show''s :timeout argument; nil means no timeout."
  :group 'flymake-childframe
  :type '(choice (const :tag "No timeout" nil)
                 integer))

(defcustom flymake-childframe-prefix
  '((note . "[i]")
    (warning . "[?]")
    (error . "[!]"))
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
  "When one of these events happens, hide the childframe."
  :type '(repeat hook)
  :group 'flymake-childframe)

(defcustom flymake-childframe-show-conditions
  `(,(lambda ()
       (let ((fn (and (fboundp 'evil-insert-state-p)
                      (symbol-function 'evil-insert-state-p))))
         (or (not fn) (not (funcall fn))))))
  "Conditions under which `flymake-childframe' should pop error message.
Each element should be a function that takes no argument and return a boolean value."
  :type '(repeat function)
  :group 'flymake-childframe)

(defcustom flymake-childframe-poshandler 'posframe-poshandler-point-bottom-left-corner
  "Poshandler function used to position the childframe.
Any function accepted by `posframe-show''s :poshandler argument.
`posframe-poshandler-point-bottom-left-corner' places the popup below the
cursor and flips upward when there is no room; see also
`posframe-poshandler-point-bottom-left-corner-upward'."
  :type '(choice (const :tag "Below point (flip up)" posframe-poshandler-point-bottom-left-corner)
                 (const :tag "Above point (flip down)" posframe-poshandler-point-bottom-left-corner-upward)
                 (const :tag "Above point, top-left" posframe-poshandler-point-top-left-corner)
                 (function :tag "Custom poshandler"))
  :group 'flymake-childframe)

;; ==================
;; internal variables
;; ==================

(defconst flymake-childframe--buffer " *flymake-childframe-buffer*"
  "Buffer to store linter information.  Managed by `posframe'.")

(defvar flymake-childframe--timer nil
  "Timer object for the scheduled childframe show (from `run-at-time').")

(defvar-local flymake-childframe--error-pos 0
  "The cursor position at which the error(s) are shown.
`flymake-childframe' will hide the childframe if `point' is different than this.")

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
    (when (timerp flymake-childframe--timer)
      (cancel-timer flymake-childframe--timer)
      (setq flymake-childframe--timer nil))
    (flymake-childframe-hide t)
    (posframe-delete flymake-childframe--buffer)))

(defun flymake-childframe-show ()
  "Show error information delaying for `flymake-childframe-delay' second."
  (when (timerp flymake-childframe--timer)
    (cancel-timer flymake-childframe--timer)
    (setq flymake-childframe--timer nil))
  (let ((pos (point)))
    (setq flymake-childframe--timer
          (run-at-time
           flymake-childframe-delay nil
           (lambda ()
             (setq flymake-childframe--timer nil)
             (when (eq pos (point))
               (flymake-childframe--show)))))))

(defun flymake-childframe-hide (&optional force)
  "Hide error information.  Only need to run once.  Once run, remove itself from the hooks."
  (when (or force (not (eq (point) flymake-childframe--error-pos)))
    (when (timerp flymake-childframe--timer)
      (cancel-timer flymake-childframe--timer)
      (setq flymake-childframe--timer nil))
    (posframe-hide flymake-childframe--buffer)
    (dolist (hook flymake-childframe-hide-childframe-hooks)
      (remove-hook hook #'flymake-childframe-hide 'local))))

;; =================
;; display mechanism
;; =================

(defun flymake-childframe--show ()
  "Show error information at point via `posframe'."
  (let ((error-list (flymake-childframe--get-error)))
    (when (and error-list
               (run-hook-with-args-until-failure 'flymake-childframe-show-conditions))
      (posframe-show
       flymake-childframe--buffer
       :string (flymake-childframe--format-info error-list)
       :position (point)
       :poshandler flymake-childframe-poshandler
       :max-width (/ (frame-width) 2)
       :timeout flymake-childframe-timeout
       :internal-border-width 1
       :border-width 1
       :border-color (face-foreground 'default nil 'default)
       :lines-truncate t)
      (setq-local flymake-childframe--error-pos (point))
      (dolist (hook flymake-childframe-hide-childframe-hooks)
        (add-hook hook #'flymake-childframe-hide nil 'local)))))

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
    (concat (propertize (format "%s" prefix) 'face face) " " text)))

(defun flymake-childframe--format-info (error-list)
  "Format the information from ERROR-LIST."
  (thread-last error-list
               (mapcar #'flymake-childframe--format-one)
               (cl-reduce (lambda (a b) (format "%s\n%s" a b)))))

(provide 'flymake-childframe)
;;; flymake-childframe.el ends here