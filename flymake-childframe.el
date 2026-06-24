;;; flymake-childframe.el --- childframe frontend to display Flymake message -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.2.0
;; Package-requires: ((emacs "31") (flymake "1.3.7"))

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
(require 'cl-lib)
(require 'seq)

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
  "When one of these event happens, hide chlidframe buffer."
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

(defvar flymake-childframe--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (internal-border-width . 1)
    (child-frame-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (fullscreen . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "The initial frame parameters for `flymake-childframe--frame'.")

(defvar flymake-childframe--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil)
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . 0)
    (right-fringe-width . 0)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (fringe-indicator-alist (continuation) (truncation))
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil))
  "Default child frame buffer parameters.")

(defvar flymake-childframe--gtk-resize-child-frames
  (let ((case-fold-search t))
    (and (string-match-p "gtk3" system-configuration-features)
         (string-match-p "gnome\\|cinnamon"
                         (or (getenv "XDG_CURRENT_DESKTOP")
                             (getenv "DESKTOP_SESSION") ""))
         'resize-mode)))

(defvar x-gtk-resize-child-frames)
(defvar x-fast-protocol-requests)

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
      (setq flymake-childframe--timer nil))
    (flymake-childframe-hide t)))

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

(defun flymake-childframe-hide (&optional force)
  "Hide error information.  Only need to run once.  Once run, remove itself from the hooks."
  ;; if move cursor, hide childframe
  (when (or force (not (eq (point) flymake-childframe--error-pos)))
    ;; cancel any pending show timer
    (when (timerp flymake-childframe--timer)
      (cancel-timer flymake-childframe--timer)
      (setq flymake-childframe--timer nil))

    (flymake-childframe--hide-frame flymake-childframe--frame)

    ;; remove hook
    (dolist (hook flymake-childframe-hide-childframe-hooks)
      (remove-hook hook #'flymake-childframe-hide 'local))))

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

      (with-current-buffer (flymake-childframe--make-buffer)
        (erase-buffer)
        (insert (flymake-childframe--format-info error-list))
        (pcase-let* ((`(,width . ,height) (flymake-childframe--frame-size))
                     (`(,x . ,y) (flymake-childframe--frame-position width height)))
          (setq flymake-childframe--frame
                (flymake-childframe--make-frame
                 flymake-childframe--frame main-frame x y width height))))

      ;; update position info
      (setq-local flymake-childframe--error-pos (point))

      ;; setup remove hook
      (dolist (hook flymake-childframe-hide-childframe-hooks)
        (add-hook hook #'flymake-childframe-hide nil 'local))

      (make-frame-visible flymake-childframe--frame))))

(define-derived-mode flymake-childframe-buffer-mode fundamental-mode "flymake-childframe"
  "Major mode to display the `flymake-childframe' buffer.")

(defun flymake-childframe--make-buffer ()
  "Create and initialize the child frame buffer."
  (let ((buffer (get-buffer-create flymake-childframe--buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'flymake-childframe-buffer-mode)
        (flymake-childframe-buffer-mode))
      (dolist (var flymake-childframe--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      buffer)))

(defun flymake-childframe--frame-size ()
  "Return popup size in pixels based on `flymake-childframe--buffer'."
  (let ((max-width (/ (frame-width (window-frame)) 2))
        (height 0)
        (width 0)
        (char-width (frame-char-width (window-frame)))
        (char-height (frame-char-height (window-frame))))
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
      (cons (* (max 1 (1+ width)) char-width)
            (* (max 1 height) char-height)))))

(defun flymake-childframe--frame-position (frame-width frame-height)
  "Return popup position for FRAME-WIDTH and FRAME-HEIGHT."
  (pcase-let* ((`(,win-left ,win-top ,win-right ,win-bottom) (window-inside-pixel-edges))
               (`(,cursor-x . ,cursor-y) (posn-x-y (posn-at-point)))
               (parent-frame (window-frame))
               (char-width (frame-char-width parent-frame))
               (char-height (frame-char-height parent-frame))
               (cursor-left (+ win-left cursor-x))
               (cursor-top (+ win-top cursor-y))
               (cursor-right (+ cursor-left char-width))
               (cursor-bottom (+ cursor-top char-height))

               (fits-right (<= (+ cursor-right frame-width) win-right))
               (fits-bottom (<= (+ cursor-bottom frame-height) win-bottom)))

    (cond ((and fits-right fits-bottom) (cons cursor-right cursor-bottom))
          ((and fits-right (not fits-bottom)) (cons cursor-right (- cursor-top frame-height)))
          ((and fits-bottom (not fits-right)) (cons (- cursor-left frame-width) cursor-bottom))
          (t (cons (- cursor-left frame-width) (- cursor-top frame-height))))))

(defun flymake-childframe--make-frame (frame parent x y width height)
  "Show current buffer in child FRAME at X/Y with WIDTH/HEIGHT pixels."
  (when-let* (((frame-live-p frame))
              (timer (frame-parameter frame 'flymake-childframe--hide-timer)))
    (cancel-timer timer)
    (set-frame-parameter frame 'flymake-childframe--hide-timer nil))
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         (x-fast-protocol-requests t)
         (x-gtk-resize-child-frames flymake-childframe--gtk-resize-child-frames)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (graphic (display-graphic-p parent))
         (params `((background-color . ,(face-background 'default nil 'default))
                   (font . ,(frame-parameter parent 'font))
                   (right-fringe . ,right-fringe-width)
                   (left-fringe . ,left-fringe-width)
                   ,@flymake-childframe--frame-parameters)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame) parent)
                 (eq graphic (display-graphic-p frame))
                 (window-live-p (frame-root-window frame)))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((name . ,(if graphic "FlymakeChildframeGUI" "FlymakeChildframeTTY"))
                     (parent-frame . ,parent)
                     (minibuffer . ,(minibuffer-window parent))
                     (width . 0)
                     (height . 0)
                     (visibility . nil)
                     ,@params))))
    (let ((new (face-foreground 'default nil 'default)))
      (unless (equal (face-attribute 'internal-border :background frame 'default) new)
        (set-face-background 'internal-border new frame))
      (unless (equal (face-attribute 'child-frame-border :background frame 'default) new)
        (set-face-background 'child-frame-border new frame)))
    (let* ((win (frame-root-window frame))
           (is (frame-parameters frame))
           (diff (cl-loop for p in params for (k . v) = p
                          unless (equal (alist-get k is) v) collect p)))
      (when diff (modify-frame-parameters frame diff))
      (when (or diff (not (eq (window-buffer win) (current-buffer))))
        (set-window-buffer win (current-buffer)))
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      (set-window-dedicated-p win t))
    (redirect-frame-focus frame parent)
    (pcase-let ((`(,px . ,py) (frame-position frame)))
      (cond
       ((and (= x px) (= y py)) (set-frame-size frame width height t))
       ((fboundp 'set-frame-size-and-position-pixelwise)
        (set-frame-size-and-position-pixelwise frame width height x y))
       (t (set-frame-size frame width height t)
          (set-frame-position frame x y)))))
  frame)

(defun flymake-childframe--hide-frame-deferred (frame)
  "Hide child FRAME and clear its buffer."
  (when (and (frame-live-p frame) (frame-visible-p frame))
    (set-frame-parameter frame 'flymake-childframe--hide-timer nil)
    (make-frame-invisible frame)
    (when-let* ((win (frame-root-window frame)))
      (with-current-buffer (window-buffer win)
        (with-silent-modifications
          (delete-region (point-min) (point-max)))))))

(defun flymake-childframe--hide-frame (frame)
  "Hide child FRAME."
  (when (and (frame-live-p frame) (frame-visible-p frame))
    (cond
     ((not (display-graphic-p frame))
      (flymake-childframe--hide-frame-deferred frame))
     ((not (frame-parameter frame 'flymake-childframe--hide-timer))
      (set-frame-parameter
       frame 'flymake-childframe--hide-timer
       (run-at-time 0 nil #'flymake-childframe--hide-frame-deferred frame))))))

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
