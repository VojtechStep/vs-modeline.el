;;; vs-modeline.el --- Pretty modeline    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <me@vojtechstep.eu>
;; URL: https://github.com/VojtechStep/vs-modeline.el
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code is heavily inspired by telephone-line (https://github.com/dbordak/telephone-line/).
;; Its purpose is to be more effective and cater to my minimal needs (no separators, no mouse keymaps).

;;; Code:

(eval-when-compile
  (require 'easy-mmode)
  (require 'cl-lib))

(defvar vs-modeline--old-modeline mode-line-format
  "Initial value of the mode line.")

(defface vs-modeline-evil-base
  '((t :inherit mode-line
       :foreground "white"
       :weight bold))
  "Used as base for rendering of current Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-normal
  '((t :inherit vs-modeline-evil-base
       :background "red3"))
  "Used to indicate Normal Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-insert
  '((t :inherit vs-modeline-evil-base
       :background "forest green"))
  "Used to indicate Insert Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-visual
  '((t :inherit vs-modeline-evil-base
       :background "dark orange"))
  "Used to indicate Visual Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-replace
  '((t :inherit vs-modeline-evil-base
       :background "black"))
  "Used to indicate Replace Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-motion
  '((t :inherit vs-modeline-evil-base
       :background "dark blue"))
  "Used to indicate Motion Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-operator
  '((t :inherit vs-modeline-evil-base
       :background "violet"))
  "Used to indicate Operator Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-evil-emacs
  '((t :inherit vs-modeline-evil-base
       :background "sky violet"))
  "Used to indicate Emacs Evil state in modeline."
  :group 'vs-modeline)

(defface vs-modeline-project
  '((t :inherit mode-line
       :weight bold
       :foreground "light green"))
  "Used to indicate project name."
  :group 'vs-modeline)

(defface vs-modeline-project-type
  '((t :inherit mode-line
       :weight light
       :foreground "light green"))
  "Used to indicate project name."
  :group 'vs-modeline)

(defface vs-modeline-error
  '((t :inherit error :underline nil))
  "Used to indicate Flycheck errors."
  :group 'vs-modeline)

(defface vs-modeline-warning
  '((t :inherit warning :underline nil))
  "Used to indicate Flyckeck warnings."
  :group 'vs-modeline)

(defface vs-modeline-info
  '((t :foreground "#81a2be" :underline nil))
  "Used to indicate Flyckeck infos."
  :group 'vs-modeline)

(defun vs-modeline-evil-face ()
  "Get the face to use in modeline for the current evil state."
  (pcase (bound-and-true-p evil-state)
    ('normal 'vs-modeline-evil-normal)
    ('insert 'vs-modeline-evil-insert)
    ('visual 'vs-modeline-evil-visual)
    ('replace 'vs-modeline-evil-replace)
    ('motion 'vs-modeline-evil-motion)
    ('operator 'vs-modeline-evil-operator)
    ('emacs 'vs-modeline-evil-emacs)
    (_ 'mode-line)))

(defun vs-modeline-evil-text ()
  "Get the text to show in the modeline related to the current Evil state."
  (pcase (bound-and-true-p evil-state)
    ('normal " NORMAL ")
    ('insert " INSERT ")
    ('visual
     (pcase (bound-and-true-p evil-visual-selection)
       ('block " V-BLOCK ")
       ('line " V-LINE ")
       (_ " VISUAL ")))
    ('replace " REPLACE ")
    ('motion " MOTION ")
    ('operator " OPERATOR ")
    ('emacs " EMACS ")
    (_ "")))

(defmacro vs-modeline-def-segment (name form)
  "Define a modeline segment NAME.

Generate a function called `vs-modeline-NAME', and prepare it for
inclusion in the modeline. The function can be called in an :eval
form in `mode-line-format', and it executes FORM."
  (declare (indent defun)
           (debug (sexp body)))
  (let ((var-name (intern (format "vs-modeline-%s" name))))
    (macroexp-progn
     `((defun ,var-name ()
           ,form)
       (put ',var-name 'risky-local-variable-p t)
       (make-variable-buffer-local ',var-name)))))

(defmacro vs-modeline--active-propertize (text face &optional always-active &rest args)
  "Generate a `propertize' form showing TEXT in FACE.

If ALWAYS-ACTIVE is non-nil, apply FACE to TEXT always.
Otherwise, use `mode-line-inactive' when not in the focused window.

ARGS are appended at the end of the form."
  (declare (debug t))
  `(propertize
    (or ,text "")
    'face ,(if always-active
               face
             `(if (vs-modeline--selected-window-active-p)
                  ,face
                'mode-line-inactive))
    ,@args))

(defmacro vs-modeline-def-prop-segment (name text face &optional always-active &rest args)
  "Define a propertized modeline segment NAME.

Show TEXT with FACE.
If ALWAYS-ACTIVE is non-nil, always use the specified face.
Otherwise, use `mode-line-inactive' for inactive windows.

Append ARGS to the `propertize' form"
  (declare (indent defun)
           (debug t))
  `(vs-modeline-def-segment ,name
     (vs-modeline--active-propertize ,text ,face ,always-active ,@args)))

(defvar vs-modeline--last-selected-window nil
  "Handle to last selected window. Used for (in)activity tracking.")

(defun vs-modeline--focus-change-function (&rest _)
  "Update currently focused window.

Should be added to `after-focus-change-function'."
  (setq vs-modeline--last-selected-window (vs-modeline--focused-window))
  (force-mode-line-update))

(defun vs-modeline--focused-window ()
  "Get the currently focused window."
  (let ((new-window (and (cl-loop for f being the frames
                                  thereis (frame-focus-state f))
                         (selected-window))))
    (if (and new-window (window-minibuffer-p new-window))
        vs-modeline--last-selected-window
      new-window)))

(defun vs-modeline--selected-window-active-p ()
  "Return t if the current window is selected."
  (eq vs-modeline--last-selected-window (selected-window)))

(vs-modeline-def-prop-segment evil
  (vs-modeline-evil-text)
  (vs-modeline-evil-face))

;;; OPTIMIZE: projectile sometimes slows down buffers that are not
;;;           part of a project (VojtechStep 2021-01-25)
(declare-function projectile-project-name 'projectile)
(vs-modeline-def-prop-segment project-name
  (when (fboundp 'projectile-project-name)
    (concat " " (projectile-project-name)))
  'vs-modeline-project)

(declare-function projectile-project-type 'projectile)
(vs-modeline-def-segment project-type
  (when-let (((fboundp 'projectile-project-type))
             (pt (projectile-project-type)))
    (vs-modeline--active-propertize
     (concat "["
             (symbol-name pt)
             "]")
     'vs-modeline-project-type)))

(vs-modeline-def-prop-segment buffer-name
  " %12b"
  'mode-line-buffer-id)

(vs-modeline-def-prop-segment input-method
  current-input-method-title
  'mode-line)

(declare-function flycheck-count-errors 'flycheck)
(vs-modeline-def-segment flycheck
  (when (and (bound-and-true-p flycheck-mode)
             (boundp 'flycheck-last-status-change)
             (boundp 'flycheck-current-errors))
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let* ((stats
                             (flycheck-count-errors flycheck-current-errors))
                            (warns (alist-get 'warning stats))
                            (errs (alist-get 'error stats))
                            (infos (alist-get 'info stats)))
                       (cl-loop for (count . face)
                                in `((,errs . vs-modeline-error)
                                     (,warns . vs-modeline-warning)
                                     (,infos . vs-modeline-info))
                                when count
                                collect (vs-modeline--active-propertize
                                         (concat (number-to-string count)
                                                 " ")
                                         face)))
                   ":)"))
      ('running "*")
      ('not-checked "=")
      ('no-checker "-")
      ('errored "!")
      ('interrupted "x")
      ('suspicious (vs-modeline--active-propertize
                    "(sus)"
                    'vs-modeline-warning)))))

(declare-function org-clock-get-clock-string 'org-clock)
(declare-function org-clocking-p 'org-clock)
(vs-modeline-def-segment org-clock
  (let* ((op-line (when-let*
                      ((op (bound-and-true-p org-pomodoro-mode-line)))
                    (and (not (string-empty-p
                               (if (stringp op)
                                   op
                                 (apply #'concat op))))
                         (cons " " op))))
         (oc-line (when (and (fboundp 'org-clock-get-clock-string)
                             (org-clocking-p))
                    (let ((oc (concat (org-clock-get-clock-string))))
                      (and (not (string-empty-p oc)) oc))))
         (text (or op-line oc-line)))
    (when text
      (if (vs-modeline--selected-window-active-p)
          text
        (propertize
         text
         'face 'mode-line-inactive)))))

(vs-modeline-def-segment position-rel
  `(:propertize (-4 " %p")
                face ,(if (vs-modeline--selected-window-active-p)
                          (vs-modeline-evil-face)
                        'mode-line-inactive)))

(declare-function pdf-cache-number-of-pages 'pdf-tools)
(declare-function image-mode-window-get 'image-mode)
(vs-modeline-def-prop-segment position
  (pcase major-mode
    ('pdf-view-mode (format " %3s/%s "
                            (image-mode-window-get 'page)
                            (or (ignore-errors
                                  (pdf-cache-number-of-pages))
                                "??")))
    (_ " %4l:%3c "))
  (vs-modeline-evil-face))

(defcustom vs-modeline-left
  '("%e"
    (:eval (vs-modeline-evil))
    mode-line-process
    (:eval (vs-modeline-project-name))
    (:eval (vs-modeline-project-type))
    (:eval (vs-modeline-buffer-name))
    (:eval (when (buffer-modified-p) "+"))
    (:eval (when buffer-read-only " RO")))
  "A list of segments to appear on the left side of the modeline.

For reference of the format of the segments, see documentation
for `mode-line-format'."
  :type '(list sexp)
  :group 'vs-modeline)

(defcustom vs-modeline-right
  '((:eval (vs-modeline-input-method))
    " "
    (:eval (vs-modeline-flycheck))
    (:eval (vs-modeline-org-clock))
    " "
    mode-name
    " "
    (:eval (vs-modeline-position-rel))
    (:eval (vs-modeline-position)))
  "A list of segments to appear on the right side of the modeline.

See documentation for `vs-modeline-left'."
  :type '(list sexp)
  :group 'vs-modeline)

(vs-modeline-def-segment center-fill
  (let ((right-width (string-width (format-mode-line vs-modeline-right))))
    (vs-modeline--active-propertize
     " "
     'mode-line
     nil
     'display `((space :align-to (- (+ right right-fringe right-margin)
                                    ,right-width))))))

(defun vs-modeline--generate-modeline-format ()
  "Generate modeline format."
  `(,@vs-modeline-left
    (:eval (vs-modeline-center-fill))
    ,@vs-modeline-right))

;;;###autoload
(define-minor-mode vs-modeline-mode
  "Minor mode for showing my preferred mode line."
  :global t
  :group 'vs-modeline
  :lighter nil
  (if vs-modeline-mode
      (progn
        (add-function :after after-focus-change-function
                      #'vs-modeline--focus-change-function)
        (add-hook 'window-configuration-change-hook #'vs-modeline--focus-change-function)
        (advice-add #'handle-switch-frame :after #'vs-modeline--focus-change-function)
        (advice-add #'select-window :after #'vs-modeline--focus-change-function)
        (let ((format (vs-modeline--generate-modeline-format)))
          (setq-default mode-line-format format)
          (setq mode-line-format format)))
    (remove-function after-focus-change-function
                     #'vs-modeline--focus-change-function)
    (remove-hook 'window-configuration-change-hook #'vs-modeline--focus-change-function)
    (advice-remove #'handle-switch-frame #'vs-modeline--focus-change-function)
    (advice-remove #'select-window #'vs-modeline--focus-change-function)
    (setq-default mode-line-format vs-modeline--old-modeline)
    (setq mode-line-format vs-modeline--old-modeline)))

(provide 'vs-modeline)
;;; vs-modeline.el ends here
