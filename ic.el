;;; ic.el --- Pretty print to debug  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/ic
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (ppp "2.2.4") (msgu "0.1.0") (ht "2.0") (dash "2.14.1"))
;; Keywords: convenience

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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Pretty print to debug
;;

;;; Code:

(require 'cl-lib)
(require 'pp)

(require 'ppp)
(require 'msgu)
(require 'ht)
(require 'dash)

(defgroup ic nil
  "Pretty print to debug."
  :prefix "ic-"
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/jcs-elpa/ic"))

(defcustom ic-log t
  "If non-nil, log out message."
  :type 'boolean
  :group 'ic)

(defvar ic-inhibit-clear t
  "If nil, clear messages buffer before logging.")

;;
;;; Util

(defun ic-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

;;
;;; Face

(defun ic-get-faces (pos)
  "Return the list of faces at this POS."
  (delete-dups
   (-flatten
    (remq nil
          (list
           (get-char-property pos 'read-face-name)
           (get-char-property pos 'face)
           (plist-get (text-properties-at pos) 'face))))))

(defun ic-get-current-point-face (&optional pos)
  "Get current POS's type face as string."
  (ic-get-faces (or pos (point))))

;;;###autoload
(defun ic-print-current-face ()
  "Print out all the faces the current cursor on."
  (interactive)
  (message "[INFO] Current faces: %s" (ic-get-current-point-face)))

;;
;;; Core

(defun ic--clear ()
  "Clean message buffer."
  (when (and ic-log
             (not ic-inhibit-clear))
    (with-current-buffer (messages-buffer)
      (let ((buffer-read-only)) (erase-buffer)))))

(defun ic--advice (fnc &rest args)
  "Clean message buffer.

Arguments FNC and ARGS are for function `advice-add'."
  (when ic-log
    (ic--clear)
    (apply fnc args)
    (with-current-buffer (messages-buffer)
      (goto-char (point-max)))))

(defun ic--pp (object)
  "Pretty print OBJECT."
  (cond ((stringp object) object)
        ((hash-table-p object)
         (concat (pp object)
                 (ppp-plist-to-string (ht-to-plist object))))
        (t
         (if-let* ((func (cond ((functionp object) #'pp)
                               ((plistp object) #'ppp-plist-to-string)
                               ((listp object) #'ppp-list-to-string)
                               (t              #'pp)))
                   (result (msgu-silent (ignore-errors (apply func (list object))))))
             (ic-2str result)
           (ic-2str object)))))

(defun ic--mapconcat (func seq)
  "Like function `mapconcat', but compatible to newline separator.

Arguments FUNC and SEQ are for function `mapconcat'."
  (let ((result "")
        (next-sep)
        (next-str)
        (count 0)
        (len (1- (length seq))))
    (dolist (elm seq)
      (setq next-str (funcall func elm)
            next-sep (if (or (= count len)
                             (string-suffix-p "\n" next-str))
                         "" " ")
            result (concat result next-str next-sep))
      (cl-incf count))
    result))

;;;###autoload
(defun ic-message (&rest args)
  "Wrapper for function `message' (ARGS)."
  (msgu-unsilent
    (if (stringp (car args))
        (apply #'message (cl-first args) (cl-rest args))
      (message "%s" (ic--mapconcat #'ic--pp args)))))

;;;###autoload
(defun ic-princ (&rest args)
  "Wrapper for function `princ' (ARGS)."
  (msgu-unsilent
    (let ((output (format "%s" (ic--mapconcat #'ic--pp args))))
      (princ output))))

;;
;;; Register Events

(dolist (func '( ic-message
                 ic-princ))
  (advice-add func :around #'ic--advice))

(provide 'ic)
;;; ic.el ends here
