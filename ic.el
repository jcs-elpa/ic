;;; ic.el --- Pretty print to debug  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/ic
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ppp "2.2.4") (msgu "0.1.0") (ht "2.0") (dash "2.14.1"))
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

;; NOTE: Unused
(defun ic-alistp (object)
  "Return t if OBJECT is a associate list."
  (and (listp object)
       (consp (car object))))

(defun ic-plistp (object)
  "Return t if OBJECT is a property list."
  (and (plistp object)
       (keywordp (car object))))

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

;;;###autoload
(defun ic-pp (object)
  "Pretty print OBJECT."
  (let ((pp-use-max-width t)
        (pp-max-width fill-column))
    (cond ((null object) (ic-2str object))
          ((stringp object) object)
          ((hash-table-p object)
           (concat (pp-to-string object)
                   (format "size: %s\n" (ht-size object))
                   (pp-to-string (ht-to-alist object))))
          ((functionp object) (pp-to-string object))
          ((ic-plistp object) (ppp-plist-to-string object))
          ((listp object) (pp-to-string object))
          (t
           (ic-2str object)))))

(defun ic--mapconcat (func seq)
  "Like function `mapconcat', but compatible to newline separator.

Arguments FUNC and SEQ are for function `mapconcat'."
  (let ((new-seq))
    (dolist (elm seq)
      (push (funcall func elm) new-seq))
    (string-join (reverse new-seq) " ")))

;;;###autoload
(defun ic-message (&rest args)
  "Wrapper for function `message' (ARGS)."
  (msgu-unsilent
    (if-let* ((fmt (car args))
              ((and (stringp fmt)
                    (string-match-p "%" fmt))))
        (apply #'message fmt (cl-rest args))
      (apply #'message "%s" (list (ic--mapconcat #'ic-pp args))))))

;;;###autoload
(defalias 'ic #'ic-message "Print any object.")

;;;###autoload
(defun ic-princ (&rest args)
  "Wrapper for function `princ' (ARGS)."
  (msgu-unsilent
    (let ((output (format "%s" (ic--mapconcat #'ic-pp args))))
      (princ output))))

;;
;;; Register Events

(dolist (func '( ic-message
                 ic-princ))
  (advice-add func :around #'ic--advice))

(provide 'ic)
;;; ic.el ends here
