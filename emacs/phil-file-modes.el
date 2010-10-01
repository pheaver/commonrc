;; phil-file-modes.el
;;
;; Copyright (C) 2010 Philip Weaver
;; Author: Philip Weaver <philip.weaver@gmail.com>
;;
;; This file is not part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary
;;
;; I use cygwin alongside a native Windows (non-cygwin) build of Emacs.  This
;; means that whenever I save a file in Emacs, the unix permissions get mangled.
;; For example, if I open a file with permissions -rw-r--r-- and then save it,
;; it will wind up with permissions -rw-rw-rw-.  This is not a bug at all; if
;; you want Emacs to understand cygwin file permissions, then you need to use a
;; cygwin build of Emacs.
;;
;; This package provides a (hackish) way to preserve the unix file permissions
;; of files that you open and save in Emacs.  It record the permissions when you
;; open a file, and then writes them using "chmod" when you save the file.  To
;; enable, I suggest putting this in your ~/.emacs:
;;
;; (when (and (equal system-type 'windows-nt) (executable-find "cygpath"))
;;   (require 'phil-file-modes)
;;   (add-hook 'find-file-hook 'phil/file-modes-check)
;;   (add-hook 'after-save-hook 'phil/file-modes-restore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar phil/default-file-modes 644) ;; 438
(defvar phil/file-modes nil)
(make-variable-buffer-local 'phil/file-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utility helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun phil/call-process-to-string (prg &rest args)
  (interactive)
  (with-output-to-string
    (with-current-buffer standard-output
      (eval (append (list 'call-process prg nil t nil) args)))))

;; type can be dos, mixed, unix, or windows
(defun phil/cygpath (file type)
  (interactive)
  (when (not (memq type '(dos mixed unix windows)))
    (error (concat "phil/cygpath, invalid type: " (symbol-name type))))
  (let ((flag (concat "--" (symbol-name type))))
    (replace-regexp-in-string "\n" ""
      (phil/call-process-to-string "cygpath" flag file))))

(defun phil/file-modes-check ()
  (interactive)
  (let ((f (buffer-file-name)))
    (setq phil/file-modes (and f (file-exists-p f) (phil/get-file-modes f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the two top-evel functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun phil/get-file-modes (file)
  (interactive)
  (string-to-number
   (replace-regexp-in-string "\n" ""
    (phil/call-process-to-string "stat" "--format=%a" file))))

;;;###autoload
(defun phil/file-modes-restore ()
  (interactive)
  (let ((f (buffer-file-name))
        (file-modes (or phil/file-modes phil/default-file-modes)))
    (when (and f file-modes)
      (call-process "chmod" nil nil nil
                    (number-to-string file-modes)
                    (phil/cygpath f 'unix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'phil-file-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; phil-file-modes.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
