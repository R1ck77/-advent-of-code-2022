;; -*- lexical-binding: t -*-

(defun template/-with-read-file (filename function)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (funcall function)
    (goto-char (point-min))
    (buffer-string)))

(defmacro template/with-read-file (filename &rest forms)
  (declare (indent 1))
  `(template/-with-read-file ,filename
                             (lambda ()
                               ,@forms)))

(defun template/-replace-tokens (day)
  (let ((day-1d (number-to-string day))
        (day-2d (format "%.2d" day)))
    ;;; TODO/FIXME look what the heck I'm doing here (copy-paste)
    (setq case-fold-search nil)
    (while (search-forward "XX" nil t)
      (replace-match day-2d t t))    
    (goto-char (point-min))
    (while (search-forward "X" nil t)
      (replace-match day-1d t t))))

;;; TODO/FIXME horrid path composition x 4
(defun template/get-solution-template (templates)
  (format "%s/dayXX.el-template" templates))

(defun template/get-solution-file (day)
  (format "day%.2d.el" day))

(defun template/copy-solution (day templates)
  (template/with-read-file (template/get-solution-template templates)
    (template/-replace-tokens day)
    (let ((out-name (template/get-solution-file day)))
      (delete-file out-name)
      (append-to-file (point-min)
                      (point-max)
                      (template/get-solution-file day)))))

(defun template/get-test-template (templates)
  (format "%s/dayXX-test.el-template" templates))

(defun template/get-test-file (day)
  (format "day%.2d-test.el" day))

(defun template/copy-test (day templates)
  (template/with-read-file (template/get-test-template templates)
    (template/-replace-tokens day)
    (let ((out-name (template/get-test-file day)))
      (delete-file out-name)
      (append-to-file (point-min)
                      (point-max)
                      out-name))))


(defun template/copy-templates ()
  (let ((day (string-to-number (elt argv 0)))
        (template-folder (elt argv 1)))
    (template/copy-solution day template-folder)
    (template/copy-test day template-folder)))

(template/copy-templates)