

;;; TODO/FIXME macro
(defun template/-with-read-file (filename function)
  (print (format "Reading '%s'" filename))
  (with-temp-buffer
    (insert-file filename)
    (goto-char (point-min))
    (funcall function)
    (goto-char (point-min))
    (buffer-string)))

(defun template/-replace-tokens (day)
  (print (format "Replacing the tokens for %d in the current buffer" day))
  (let ((day-1d (number-to-string day))
        (day-2d (format "%.2d" day)))
    ;;; TODO/FIXME look what the heck I'm doing here (copy-paste)
    (setq case-fold-search nil)
    (while (search-forward "XX" nil t)
      (replace-match day-2d t t))    
    (goto-char (point-min))
    (while (search-forward "X" nil t)
      (replace-match day-1d t t))))

(defun template/get-solution-template (templates)
  (format "%s/dayXX.el-template" templates))

(defun template/get-solution-file (day)
  (format "day%.2d.el" day))

(defun template/copy-solution (day templates)
  (print (format "* Copying the solution for day %s " day))
  ;;; TODO/FIXME horrid path composition
  (template/-with-read-file (template/get-solution-template templates)
                            (lambda ()
                              (template/-replace-tokens day)
                              (append-to-file (point-min)
                                              (point-max)
                                              (template/get-solution-file day)))))

(defun template/get-test-template (templates)
  (format "%s/dayXX-test.el-template" templates))

(defun template/get-test-file (day)
  (format "day%.2d-test.el" day))

(defun template/copy-test (day templates)
  (print (format "* Copying the test for day %s " day))
  ;;; TODO/FIXME horrid path composition
  (template/-with-read-file (template/get-test-template templates)
                            (lambda ()
                              (template/-replace-tokens day)
                              (append-to-file (point-min)
                                              (point-max)
                                              (template/get-test-file day)))))


(defun template/copy-templates ()
  (let ((day (string-to-number (elt argv 0)))
        (template-folder (elt argv 1)))
    (template/copy-solution day template-folder)
    (template/copy-test day template-folder)))

(template/copy-templates)
