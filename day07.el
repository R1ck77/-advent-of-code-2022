(require 'dash)
(require 's)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 7 :example))
(defvar problem (advent/read-problem-lines 7 :problem))

(defun day07/is-command? (term-output)
  (if (s-starts-with? "$" term-output)
      (rest (s-split " " term-output))))

(defun day07/down-dir (fs path)
  (assert (hash-table-p fs))
  (if-let ((path-dir (advent/get fs path)))
      path-dir    
    (print (format "*** missing path '%s' (will create)" path))
    (day07/add-dir fs path) ;; TODO/FIXME horrid
    (advent/get fs path)))

(defun day07/root-dir (fs)
  (assert (hash-table-p fs))  
  (if (advent/get fs "..")
      (day07/root-dir (day07/up-dir fs))
    fs))

(defun day07/up-dir (fs)
  (assert (hash-table-p fs))  
  (let ((up-dir (advent/get fs "..")))
    (unless up-dir (error "Missing parent"))
    up-dir))

(defun day07/change-dir (fs path)
  (assert (hash-table-p fs))  
  (cond ((string= path "..") (day07/up-dir fs))
        ((string= path "/") (day07/root-dir fs))
        (t (day07/down-dir fs path))))

(defun day07/-new-dir (parent)
  (let ((new-dir (advent/table)))
    (advent/put new-dir ".." parent)
    new-dir))

(defmacro assert (condition &optional message)
  `(unless ,condition (error (or ,message "Assertion failed"))))

(defun day07/add-dir (fs name)
  (assert (hash-table-p fs))
  (let ((dir (advent/get fs name)))
    (if (not dir)
        (advent/put fs name (day07/-new-dir fs))
      (when (number dir) (error (string-format "Dir %s already present and it's a file (%d)!" name dir))))))

(defun day07/add-file (fs name size-string)
  (assert (hash-table-p fs))  
  (let ((file (advent/get fs name))
        (size (string-to-number size-string)))
    (if (not file)
        (advent/put fs name size)
      (when (not (number file) (error (string-format "File %d already present but it's a directory!" name)))))))

(defun day07/add-object (fs term-output)
  (assert (hash-table-p fs))  
  (seq-let (size-dir name) (s-split " " term-output())
    (if (string= size-dir "dir")
        (day07/add-dir fs name)
      (day07/add-file fs name size-dir)))
  fs)

(defun day07/slurp-data (fs term-output)
  (assert (hash-table-p fs))  
  (if-let ((command (day07/is-command? term-output)))
      (if (string= (car command) "cd") ;; "ls" is ignored
          (day07/change-dir fs (cadr command))
        fs)
    (day07/add-object fs term-output)))

(defun day07/fill-filesystem (lines)
  (let ((fs (advent/table)))
    (unless (string= (car lines) "$ cd /") (error "Invalid starting point"))
    (-reduce-from #'day07/slurp-data fs lines)
    fs))

(defun day07/print-dir (fs)
  (advent/-each-hash fs
    (print (format "%15s %s" it-key it-value))))

(defun day07/dir-size (fs)
  (apply #'+
         (advent/-map-hash fs
           (cond
            ((string= it-key "..") 0)
            ((numberp it-value) it-value)
            ((hash-table-p it-value) (day07/dir-size it-value))
            (t (error "Unexpected condition"))))))

(defun day07/all-folders (fs)
  (append (list fs)
          (--filter (and (not (string= (car it) ".."))
                         (not (numberp (cadr it))))
                    (advent/-map-hash fs (list it-key it-value))))
  )

(defun day07/recurse-dirs (fs filter-function)
  (append (list (funcall filter-function fs))
          (--filter (and (not (string= (car it) ".."))
                         (not (numberp (cadr it))))
                    (advent/-map-hash fs (list it-key it-value)))))

(defun day07/part-1 (lines)
  (day07/fill-filesystem lines))

(defun day07/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day07)
