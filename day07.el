(require 'dash)
(require 's)
(require 'advent-utils)

(defvar example (advent/read-problem-lines 7 :example))
(defvar problem (advent/read-problem-lines 7 :problem))

(defun day07/is-command? (term-output)
  (if (s-starts-with? "$" term-output)
      (rest (s-split " " term-output))))

(defun day07/down-dir (fs path)
  (advent/assert (hash-table-p fs))
  (if-let ((path-dir (advent/get fs path)))
      path-dir
    (day07/add-dir fs path)))

(defun day07/root-dir (fs)
  (advent/assert (hash-table-p fs))  
  (if (advent/get fs "..")
      (day07/root-dir (day07/up-dir fs))
    fs))

(defun day07/up-dir (fs)
  (advent/assert (hash-table-p fs))  
  (let ((up-dir (advent/get fs "..")))
    (unless up-dir (error "Missing parent"))
    up-dir))

(defun day07/change-dir (fs path)
  (advent/assert (hash-table-p fs))  
  (cond ((string= path "..") (day07/up-dir fs))
        ((string= path "/") (day07/root-dir fs))
        (t (day07/down-dir fs path))))

(defun day07/-new-dir (parent)
  (advent/assert (hash-table-p parent))
  (let ((new-dir (advent/table)))
    (advent/put new-dir ".." parent)
    new-dir))

(defun day07/add-dir (fs name)
  (advent/assert (hash-table-p fs))
  (advent/assert (hash-table-p fs))
  (let ((dir (advent/get fs name)))
    (when (numberp dir)
      (error (string-format "Dir %s already present and it's a file (%d)!" name dir)))    
    (when (not dir)
      (let ((new-dir (day07/-new-dir fs)))
        (advent/put fs name new-dir)
        (setq dir new-dir)))
    dir))

(defun day07/add-file (fs name size-string)
  (advent/assert (hash-table-p fs))  
  (let ((file (advent/get fs name))
        (size (string-to-number size-string)))
    (if (not file)
        (advent/put fs name size)
      (when (not (number file) (error (string-format "File %d already present but it's a directory!" name)))))))

(defun day07/add-object (fs term-output)
  (advent/assert (hash-table-p fs))  
  (seq-let (size-dir name) (s-split " " term-output())
    (if (string= size-dir "dir")
        (day07/add-dir fs name)
      (day07/add-file fs name size-dir)))
  fs)

(defun day07/slurp-data (fs term-output)
  (advent/assert (hash-table-p fs))
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
  (advent/assert (hash-table-p fs))  
  (advent/-each-hash fs
    (print (format "%15s %s" it-key it-value))))

(defun day07/dir-size (fs)
  (advent/assert (hash-table-p fs))  
  (apply #'+
         (advent/-map-hash fs
           (cond
            ((string= it-key "..") 0)
            ((numberp it-value) it-value)
            ((hash-table-p it-value) (day07/dir-size it-value))
            (t (error "Unexpected condition"))))))

(defun day07/all-folders-but-root (fs)
  (advent/assert (hash-table-p fs))  
  (--mapcat (list (list (car it) (day07/all-folders-but-root (cadr it))))
         (--filter (and (not (string= (car it) ".."))
                        (not (numberp (cadr it))))
                   (advent/-map-hash fs (list it-key it-value)))))

(defun day07/is-dir? (fs name)
  (advent/assert (hash-table-p fs))
  (hash-table-p (advent/get fs name)))

(defun day07/all-sizes (fs &optional sizes)
  (setq sizes (cons (day07/dir-size fs) sizes))
  (advent/-each-hash fs
    (when (and (not (string= ".." it-key))
               (day07/is-dir? fs it-key))
        (setq sizes (day07/all-sizes it-value sizes))))
  sizes)

(defun day07/part-1 (lines)  
  (apply #'+
         (--filter (<= it 100000)
             (day07/all-sizes (day07/fill-filesystem lines)))))

(defun day07/part-2 (lines)
  (let* ((fs (day07/fill-filesystem lines))
         (used-space (day07/dir-size fs))
         (free-space (- 70000000 used-space))
         (required-space (- 30000000 free-space)))
    (car
     (-sort #'<
            (--filter (>= it required-space) (day07/all-sizes fs))))))

(provide 'day07)
