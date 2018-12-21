(require 'json)

(let ((json-object-type 'plist)
      (json-array-type  'list)
      (json-key-type    'keyword))
  (let* ((current-dir (file-name-directory
                       (directory-file-name load-file-name)))
         (read-file   (concat current-dir (nth 0 command-line-args-left)))
         (write-file  (concat current-dir (nth 1 command-line-args-left)))
         (detail-p    (and (nth 2 command-line-args-left)
                           (not (string= "nil" (nth 2 command-line-args-left)))))
         (list-p      (and (nth 3 command-line-args-left)
                           (not (string= "nil" (nth 3 command-line-args-left))))))
    (if (and (file-readable-p read-file)
             (file-writable-p read-file)
             (file-writable-p write-file))
        (let ((hash (make-hash-table :test 'eq))
              (obj  (json-read-file read-file))
              (key) (val) (props))
          (while obj
            (setq key (pop obj))
            (setq val (pop obj))
            (setq props (plist-get val :props))
            
            (puthash key (cdr
                          `(:dammy-symbol
                            :ver  ,(plist-get val :ver)
                            :deps ,(plist-get val :deps)
                            :url  ,(plist-get props :url)
                            ,@(when detail-p
                                (cdr
                                 `(:dammy-symbol
                                   :description ,(plist-get props :desc)
                                   :keywords    ,(plist-get props :keywords)
                                   :authors     ,(plist-get props :authors)
                                   :maintainer  ,(plist-get props :maintainer))))))
                     hash))

          (with-temp-file write-file
            (if list-p
                (let ((str ""))
                  (maphash (lambda (key val)
                             (setq str (format "%s %s %s" str key val)))
                           hash)
                  (setq str (concat str ")\n"))
                  
                  (insert (replace-regexp-in-string ":@ " ":feather--@ " str))
                  (goto-char (point-min))
                  (delete-char 1) (insert "(")

                  (condition-case err
                      (while t
                        (forward-sexp)
                        (forward-sexp)
                        (newline))
                    (error #'ignore))

                  (goto-char (point-min))
                  (while (search-forward ":feather--@" nil t)
                    (replace-match ":@" nil t)))
              
              (insert (replace-regexp-in-string ":@ " ":feather--@ "
                                                (prin1-to-string hash)))
              (newline)

              (goto-char (point-min))
              (search-forward "(") (search-forward "(")
              (backward-char)
              (newline) (insert (make-string 3 ? ))
              (forward-char)
              
              (condition-case err
                  (while t
                    (forward-sexp)
                    (forward-sexp)
                    (newline) (insert (make-string 3 ? )))
                (error #'ignore))

              (goto-char (point-min))
              (while (search-forward ":feather--@" nil t)
                (replace-match ":@" nil t))))

          (with-temp-file read-file
            (insert-file-contents read-file)
            (when (= (count-lines (point-min) (point-max)) 1)
              (goto-char (point-min))
              (forward-char)

              (condition-case err
                  (while t
                    (forward-sexp) (forward-sexp)
                    (forward-char)
                    (newline) (insert " "))
                (error #'ignore))))
          
          (princ (format "Process completed!!\nRead file:  %s\nWrite file: %s\n"
                         read-file write-file)))
      
      (error "File open error"))))
