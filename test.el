(require 'org)
(require 'cl-lib)

(setq keywords '("TITLE" "DATE" "DESCRIPTION" "IMAGE"))

(defun get-org-files (directory)
  "Return a list of .org files in DIRECTORY excluding 'index.org'."
  (cl-remove-if
   (lambda (file) (string-equal "index.org" (file-name-nondirectory file)))
   (directory-files-recursively directory "\\.org$")))

(defun get-org-file-keywords (file)
  (with-temp-buffer
    (insert-file-contents file)
    ;(org-mode)
    (list file (org-collect-keywords keywords))))

(defun sort-keyword-list-by-date (keyword-list &optional new-to-old)
  "Sort the list by the date value of the form <YYYY-MM-DD HH:MM>"
  (sort keyword-list
        (lambda (a b)
          (let* ((date-str-a (replace-regexp-in-string "<\\[\\]>" "" (cadr (assoc "DATE" (cadr a)))))
                 (date-str-b (replace-regexp-in-string "<\\[\\]>" "" (cadr (assoc "DATE" (cadr b))))))
            (if new-to-old
              (string> date-str-a date-str-b)
              (string< date-str-a date-str-b))))))

(defun create-org-list-from-keyword-list (keyword-list)
  "Return a list of strings that makes up an org-mode list"
  ; (dolist (entry keyword-list)
  ;   (format "- [[%s][%s]]\n" path title)
  ; )
  )

; (org-publish "andersch.dev")
;  :preparation-function generate-feed-and-fill-markers

; create keyword-lists for project/ and article/
(setq article-keyword-list '())
(dolist (article (get-org-files "article"))
  (push (get-org-file-keywords article) article-keyword-list))
(setq article-keyword-list (sort-keyword-list-by-date article-keyword-list t))

(setq project-keyword-list '())
(dolist (project (get-org-files "project"))
  (push (get-org-file-keywords project) project-keyword-list))
(setq project-keyword-list (sort-keyword-list-by-date project-keyword-list t))
(print project-keyword-list)


; rss header
; for

; find markers
(dolist (org-file (directory-files-recursively "./" "\\.org$"))
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    (if (search-forward-regexp "@@start:.*@@" nil t)
      (print org-file)
      ;; start:projects          -> list of projects
      ;; start:articles          -> list of articles
      ;; start:latest-article    -> latest article
      ;; start:latest-projectile -> latest project

        )
      )
  )

; NOTE workaround to pass keyword-list to a source-block in an org file
(defun get-article-keyword-list () article-keyword-list)
(defun get-project-keyword-list () project-keyword-list)

(find-file "index.org")
;(org-mode)
(goto-char (point-min))
(setq org-confirm-babel-evaluate nil) ;; NOTE needed when org-babel-execute-src-block is called in a script

(org-babel-goto-named-src-block "list-of-articles")
;(if (re-search-forward (format "#\\+NAME: %s" "test") nil t)
;  (progn
    ;(org-babel-execute-src-block nil nil '((list . article-keyword-list) ("x" . 10)))
    ;(org-babel-execute-src-block nil nil '((x . 10) ("x=5") (x . "5") ("x" . 5))))
(org-babel-execute-src-block)
(org-babel-goto-named-src-block "latest-article")
(org-babel-execute-src-block)
(org-babel-goto-named-src-block "latest-project")
(org-babel-execute-src-block)
(save-buffer)
(kill-buffer)
