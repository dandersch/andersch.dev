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

;
; GENERATE RSS FEED FOR ARTICLES
;
; rss header, check with  https://validator.w3.org/feed/
(with-temp-file "feed.rss"
  (insert
   (let* ((website-title "andersch.dev")
          (homepage      "https://andersch.dev")
          (rss-filepath  "/feed.rss"))
   (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
           "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
           "<channel>\n"
           (format "<title>%s</title>\n" website-title)
           "<!-- <lastBuildDate>Wed, 15 Dec 2021 00:00:00 +0000</lastBuildDate> -->\n" ; TODO insert todays date
           (format "<atom:link href=\"%s%s\" rel=\"self\" type=\"application/rss+xml\"/>\n" homepage rss-filepath)
           (format "<link>%s/index.html</link>\n" homepage)
           "<description>Stuff on programming</description>\n"
           "<language>en-us</language>\n"))))
; rss entries
(dolist (article article-keyword-list)
  (write-region
    (format
       (concat "<item>\n"
               "<title>%s</title>\n"
               "<link>%s</link>\n"
               "<guid>%s</guid>\n"
               "<description>%s</description>\n"
               "<pubDate>%s</pubDate>\n</item>\n")
          (cadr (assoc "TITLE" (cadr article)))
          (concat "https://andersch.dev/" (string-replace ".org" ".html" (car article)))
          (concat "https://andersch.dev/" (string-replace ".org" ".html" (car article)))
          (cadr (assoc "DESCRIPTION" (cadr article)))
          (format-time-string "%a, %d %b %Y %H:%M:%S %z" (seconds-to-time (org-time-string-to-time (cadr (assoc "DATE" (cadr article))))))
          )
    nil "feed.rss" 'append))
; rss ending
(write-region "</channel>\n</rss>" nil "feed.rss" 'append)

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

;
; EXECUTE NAMED SRC BLOCKS
;
(defun get-article-keyword-list () article-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
(defun get-project-keyword-list () project-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
(find-file "index.org")
(setq src-block-names '("list-of-articles" "latest-article" "latest-project"))
(goto-char (point-min))
(setq org-confirm-babel-evaluate nil) ; NOTE needed when org-babel-execute-src-block is called in a script
(dolist (src-block-name src-block-names)
  (org-babel-goto-named-src-block src-block-name)
  (org-babel-execute-src-block))
(save-buffer)
(kill-buffer)
