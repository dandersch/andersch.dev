;
; ENABLE #+BEGIN_SRC CODE BLOCKS
;
;; set package install dir to local directory
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; dependencies
(package-install 'htmlize)

(require 'ox-publish)
(require 'org)
(require 'cl-lib)

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)

(setq keywords '("TITLE" "DATE" "DESCRIPTION" "IMAGE")) ; keywords to parse from .org files

; helper functions
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

(defun prepare-publishing (project-properties)
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
                 "<description>\n"
                 "&lt;p&gt;%s&lt;/p&gt;\n"
                 "&lt;img src=\"https://andersch.dev/article/%s\"/&gt;\n"
                 "</description>\n"
                 "<pubDate>%s</pubDate>\n</item>\n")
            (cadr (assoc "TITLE" (cadr article)))
            (concat "https://andersch.dev/" (string-replace ".org" ".html" (car article)))
            (concat "https://andersch.dev/" (string-replace ".org" ".html" (car article)))
            (cadr (assoc "DESCRIPTION" (cadr article)))
            (cadr (assoc "IMAGE" (cadr article)))
            (format-time-string "%a, %d %b %Y %H:%M:%S %z" (seconds-to-time (org-time-string-to-time (cadr (assoc "DATE" (cadr article))))))
            )
      nil "feed.rss" 'append))
  ; rss ending
  (write-region "</channel>\n</rss>" nil "feed.rss" 'append)

  ;
  ; EXECUTE NAMED SRC BLOCKS
  ;
  (defun get-article-keyword-list () article-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
  (defun get-project-keyword-list () project-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
  (dolist (org-file (directory-files-recursively "./" "\\.org$"))
    (find-file org-file)
    (setq src-block-names '("list-of-articles" "list-of-projects" "latest-article" "latest-project"))
    (goto-char (point-min))
    (setq org-confirm-babel-evaluate nil) ; NOTE needed when org-babel-execute-src-block is called in a script
    (dolist (src-block-name src-block-names)
      (if (org-babel-find-named-block src-block-name)
        (progn
          (org-babel-goto-named-src-block src-block-name)
          (org-babel-execute-src-block))))
    (save-buffer)
    (kill-buffer))
)

;; customize HTML output (see https://pank.eu/blog/blog-setup.html)
(setq
       org-html-validation-link nil                      ;; Don't show validation link
       org-html-html5-fancy t                            ;; ...
       org-html-preamble t
       org-html-divs '((preamble "header" "top")         ;; ...
                       (content "main" "content")
                       (postamble "footer" "postamble"))
       org-html-head (concat
                      "<title>andersch.dev</title>\n"
                      "<link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">\n"
                      "<link rel=\"stylesheet\" href=\"/style.css\">\n")
       org-html-preamble-format `(("en" ,(with-temp-buffer (insert-file-contents "header.html") (buffer-string)))))

(setq org-publish-project-alist
      (list
       (list "andersch.dev"
             :recursive            t
             :base-directory       "./"
             :publishing-directory "./"
             :publishing-function  'org-html-publish-to-html ;; may be a list of functions
             :preparation-function 'prepare-publishing       ;; called before publishing
           ; :completion-function                            ;; called after
           ; :base-extension                                 ;; extension of source files
             :exclude "sitemap.org"                          ;; regex of files to exclude NOTE excluding dirs seems to not work
           ; :include                                        ;; list of files to include

             :auto-sitemap         t                         ;; https://orgmode.org/manual/Site-map.html
             :sitemap-filename     "sitemap.org"             ;; ...
           ; :sitemap-title
             :sitemap-style        'tree                     ;; list or tree
             :sitemap-sort-files   'anti-chronologically
           ;  :sitemap-function     'my-format-rss-feed
           ;  :sitemap-format-entry 'my-format-rss-feed-entry

           ; :makeindex t                                    ;; https://orgmode.org/manual/Generating-an-index.html

             :with-title           nil                       ;; we include our own header
             :with-author          nil
             :with-creator         nil                       ;; don't include emacs and org versions in footer
             :with-toc             nil                       ;; no table of contents
             :section-numbers      nil                       ;; no section numbers for headings
             :time-stamp-file      nil)                      ;; don't include "Created: <timestamp>" in footer
       (list "attachments"
             :recursive            t
             :base-directory "./"
             :base-extension "png\\|jpg\\|rss"
             :publishing-directory "../publish/"
             :publishing-function 'org-publish-attachment
     )))

; NOTE caching causes problems with updating titles etc., so we reset the cache before publishing
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-timestamp-directory "./.org-timestamps/")
(org-publish-remove-all-timestamps)
; NOTE these resets seem unnessecary
;(org-element-cache-reset)
;(org-refile-cache-clear)
;(org-reset-file-cache)
;(org-publish-reset-cache)

; NOTE workaround to not get a "Symbol’s function definition is void" error when publishing
(defun get-article-keyword-list ())
(defun get-project-keyword-list ())

(org-publish "andersch.dev" t) ;; expand @@..@@ markers, export html files, copy image files
(org-publish "attachments")    ;; copy image files

(message "Build complete")
