;; set package install dir to local directory
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(package-install 'htmlize) ; enable export of #+begin_src code blocks
(require 'ox-publish)
(require 'org)
(require 'cl-lib)
(package-install 'org-contrib) ; for ox-extra
(require 'ox-extra) ; for :IGNORE: headlines
(ox-extras-activate '(latex-header-blocks ignore-headlines))

(require 'ob-emacs-lisp)

(setq
      keywords '("TITLE" "DATE" "DESCRIPTION" "IMAGE" "TAGS[]") ; keywords to parse from .org files
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t
      org-export-allow-bind-keywords t ; Allows #+BIND: in a buffer
      )

(defun get-org-files (directory)
  "Return a list of .org files in DIRECTORY excluding 'index.org'."
  (cl-remove-if
   (lambda (file) (string-equal (concat directory "/" "index.org") file))
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
(defun article-marked-for-noexport-p (article)
  (string-match-p (regexp-quote "noexport") (cadr (assoc "TAGS[]" (cadr article)))))

; TODO put together
(setq article-keyword-list '())
(setq project-keyword-list '())
(setq other-keyword-list   '())

(defun filter-out-index-html (transcoded-data-string backend communication-channel-plist)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "/index.html" "" transcoded-data-string)
  ))

(add-to-list 'org-export-filter-link-functions 'filter-out-index-html)


(defun footnotes-filter (transcoded-data-string backend communication-channel-plist)
  ;(when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\*" "" transcoded-data-string)
  ;  )
  )
(add-to-list 'org-export-filter-footnote-reference-functions 'footnotes-filter)

(defun footnotes-def-filter (transcoded-data-string backend communication-channel-plist)
  ;(when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\*" "" transcoded-data-string)
  ;  )
  )
(add-to-list 'org-export-filter-footnote-definition-functions 'footnotes-def-filter)

; set footnotes to be h3
(setq org-html-footnotes-section
      "<div id=\"footnotes\">\n<h3 class=\"footnotes\">%s: </h3>\n<div id=\"text-footnotes\">\n%s\n</div>\n</div>")

(setq comment-section-html
      (concat "<hr>\n"
       "<div id=\"comment-section\">\n"
       "<h3 id=\"comment-section-title\">Comments</h3>\n"
       "<script src=\"https://utteranc.es/client.js\"
               repo=\"dandersch/andersch.dev\"
               issue-term=\"pathname\"
               label=\".💬\"
               theme=\"photon-dark\"
               crossorigin=\"anonymous\"
               async>
       </script></div>\n"))

; needed because otherwise footnotes will be below the comment section
(defun insert-comment-section  (contents html-backend info)
  (when (string-match "</main>" contents)
    (replace-match (concat comment-section-html "</main>") t t contents 0)))

(defun prepare-publishing (project-properties)
  ;
  ; FILL & SORT KEYWORD-LISTS FOR PROJECT/, ARTICLE/, OTHER/
  ;
  (dolist (article (get-org-files "article"))
    (let ((article-keywords (get-org-file-keywords article)))
      (unless (article-marked-for-noexport-p article-keywords)
        (push (get-org-file-keywords article) article-keyword-list))))
  (setq article-keyword-list (sort-keyword-list-by-date article-keyword-list t))

  (dolist (project (get-org-files "project"))
    (let ((project-keywords (get-org-file-keywords project)))
      (unless (article-marked-for-noexport-p project-keywords)
        (push (get-org-file-keywords project) project-keyword-list))))
  (setq project-keyword-list (sort-keyword-list-by-date project-keyword-list t))

  (dolist (other (get-org-files "other"))
    (let ((other-keywords (get-org-file-keywords other)))
      (unless (article-marked-for-noexport-p other-keywords)
        (push (get-org-file-keywords other) other-keyword-list))))
  (setq other-keyword-list (sort-keyword-list-by-date other-keyword-list t))

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
  (dolist (org-file (directory-files-recursively "./" "\\.org$"))
      (find-file org-file)
      (setq src-block-names '("list-of-projects" "latest-article" "latest-project" "generate-tags"))
      (goto-char (point-min))
      (setq org-confirm-babel-evaluate nil) ; NOTE needed when org-babel-execute-src-block is called in a script
      (dolist (src-block-name src-block-names)
        (if (org-babel-find-named-block src-block-name)
          (progn
            (org-babel-goto-named-src-block src-block-name)
            (org-babel-execute-src-block))))
      (save-buffer)
      (kill-buffer))

  ;
  ; TAGGING SYSTEM
  ;
  ; collect all tags
  (setq all-tags '())
  (dolist (article article-keyword-list)
     (setq all-tags (append (split-string (cadr (assoc "TAGS[]" (cadr article)))  " +") all-tags)))
  (dolist (project project-keyword-list)
     (setq all-tags (append (split-string (cadr (assoc "TAGS[]" (cadr project)))  " +") all-tags)))
  (dolist (other other-keyword-list)
     (setq all-tags (append (split-string (cadr (assoc "TAGS[]" (cadr other)))  " +") all-tags)))
  (delete-dups all-tags)
  ; generate .org files for all tags
  (dolist (tag all-tags)
    (with-temp-file (format "tag/%s.org" tag)
      (insert (format "#+TITLE: Pages tagged %s\n" tag)))

  (write-region (format "* Articles tagged ~%s~\n" tag) nil (format "tag/%s.org" tag) 'append))
  ; add entry of an article to its tag.org's
  (dolist (article article-keyword-list)
    (dolist (tag (split-string (cadr (assoc "TAGS[]" (cadr article)))  " +"))
      (write-region (format "- [[../%s][%s]]\n"
                            (car article)
                            (cadr (assoc "TITLE" (cadr article))))
                    nil (format "tag/%s.org" tag) 'append)))

  ; append "* Projects" headline
  (dolist (tag all-tags)
    (write-region (format "* Projects tagged ~%s~\n" tag) nil (format "tag/%s.org" tag) 'append))
  ; add entry of a project to its tag.org's
  (dolist (project project-keyword-list)
    (dolist (tag (split-string (cadr (assoc "TAGS[]" (cadr project)))  " +"))
      (write-region (format "- [[../%s][%s]]\n"
                            (car project)
                            (cadr (assoc "TITLE" (cadr project))))
                    nil (format "tag/%s.org" tag) 'append)))

  ; append "* Projects" headline
  (dolist (tag all-tags)
    (write-region (format "* Other tagged ~%s~\n" tag) nil (format "tag/%s.org" tag) 'append))
  ; add entry of a project to its tag.org's
  (dolist (other other-keyword-list)
    (dolist (tag (split-string (cadr (assoc "TAGS[]" (cadr other)))  " +"))
      (write-region (format "- [[../%s][%s]]\n"
                            (car other)
                            (cadr (assoc "TITLE" (cadr other))))
                    nil (format "tag/%s.org" tag) 'append)))
)

;; customize HTML output (see https://pank.eu/blog/blog-setup.html)
; see https://www.gnu.org/software/emacs/manual/html_node/org/Publishing-options.html
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
           ; :html-extension       ""                        ;; extension of generated html files
             :exclude "code.org"                 ;; regex of files to exclude NOTE excluding dirs seems to not work
           ; :include                                        ;; list of files to include
           ; :html-doctype "html5"                           ;; default is "xhtml-strict"
             :html-divs            '((preamble "header" "top")
                                     (content "main" "content")
                                     (postamble "footer" "postamble"))
             :html-html5-fancy     t
             :html-head            (concat "<title>andersch.dev</title>\n"
                                           "<link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">\n"
                                           "<link rel=\"stylesheet\" href=\"/style.css\">\n"
                                           ; NOTE import ubuntu font for now
                                           "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Ubuntu:regular,bold&subset=Latin\">"
                                           )
             :html-preamble        t
             :html-preamble-format `(("en" ,(with-temp-buffer (insert-file-contents "header.html") (buffer-string))))
             :html-postamble       nil                       ;; don't insert a footer with a date etc.

             :auto-sitemap         t                         ;; https://orgmode.org/manual/Site-map.html
             :sitemap-filename     "sitemap.org"             ;; ...
           ; :sitemap-title
             :sitemap-style        'tree                     ;; list or tree
             :sitemap-sort-files   'anti-chronologically
           ; :makeindex t                                    ;; https://orgmode.org/manual/Generating-an-index.html
             :with-title           nil                       ;; we include our own header
             :with-author          nil
             :with-creator         nil                       ;; don't include emacs and org versions in footer
             :with-toc             nil                       ;; no table of contents
             :section-numbers      nil                       ;; no section numbers for headings
             :html-validation-link nil                       ;; don't show validation link
             :time-stamp-file      nil                       ;; don't include "Created: <timestamp>" in footer
             :with-date            nil)))

; NOTE caching causes problems with updating titles etc., so we reset the cache before publishing
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-timestamp-directory "./.org-timestamps/")
(org-publish-remove-all-timestamps)

; NOTE workaround to not get a "Symbol’s function definition is void" error when publishing
(defun get-article-keyword-list () article-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
(defun get-project-keyword-list () project-keyword-list) ; NOTE workaround to pass keyword-list to a source-block in an org file
(defun get-other-keyword-list   () other-keyword-list)   ; NOTE workaround to pass keyword-list to a source-block in an org file

(org-publish "andersch.dev" t) ;; export html files
(message "Build complete")
