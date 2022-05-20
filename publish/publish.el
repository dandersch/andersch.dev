; STUFF FOR #+BEGIN_SRC BLOCKS
;; Set the package installation directory so that packages aren't stored in the
; ;; ~/.emacs.d/elpa path.
; (require 'package)
; (setq package-user-dir (expand-file-name "./.packages"))
; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;                          ("elpa" . "https://elpa.gnu.org/packages/")))
; 
; ;; Initialize the package system
; (package-initialize)
; (unless package-archive-contents
;   (package-refresh-contents))
; 
; ;; Install dependencies
; (package-install 'htmlize)


(require 'ox-publish)

(defun update-article-list (path)
  (let* ((org-files (directory-files path nil ".org"))
         (articles  '(('title 'desc 'date))) ; TODO
         )
    (cd path)
    (mapcar (lambda (elem)
      (print elem)
      (set-buffer (find-file-noselect elem))
      (print (plist-get (org-export-get-environment) ':title))
      (print (plist-get (org-export-get-environment) ':date))
            )
            org-files)

    ;(org-publish-find-date FILE PROJECT)
    ;(print (thing-at-point 'line t))
    ;(print (org-entry-get (point-min) "ITEM"))
    ;(print (plist-get (org-export-get-environment) ':title))
    ;(print (org-export-get-environment))
    ;(print (plist-get org-export-options-alist ':title))
    ;(print (org-get-export-keywords))
    ;(print (org-heading-components))
    ; Get title of article, error if nil
    ; get description of header, error if nil
    (cd "..")
  )
)
;(update-article-list "./articles")
;(save-buffers-kill-terminal)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
;      org-html-head-include-scripts nil       ;; Use our own scripts
;      org-html-head-include-default-style nil ;; Use our own styles
;      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
       org-html-head "<style>
          body { background-color: #28292b; }
          #content { max-width: 60em; margin: auto; background-color: #28292b; color: #ffffff; }

          nav.nav {
            text-align: center;
            list-style-type: none;
            color: #ffffff;
            background-color: #18191b;
          }
          a:link {
            color: #46D0FF;
            /* background-color: yellow; */
          }
          a:visited {
            color: #0170BF;
          }
          a.nav-link {
            margin: 15px;
            color: #99BB66;
            background-color: #18191b;
          }

          .title  { text-align: center; margin-bottom: .2em; color: #B4916D; }
          .subtitle { text-align: center; font-size: medium; font-weight: bold; margin-top:0; }
          .tag    { background-color: #eee; font-family: monospace; padding: 2px; font-size: 80%; font-weight: normal; }
          .underline { text-decoration: underline; }
          #postamble { text-align: right; }
          #postamble p, #preamble p { font-size: 80%; margin: .2em; }

        </style>"
      )

;; see https://writepermission.com/org-blogging-rss-feed.html
;;     https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/ox-rss.el
(defun my-org-rss-publish (plist filename pub-dir)
  "Return output file name."
  (let ((bf (get-file-buffer filename)))
    (if bf
          (with-current-buffer bf
            (org-icalendar-create-uid filename 'warn-user)
            (org-rss-add-pubdate-property)
            (write-file filename))
      (find-file filename)
      (org-icalendar-create-uid filename 'warn-user)
      (org-rss-add-pubdate-property)
      (write-file filename) (kill-buffer)))
  (org-publish-org-to
   'rss filename ".rss" plist pub-dir))

(defun my-format-rss-feed (title list)
  (let* ((list-entries   (cdr list))
         (latest-article "")
         (latest-project ""))
    (mapcar (lambda (elem)
              (when (string= (car elem) "articles")
                  (setq latest-article (car (car (cdr (car (cdr elem))))))
                  (with-temp-file "articles.org"
                    (insert (concat
                             "#+TITLE: Articles\n"
                             ;"#+INCLUDE: \"header.html\" export html\n\n"
                             "* Articles\nThese are the articles I have written:\n"
                             (org-list-to-org (car (cdr elem)))))))
              (when (string= (car elem) "projects")
                  (setq latest-project (car (car (cdr (car (cdr elem))))))
                  (with-temp-file "projects.org"
                    (insert (concat
                             "#+TITLE: Projects\n"
                             ;"#+INCLUDE: \"header.html\" export html\n\n"
                             "* Projects\nThese are the projects I wrote:\n"
                             (org-list-to-org (car (cdr elem))))))))
            list-entries)
    (with-temp-file "home.org"
      (insert (concat
               "#+TITLE: Home of my website\n"
               ;"#+INCLUDE: \"header.html\" export html\n\n"
               "* Home\nThis is my website.\n"
               "- Latest article: " latest-article "\n"
               "- Latest project: " latest-project "\n"
               ))))

  (concat "#+TITLE: " title "\n\n" (org-list-to-org list))
)

(defun my-format-rss-feed-entry (entry style project)

  (when (and (string-match-p "articles/" entry) (not (string= entry "articles/")))
    (write-region ; TODO don't append
      (format "<item>\n<title>%s</title>\n<link>%s</link>\n<pubDate>%s</pubDate>\n</item>\n"
            (org-publish-find-title entry project)
            (concat "/" (string-replace ".org" ".html" entry))
            (format-time-string "%a, %d %m %Y %H:%M:%S" (seconds-to-time (org-publish-find-date entry project)))
            ; TODO 01 to Jan & end with (current-time-zone)
      )
      nil "feed_test.rss" 'append)
    )

  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]]"
             entry
             (org-publish-find-title entry project)))
        ((eq style 'tree)
          ;; Return only last subdir.
          (file-name-nondirectory (directory-file-name entry)))
        (t entry))

  ;; if entry == projects, add list to projects.org, add recent project to home.org
  ;; if entry == articles, add list to articles.org, add recent project to articles.org
)

;; Define the publishing project
(setq org-publish-project-alist
      (list
       ;(list "rss"
       ;      :recursive            t
       ;      :base-directory       "./"
       ;      :publishing-directory "./"
       ;      :publishing-function  'my-org-rss-publish        ; PLIST FILENAME PUB-DIR
       ;      :auto-sitemap         t
       ;      :sitemap-filename     "rss.org"
       ;      ;:sitemap-title rw-title
       ;      :sitemap-style        'list
       ;      :sitemap-sort-files   'chronologically
       ;      :sitemap-function     'my-format-rss-feed        ; defaults to org-publish-sitemap-default
       ;      :sitemap-format-entry 'my-format-rss-feed-entry) ; defaults to org-publish-sitemap-default-entry
       ;      )
       (list "andersch.xyz"
             ;; check https://orgmode.org/manual/Publishing-options.html for more options
             :recursive t
             :base-directory "./"
             :publishing-directory "./"
             :publishing-function 'org-html-publish-to-html ;; may be a list of functions
           ; :preparation-function      ;; called before publishing
           ; :completion-function       ;; called after
           ; :base-extension            ;; extension of source files
           ; :exclude                   ;; regex of files to exclude
           ; :include                   ;; list of files to include

             :auto-sitemap t
             :sitemap-filename     "rss.org"                 ;; ...
           ; :sitemap-title
             :sitemap-style        'tree                     ;; list or tree
             :sitemap-sort-files   'anti-chronologically
             :sitemap-function     'my-format-rss-feed
             :sitemap-format-entry 'my-format-rss-feed-entry

             :with-title nil            ;; we include our own header
             :with-author nil           ;; Don't include author name
             :with-creator nil          ;; Include Emacs and Org versions in footer
             :with-toc nil              ;; Include a table of contents
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file nil))    ;; Don't include time stamp in file
      )

(defun update-article-list (path) )

;; TODO see sitemap: https://orgmode.org/manual/Site-map.html
;; TODO see index:   https://orgmode.org/manual/Generating-an-index.html

; TODO remove hardcoded rss header
(with-temp-file "feed_test.rss"
  (insert
   (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
           "<rss version=\"2.0\">\n"
           "<channel>\n"
           "<title>My Website</title>\n"
           "<!-- <lastBuildDate>Wed, 15 Dec 2021 00:00:00 +0000</lastBuildDate> -->\n"
           "<!--- <atom:link href=\"/article/index.xml\" rel=\"self\" type=\"application/rss+xml\"/> -->\n"
           "<!-- TODO use relative links -->\n"
           "<link>localhost:8000/home.html</link>\n"
           "<description>Stuff on programming</description>\n"
           "<language>en-us</language>\n")))

;; Generate the site output
(org-publish-all t)

; TODO remove hardcoded rss ending
(write-region "</channel>\n</rss>" nil "feed_test.rss" 'append)

(message "Build complete")
