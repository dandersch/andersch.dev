; execute this file as script: 
; $ emacs -Q --script publish.el
(switch-to-buffer (find-file-noselect "test.txt" nil nil nil))
(replace-regexp-in-region "here" "here__" nil nil)
(save-buffer)

; TODO add alternative
; (while re-search-forward  (replace-match "here__))

; NOTE shitty workaround
;(switch-to-buffer (find-file-noselect "test.txt" nil nil nil))
;(setq buf-str (buffer-string))
;(with-temp-file "test.txt"
;  (insert (replace-regexp-in-string "here" "here______" buf-str nil t)))
