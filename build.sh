emacs -Q --script publish.el

#firefox -new-tab http://andersch.dev/
# python -m http.server --dir ../publish 1337

#inotifywait --recursive --excludei ".git|.packages|.org-timestamps|feed.rss" --monitor --event modify ./ | 
inotifywait --recursive --includei "style.css|code.css|header.html|opengl*" --monitor --event modify ./ | 
   while read file_path file_event file_name; do 
       echo ${file_path}${file_name} event: ${file_event}
       emacs -Q --script publish.el

       # refresh firefox tab
       CURRENT_WID=$(xdotool getwindowfocus)
       WID=$(xdotool search --name "Mozilla Firefox")
       xdotool windowactivate $WID
       xdotool key F5
       xdotool windowactivate $CURRENT_WID
   done
