#!/bin/sh

terminate_program()
{
    echo "Terminating program running in background"
    if [[ -n "$bg_pid" ]]; then
        kill "$bg_pid"
    fi
}

./build.sh

trap terminate_program EXIT # call on exit

# serve webpage if not already serving
if [[ -z $(ss -tulpn | grep 1337) ]]
then
       python -m http.server --dir ./ 1337 &
       bg_pid=$!
       firefox -new-tab http://localhost:1337
fi

inotifywait --recursive --exclude "flycheck_publish.el|.git|.packages|feed.rss|index.org|sitemap|tag" --monitor --event modify ./ |
   while read file_path file_event file_name; do
       if [[ ${file_name} =~ .org$|publish.el ]]
       then
              echo ${file_path}${file_name} event: ${file_event}
              ./build.sh

              # refresh firefox tab
              CURRENT_WID=$(xdotool getwindowfocus)
              WID=$(xdotool search --name "Mozilla Firefox")
              xdotool windowactivate $WID
              xdotool key ctrl+F5
              xdotool windowactivate $CURRENT_WID

              sleep 5
       fi
   done
