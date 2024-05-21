#!/bin/bash
firefox -new-tab http://localhost
sudo python -m http.server --dir ./ 80
