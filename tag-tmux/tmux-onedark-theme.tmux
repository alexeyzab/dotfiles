#!/bin/bash
#onedark_black="#282c34"
onedark_blue="#61afef"
onedark_yellow="#e5c07b"
onedark_red="#e06c75"
onedark_white="#aab2bf"
onedark_green="#98c379"
onedark_visual_grey="#3e4452"
onedark_comment_grey="#5c6370"

get() {
   local option=$1
   local default_value=$2
   local option_value="$(tmux show-option -gqv "$option")"

   if [ -z "$option_value" ]; then
      echo "$default_value"
   else
      echo "$option_value"
   fi
}

set() {
   local option=$1
   local value=$2
   tmux set-option -gq "$option" "$value"
}

setw() {
   local option=$1
   local value=$2
   tmux set-window-option -gq "$option" "$value"
}

set "status" "on"
set "status-justify" "left"

set "status-left-length" "100"
set "status-right-length" "100"
set "status-right-attr" "none"

set "message-fg" "$onedark_white"
set "message-bg" "#282c34"

set "message-command-fg" "$onedark_white"
set "message-command-bg" "#282c34"

set "status-attr" "none"
set "status-left-attr" "none"

setw "window-status-fg" "#282c34"
setw "window-status-bg" "#282c34"
setw "window-status-attr" "none"

setw "window-status-activity-bg" "$onedark_black"
setw "window-status-activity-fg" "$onedark_black"
setw "window-status-activity-attr" "none"

setw "window-status-separator" ""

set "window-style" "fg=$onedark_comment_grey,bg=$onedark_black"
set "window-active-style" "fg=$onedark_white,bg=$onedark_black"

set "pane-border-fg" "$onedark_white"
set "pane-active-border-fg" "$onedark_white"

set "display-panes-active-colour" "$onedark_yellow"
set "display-panes-colour" "$onedark_blue"

#set "status-bg" "$onedark_black"
set "status-bg" "#282c34"
set "status-fg" "$onedark_white"

set "@prefix_highlight_fg" "#282c34"
set "@prefix_highlight_bg" "$onedark_green"
set "@prefix_highlight_copy_mode_attr" "fg=#282c34,bg=$onedark_green"
set "@prefix_highlight_output_prefix" "  "

status_widgets=$(get "@onedark_widgets")
time_format=$(get "@onedark_time_format" "%I:%M %p")
date_format=$(get "@onedark_date_format" "%D")

set "status-right" "#[fg=$onedark_white,bg=#282c34,nounderscore,noitalics]${time_format}  ${date_format} #[fg=$onedark_visual_grey,bg=#282c34]#[fg=$onedark_visual_grey,bg=$onedark_visual_grey]#[fg=$onedark_white, bg=$onedark_visual_grey]${status_widgets} #[fg=$onedark_green,bg=$onedark_visual_grey,nobold,nounderscore,noitalics]#[fg=#282c34,bg=$onedark_green,bold] #h #[fg=$onedark_yellow, bg=$onedark_green]#[fg=$onedark_red,bg=$onedark_yellow]"
set "status-left" "#[fg=$onedark_visual_grey,bg=$onedark_green,bold] #S #{prefix_highlight}#[fg=$onedark_green,bg=#282c34,nobold,nounderscore,noitalics]"

set "window-status-format" "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=$onedark_white,bg=#282c34] #I  #W #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"
set "window-status-current-format" "#[fg=#282c34,bg=$onedark_visual_grey,nobold,nounderscore,noitalics]#[fg=$onedark_white,bg=$onedark_visual_grey,nobold] #I  #W #[fg=$onedark_visual_grey,bg=#282c34,nobold,nounderscore,noitalics]"
# set "window-status-current-format" '#[fg=white,bold]** #{window_index} #[fg=green]#{pane_current_command} #[fg=blue]#(echo "#{pane_current_path}" | rev | cut -d'/' -f-3 | rev) #[fg=white]**|'
# set "window-status-format" '#[fg=white,bold]#{window_index} #[fg=green]#{pane_current_command} #[fg=blue]#(echo "#{pane_current_path}" | rev | cut -d'/' -f-3 | rev) #[fg=white]|'
