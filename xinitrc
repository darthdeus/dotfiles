#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then

    xrdb -merge $sysresources

fi

# TODO: handle possibly missing xmodmap

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then

    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# TODO -
# xsetroot -cursor_name left_ptr

# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login

# exec xmonad

if command -v numlockx; then
  numlockx on
fi

# sxhkd &
# st -e "tmux" &
# exec bspwm

# exec awesome

# exec twm


# if command -v autocutsel; then
#   autocutsel -fork &
#   autocutsel -selection PRIMARY -fork &
# fi

if [[ "$(hostname)" == "pipik" ]]; then
  if command -v xrandr; then
    # xrandr --output HDMI-0 --off \
    #   --output HDMI-1 --off \
    #   --output HDMI-2 --mode 1920x1200 --pos 0x240 --rotate normal \
    #   --output DP-0 --off \
    #   --output DP-1 --off \
    #   --output DP-2 --off \
    #   --output DP-3 --off --output DP-4 --mode 2560x1440 --pos 1920x0 --rotate normal --output DP-5 --off

    # xrandr --output HDMI-0 --mode 2560x1440 --pos 0x0 --rotate normal \
    #   --output DP-2 --mode 3440x1440 --pos 2560x0 --rotate normal --primary

    # xrandr \
    #   --output DP-2 --mode "2560x1440" --pos 0x0 --rotate normal \
    #   --output DP-1 --mode "3440x1440" --pos 2560x0 --rotate normal --primary

    # xrandr \
    #   --output DisplayPort-1 --mode "2560x1440" --pos 0x0 --rotate normal \
    #   --output DisplayPort-0 --mode "3440x1440" --pos 2560x0 --rotate normal --primary

    # xrandr \
    #   --output DisplayPort-1 --mode 5120x2160 --pos 0x0 --rotate normal --primary \
    #   --output HDMI-A-0 --mode 3840x2160 --pos -3840x0 --rotate normal

    # xrandr \
    #   --output DP-0 --mode 5120x2160 --pos 0x0 --rotate normal --primary \
    #   --output HDMI-0 --mode 3840x2160 --pos -3840x0 --rotate normal

    # xrandr \
    #   --output HDMI-0 --mode 3840x2160 --pos -3840x0 --rotate normal \
    #   --output DP-2 --mode 3840x2160 --pos 0x0 --rotate normal --primary \
    #   --output DP-0 --mode 3840x2160 --pos 3840x0 --rotate normal
    #
    echo ":( resolution lol"
  else
    echo "xrandr not installed"
  fi
fi

# systemctl start --user sxhkd.service
if command -v sxhkd; then
  sxhkd &
fi

if command -v autocutsel; then
  autocutsel -selection CLIPBOARD -fork &
fi

# if command -v copyq; then
#   copyq &
# fi

if command ssh-agent; then
  eval $(ssh-agent)
fi

# if command -v picom; then
#   picom -D0 -o1 -i1 --vsync &
# fi

pactl set-default-sink 190

# exec startxfce4
exec i3

# while :; do
#   "$HOME/.dotfiles/bin/dwm"
# done

# exec "$HOME/.dotfiles/dwm/dwm"

# "$HOME/.dotfiles/dwm/dwm" &
# exec "$HOME/.dotfiles/sleeper.sh"

# exec i3
# exec awesome

# exec herbstluftwm --locked
# xrandr --output VGA-1 --auto --right-of HDMI-2
