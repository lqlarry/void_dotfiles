xrandr --output HDMI3 --same-as LVDS1 &
feh --bg-scale ~/.wal/linux-force.jpg &
udiskie &
# nitrogen --restore &
picom --config ~/.config/picom/picom.conf &
# blueman-applet &
numlockx &
mpd &
sleep 5 & /usr/libexec/polkit-gnome-authentication-agent-1 &
sleep 10 & xset s 0 0 &
sleep 10 & xset s noblank &
sleep 10 & xset dpms 0 0 0 &

