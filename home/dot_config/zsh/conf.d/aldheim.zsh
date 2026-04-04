# Display outputs
output_left="DisplayPort-1"
output_right="DisplayPort-2"

# Restore default layout (matches ~/.xinitrc)
alias snormal="xrandr --output $output_left --primary --pos 0x1000 --output $output_right --rotate right --pos 3840x0"

# Single monitor (right display off)
alias ssingle="xrandr --output $output_right --off"

# Left monitor at lower resolutions (right display adjusts position accordingly)
alias s1440="xrandr --output $output_right --auto --pos 2560x0 --rotate right --output $output_left --mode 2560x1440 --pos 0x1180"
alias s1080="xrandr --output $output_right --auto --pos 1920x0 --rotate right --output $output_left --mode 1920x1080 --pos 0x1400"

# vim:ft=zsh
