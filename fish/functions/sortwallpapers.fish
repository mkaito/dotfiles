function sortwallpapers
	set -l widedir $HOME/Images/Fondos/1.77
  set -l dualdir $HOME/Images/Fondos/3.55
  set -l portraitdir $HOME/Images/Fondos/0.56

  command mkdir -p $widedir $dualdir $portraitdir

  for i in $HOME/Images/Fondos/*.{jpg,png,gif}
    set -l imgchar (identify -format '%w %h %[fx:w/h]' $i ^ /dev/null)
    set -l imgw    (echo $imgchar | awk '{print $1}')
    set -l imgh    (echo $imgchar | awk '{print $2}')
    set -l imgar   (echo $imgchar | awk '{print $3}')
    set -l sha1    (sha1sum $i | awk '{print $1}')
    set -l ext     (echo (basename $i) | sed 's/.*\.//')
    set -l destpath
    set -e imgchar

    ## Check for landscape format and minimum width
    if test \( $imgw -gt $imgh \) -a \( $imgw -ge 1920 \)

      ## Dual 16:9 wallpaper
      if test $imgar = "3.55556"
        set destpath $dualdir/$sha1.$ext

      ## Single 16:9 wallpaper
      else if test $imgar = "1.77778"
        set destpath $widedir/$sha1.$ext

      ## Wrong aspect ratio.
      else
        continue
      end

      if test ! -f $destpath
        echo L {$imgw}x{$imgh} $imgar "--" $i "->" $destpath
        cp -- $i $destpath
      end

    ## Portrait
    else if test $imgh -ge 1920

      ## Single 9:16 wallpaper
      if test $imgar = "0.5625"
        set destpath $portraitdir/$sha1.$ext

      else
        continue
      end

      if test ! -f $destpath
        echo P {$imgw}x{$imgh} $imgar "--" $i "->" $destpath
        cp -- $i $destpath
      end
    end
  end
end
