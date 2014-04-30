function sortwallpapers
	set -l widedir $HOME/Images/Fondos1080
	set -l dualdir $HOME/Images/Fondos3840

  command mkdir -p $widedir $dualdir

  for i in $HOME/Images/Fondos/*.{jpg,png,gif}
    set -l imgchar (identify -format '%w %h %[fx:w/h]' $i ^ /dev/null)
    set -l imgw    (echo $imgchar | awk '{print $1}')
    set -l imgh    (echo $imgchar | awk '{print $2}')
    set -l imgar   (echo $imgchar | awk '{print $3}')
    set -l destpath
    set -l sha1
    set -l ext

    if test \( $imgw -gt $imgh \) -a \( $imgh -ge 1080 \)
      set sha1 (sha1sum $i | awk '{print $1}')
      set ext (echo (basename $i) | sed 's/.*\.//')

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

      echo {$imgw}x{$imgh} $imgar "--" $i "->" $destpath
      if test ! -f $destpath
        cp -- $i $destpath
      end

    end
  end
end
