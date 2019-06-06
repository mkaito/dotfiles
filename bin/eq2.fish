#!/bin/fish

# 1.7.14-1.7.46 works
# 1.7.40 had texture issues
# 1.7.47 crashes the wineserver

set -gx WINEARCH win32
set -gx WINEPREFIX $HOME/.local/share/wineprefixes/eq2
# set -gx WINEPATH "/usr/bin"
# set -gx WINEPATH $HOME/.wine-versions/eq2/bin
# set -gx WINE $WINEPATH/wine
set -gx WINE /opt/wine-staging/bin/wine
set -gx EQ2PATH "Program Files/Daybreak Game Company/Installed Games/EverQuest II"
set -gx EQ2PATCH (echo "C://$EQ2PATH/LaunchPad.exe" | sed 's/ /\\\ /g' )
set -gx EQ2GAME (echo "C://$EQ2PATH/EverQuest2.exe" | sed 's/ /\\\ /g' )
set -gx UIBUILDER (echo "C://$EQ2PATH/UIBuilder/EQ2UIBuilder.exe" | sed 's/ /\\\ /g' )
# set -gx EQ2PATCH ~/media/eq2/LaunchPad.exe

## Wine is really verbose. So much that it affects performance.
# set -gx WINEDEBUG "-all"

## This ought to improve performance on Nvidia cards, but can cause other
## issues.
# set -gx LD_PRELOAD "libpthread.so.0 libGL.so.1"
# set -gx __GL_THREADED_OPTIMISATIONS 1

## Sync to vblank is enabled by default, but you might need to configure the
## sync device on multi-head systems.
# set -gx __GL_SYNC_TO_VBLANK 1
# set -gx __GL_SYNC_DISPLAY_DEVICE "DFP-2"

# Apparently, $_ contains the used executable name?

if set -q argv
  switch $argv[1]
    case reset
      echo "This will completely remove $WINEPREFIX. This can not be undone."
      read -l confirm -p "echo 'Are you sure? [Y/n] '"
      switch $confirm
        case y ""
          echo Removing $WINEPREFIX.
          rm -rf $WINEPREFIX
        case n
          echo Aborting.
      end

    ## Clean up after a crash
    case kill
        ps ux | grep -v grep | grep -v "eq2 kill" | \
        grep -i "windows\|wineserver\|everquest\|awesomium\|winetricks\|winedbg\|explorer.exe" | \
        awk '{print $2}' | xargs kill -KILL

    case config
        eval $WINEPATH/winecfg

    case regedit
        eval $WINEPATH/regedit

    case ui
        cd $WINEPREFIX/drive_c/"$EQ2PATH"
        eval $WINE $UIBUILDER

    case install
        ## The full client seems to have issues with the launcher. It's supposed to
        ## be exactly the same launcher, but it isn't. Don't ask me why. The
        ## streaming launcher works. The full launcher crashes when it closes itself.

        ## Full client
        set -l installer_url "https://launch.daybreakgames.com/installer/EQ2_setup.exe"
        set -l installer_md5 'dda87fab3af318900f53f30b3d6c65fd'

        ## Streaming client
        # set -l installer_url "http://launch.soe.com/installer/EQ2_Streaming_setup.exe"
        # set -l installer_md5 '4e110e353367b5409447de69dc738dff'

        set -l installer $HOME/.cache/(basename $installer_url)

        set -l winetricks corefonts
        set -l regtricks vd=1920x1080 psm=0

        ## Do not overwrite. Run reset first.
        if test -d $WINEPREFIX
          echo Destination WINEPREFIX exists. Aborting. Run (basename (status -f)) reset.
          exit
        end

        winetricks $regtricks $winetricks

        ## The launcher tends to crash when it runs right after installing. This
        ## fixes it, sometimes. If it crashes anyway, just run `eq2` again to
        ## run the installed patcher.
        eval $WINEPATH/wineboot

        if not test -f $installer
          curl -o $installer $installer_url
        end

        if test -f $installer
          set -l md5 (md5sum $installer)
          if test $md5 = "$installer_md5  $installer"
            eval $WINE $installer
          else
            echo Installer did not pass md5 check. Removing and downloading again.

            rm $installer
            curl -o $installer $installer_url

            set -l md5 (md5sum $installer)
            # if test $md5 = "$installer_md5  $installer"
            eval $WINE $installer
            # else
            # echo Downloaded installer again, still did not pass md5 check. Aborting.
            # end
          end
        else
          echo The installation file could not be found at $installer.
        end
  end
else
  eval $WINE $EQ2PATCH
end
