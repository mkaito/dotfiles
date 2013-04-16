# {{{ Android phone sync (PS) and backup
PS_BACKUP_DIR=~/Android/Backup	  # Where to keep backups
PS_BACKUP_KEEP=60		# Days to keep backups
PS_BACKUP_ENCRYPT_TO="0x6866852C" # GPG key ID to sign and encrypt to

PS_IP="192.168.1.3"		# The IP your device will be found at, for pinging
PS_SSH="Nexus"			# The SSH host of your phone, or an SSH alias
PS_STAGING=~/Android/Sync	# The staging folder for sync, sans trailing slash
PS_SDCARD=/sdcard		# The sync root on the phone, sans trailing slash

function ps_probe() {
    ping -c 1 ${PS_IP} > /dev/null
    if [[ $? == 0 ]]; then
        $@
        return 0
    else
        echo "The phone could not be located on the network"
        return 1
    fi
}

function ps_pull() {
    ps_probe && rsync -vzuLr --no-perms --exclude Music/ --exclude Android/ ${PS_SSH}:${PS_SDCARD}/ ${PS_STAGING} && ps_backup_create
}

function ps_push() {
    ps_probe && rsync -vzuLr --no-perms --delete-after ${PS_STAGING}/ ${PS_SSH}:${PS_SDCARD}
}

function ps_backup_create() {
    t=$(mktemp -d)
    ts=$(mktemp -d)

    cp -a ${PS_STAGING}/* $t
    [[ $? != 0 ]] && echo "Failed to copy files to tmp folder." && return 1

    fn="${ts}/$(date +%Y-%m-%d).tar.bz2"
    tar -cjpsf $fn ${t}/* --exclude=Music
    [[ $? != 0 ]] && echo "Failed to create snapshot archive." && return 1

    gpg2 -se -r $PS_BACKUP_ENCRYPT_TO $fn
    mv -f "${fn}.gpg" $PS_BACKUP_DIR/
    rm -rf $t
    rm -rf $ts

    ps_backup_rotate
}

function ps_backup_rotate() {
    find $PS_BACKUP_DIR -mtime $PS_BACKUP_KEEP -delete
}

function ps_backup_restore() {
    # TODO: Date parsing to restore specific backup.

    # Create a new backup first, if no current backup is found
    if [[ ! -f "${PS_BACKUP_DIR}/$(date +%Y-%M-%d).tar.bz2.gpg" ]]; then
        echo "Creating a backup first..."
        ps_backup_create
    fi
    
    if [[ -f "$1" ]]; then
        rm -rf ${PS_STAGING}/*
        gpg2 -d $1 | tar xjf - -C $PS_STAGING --strip-components 2
    fi
}
# }}}
