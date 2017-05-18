#-*-sh-*-

# ----------------------------------
# mail notification

if [ -e ${HOME}/.maildir ]; then
   NEWMAIL=`/usr/bin/find ${HOME}/.maildir/new/ -type f`

    if [ ! -z "$NEWMAIL" ]; then
        echo "You have new mail."
    fi
fi

# ----------------------------------

