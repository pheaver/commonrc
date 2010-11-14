#-*-sh-*-

# default $PATH is usually something like this:
# /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin

clear-paths
addpaths /usr/X11
addpaths /usr/local
addpaths /usr
addpaths /
addpaths /opt
addpaths /opt/local
addpaths ~/homebrew
pathmunge "${RC}/gntp-send.git/bin"
pathmunge "${RC}/bin"
addpaths ~/local
addpaths ~

# -----------------------------------
