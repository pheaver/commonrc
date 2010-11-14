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
pathmunge ~/commonrc/bin
addpaths ~/local
addpaths ~

# -----------------------------------
