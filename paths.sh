#-*-sh-*-

# -----------------------------------
# paths
# -----------------------------------

# given a path p, handle all of the following:
#   PATH += $p/sbin $p/bin
#   {LD_}LIBRARY_PATH += $p/lib
#   {C_,CPLUS_}INCLUDE_PATH += $p/include
#   MANPATH += $p/man
addpaths() {
    # remove trailing slash if it exists
    p="${1%/}"

    pathmunge "$p/sbin"
    pathmunge "$p/bin"
    library-pathmunge "$p/lib"
    include-pathmunge "$p/include"
    man-pathmunge "$p/man"
}

generic-pathmunge() {

    # example names: PATH, LD_LIBRARY_PATH, LIBRARY_PATH, INCLUDE_PATH, C_INCLUDE_PATH
    name=$1
    pathdir="$2"

    # variable indirection is different on bash and zsh
    case ${shell} in
        bash) old_path=${!name};;
        zsh) old_path=${(P)name};;
        *) echo "In generic-pathmunge, bad value: shell=$shell"; return 1;;
    esac

    # make sure directory exist.

    # make sure pathdir isn't already in the path variable.
    # this doesn't work:
    # "${old_path}" != "*${pathdir}*"

    if [[ -d "${pathdir}" ]]; then

        if [ -z "${old_path}" ]; then
            export $name="${pathdir}"
        elif [ "$3" = "after" ]; then
            export $name="${old_path}":"${pathdir}"
        else
            export $name="${pathdir}":"${old_path}"
        fi
        return 0
    else
        return 1
    fi
}

pathmunge() {
    generic-pathmunge PATH "$1" "$2"
}

library-pathmunge() {
    generic-pathmunge LD_LIBRARY_PATH "$1" "$2"
    generic-pathmunge LIBRARY_PATH "$1" "$2"
}

include-pathmunge() {
    generic-pathmunge INCLUDE_PATH "$1" "$2"
    generic-pathmunge C_INCLUDE_PATH "$1" "$2"
    generic-pathmunge CPLUS_INCLUDE_PATH "$1" "$2"
}

man-pathmunge () {
    generic-pathmunge MANPATH "$1" "$2"
}

clear-paths () {
    export PATH=
    export LD_LIBRARY_PATH=
    export LIBRARY_PATH=
    export INCLUDE_PATH=
    export C_INCLUDE_PATH=
    export CPLUS_INCLUDE_PATH=
    export MANPATH=
}

# -----------------------------------

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
