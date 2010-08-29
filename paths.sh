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
    pathmunge "$1/bin"
    pathmunge "$1/sbin"
    library-pathmunge "$1/lib"
    include-pathmunge "$1/include"
}

generic-pathmunge() {

    # examples names: PATH, LD_LIBRARY_PATH, LIBRARY_PATH, INCLUDE_PATH, C_INCLUDE_PATH
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
    if [[ -d "${pathdir}" && "${old_path}" != "*${pathdir}*" ]]; then

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

addpaths # /
addpaths /usr
addpaths /usr/local
addpaths /opt
addpaths /opt/local
pathmunge ~/commonrc/bin
#pathmunge ~/.cabal/bin
addpaths ~/local
addpaths ~

# pathmunge /sbin
# pathmunge /usr/sbin
# pathmunge /usr/local/bin
# pathmunge /usr/local/sbin
# pathmunge /opt/local/bin
# pathmunge /opt/local/sbin
# pathmunge ~/commonrc/bin
# pathmunge ~/.cabal/bin
# pathmunge ~/local/bin
# pathmunge ~/bin

# library-pathmunge /usr/lib
# library-pathmunge /usr/local/lib
# library-pathmunge /sw/lib
# library-pathmunge /opt/local/lib
# library-pathmunge ~/lib
# library-pathmunge ~/local/lib

# include-pathmunge /usr/local/include
# include-pathmunge /sw/include
# include-pathmunge /opt/local/include
# include-pathmunge ~/local/include

# man-pathmunge /usr/local/man
# man-pathmunge /opt/local/man
# man-pathmunge ~/local/man

