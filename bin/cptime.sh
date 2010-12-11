#!/bin/bash

set -e

if [ $# -ne 2 ]; then
  echo "Usage: `basename $0` timestamp_src dest_file";
  exit 1;
fi

UNAME=`uname`

function printtimestamp()
{
    case ${UNAME} in
        Darwin)
            stat -n -f "%Sm" -t "%Y%m%d%H%M" "$1";;
        Linux)
            stat --format "%y" "$1";;
    esac
}

SRC=$1
DST=$2

if [ ! -f "${SRC}" ]; then
  echo "Error: ${SRC} does not exist.";
  exit 1;
fi;

TIME_SRC=`printtimestamp "${SRC}"`;

case ${UNAME} in
    Darwin)
        touch -t ${TIME_SRC} "${DST}";;
    Linux)
        touch -m --date="${TIME_SRC}" "${DST}";;
esac
