#!/bin/sh

if [ $# -ne 2 ]; then
  echo "Usage: `basename $0` timestamp_src dest_file";
  exit 1;
fi

function printtimestamp()
{
  stat -n -f "%Sm" -t "%Y%m%d%H%M" "$1"
}

SRC=$1
DST=$2

if [ ! -f "${SRC}" ]; then
  echo "Error: ${SRC} does not exist.";
  exit 1;
fi;

TIME_SRC=`printtimestamp "${SRC}"`;

touch -t ${TIME_SRC} "${DST}"
