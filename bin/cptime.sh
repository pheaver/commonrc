#!/bin/bash

if [ $# -ne 2 ]; then
  echo "Usage: `basename $0` timestamp_src dest_file";
  exit 1;
fi

function printtimestamp()
{
  # OS X
  # stat -n -f "%Sm" -t "%Y%m%d%H%M" "$1"

  # Linux (Ubuntu)
  stat --format "%y" "$1"
}

SRC=$1
DST=$2

if [ ! -f "${SRC}" ]; then
  echo "Error: ${SRC} does not exist.";
  exit 1;
fi;

TIME_SRC=`printtimestamp "${SRC}"`;

# OS X
# touch -t ${TIME_SRC} "${DST}"

# Linux (Ubuntu)
touch -m --date="${TIME_SRC}" "${DST}"
