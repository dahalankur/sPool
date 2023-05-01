#!/bin/bash

# This script is used to compile a given sP file to the executable, 
# generating any intermediate files in the process
# Usage: compile.sh [-stdlib] <sP file> <output file>
# Note: This script generates the .ll and .s files in the current working 
#       directory and not in the directory where the sP file is present
#
# Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

set -euo pipefail

LLC=llc

# check that llc is installed. If not, look for llc-14
if ! command -v $LLC &> /dev/null
then
    LLC=llc-14
    if ! command -v $LLC &> /dev/null
    then
        echo "llc could not be found. Please make sure llc or llc-14 is installed and in your PATH."
        exit 1
    fi
fi

import=0

# check arguments
if [ $# -lt 2 ] || [ $# -gt 3 ]; then
    echo "Usage: compile.sh [-stdlib] <file.sP> <exec>"
    exit 1
fi

sP_file=$1
exec=$2

if [ $# -eq 3 ] && [ "$1" = "-stdlib" ]; then
    import=1
    sP_file=$2
    exec=$3
elif [ $# -eq 3 ] && [ "$1" != "-stdlib" ]; then
    echo "Usage: compile.sh [-stdlib] <file.sP> <exec>"
    exit 1
fi

if [ ! -f "$sP_file" ]; then
    echo "File $sP_file does not exist"
    exit 1
fi

sP_file_no_ext="${sP_file%.*}"
sP_file_no_ext="${sP_file_no_ext##*/}"
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# run make to compile the compiler
rm -f "$script_dir"/*.o
make -C "$script_dir"

# run the compiler on the sP file
if [ $import -eq 1 ]; then
    "$script_dir"/toplevel.native -i "$sP_file" > "$sP_file_no_ext".ll
else
    "$script_dir"/toplevel.native "$sP_file" > "$sP_file_no_ext".ll
fi

# compile the llvm file to assembly
"$LLC" -relocation-model=pic "$sP_file_no_ext".ll -o "$sP_file_no_ext".s

# link it with builtins.o and the pthread library
gcc -O2 -pthread "$sP_file_no_ext".s "$script_dir"/builtins.o "$script_dir"/list.o "$script_dir"/store.o -o "$exec"
