#!/bin/bash

# This script is used to compile a given sP file to the executable, 
# generating any intermediate files in the process
# Usage: compile.sh <sP file> <output file>
# Note: This script generates the .ll and .s files in the current working 
#       directory, not the directory of the sP file
#
# Written by: Team Nautilus

set -euo pipefail

LLC=llc

# check that llc is installed. If not, look for llc-14
if ! command -v $LLC &> /dev/null
then
    LLC=llc-14
    if ! command -v $LLC &> /dev/null
    then
        echo "llc could not be found. Please make sure llc or llc-14 is installed and in your PATH."
        exit
    fi
fi


# check arguments
if [ $# -ne 2 ]; then
    echo "Usage: compile.sh <file.sP> <exec>"
    exit 1
fi

sP_file=$1
if [ ! -f "$sP_file" ]; then
    echo "File $sP_file does not exist"
    exit 1
fi

sP_file_no_ext="${sP_file%.*}"

# only get the file name, not the path
sP_file_no_ext="${sP_file_no_ext##*/}"

exec=$2

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# run make to compile the compiler
rm -f "$script_dir"/builtins.o
make -C "$script_dir"

# run the compiler on the sP file
"$script_dir"/toplevel.native "$sP_file" > "$sP_file_no_ext".ll

# compile the llvm file to assembly
"$LLC" -relocation-model=pic "$sP_file_no_ext".ll -o "$sP_file_no_ext".s

# link it with builtins.o
gcc "$sP_file_no_ext".s "$script_dir"/builtins.o -o "$exec"
