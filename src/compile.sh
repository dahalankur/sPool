#!/bin/bash

set -euo pipefail

# This script is used to compile a given sP file to the executable, 
# generating any intermediate files in the process
# Usage: compile.sh <sP file> <output file>

# check the args
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
make -C "$script_dir"

# run the compiler on the sP file
"$script_dir"/toplevel.native "$sP_file" > "$sP_file_no_ext".ll

# compile the llvm file to an executable
llc "$sP_file_no_ext".ll -o "$sP_file_no_ext".s

# link it with builtins.o
gcc "$sP_file_no_ext".s "$script_dir"/builtins.o -o "$exec"