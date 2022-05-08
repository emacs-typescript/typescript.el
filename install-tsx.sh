#!/bin/bash

lang=$1

if [ $(uname) == "Darwin" ]
then
    soext="dylib"
else
    soext="so"
fi

# Retrieve sources.
git clone "https://github.com/tree-sitter/tree-sitter-typescript.git" \
    --depth 1
cd "tree-sitter-typescript/tsx/src"

# Build.
cc -c -I. parser.c
# Compile scanner.c.
cc -fPIC -c -I. scanner.c
# Link.
cc -fPIC -shared *.o -o "libtree-sitter-tsx.${soext}"

mkdir -p ../../../dist
cp "libtree-sitter-tsx.${soext}" ../../../dist
cd ../../../
rm -rf "tree-sitter-tsx"
