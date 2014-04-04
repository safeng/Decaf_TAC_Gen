#!/bin/bash

##** test.sh - A simple test framework ********************************

make clean
if make
then
    for src in samples/*.decaf
    do
        base=${src%.decaf}
        ./dcc < ${src} &> ${base}.test.s
        if [ -e ${base}.in ];
        then
            cat ${base}.in | spim -file ${base}.test.s | tail -n +6 > ${base}.test.out
        else
            spim -file ${base}.test.s | tail -n +6 > ${base}.test.out
        fi
        if diff -urw ${base}.test.out ${base}.out > /dev/null
        then
            echo -e "\e[32m${base}.decaf\e[0m"
        else
            echo -e "\e[31m${base}.decaf\e[0m"
            diff -urw ${base}.test.out ${base}.out
        fi
    done
    rm samples/*.test.s samples/*.test.out
else
    echo -e "\e[31mBuild failed\e[0m"
fi
