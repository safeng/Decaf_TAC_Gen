#!/bin/bash

##** test.sh - A simple test framework ********************************

make clean
if make
then
    for src in samples/*.decaf
    do
        base=${src%.decaf}
        if ./dcc < ${src} &> ${base}.test.s;
        then
            if [ -e ${base}.in ];
            then
                cat ${base}.in | spim -file ${base}.test.s | tail -n +6 > ${base}.test.out
                cat ${base}.in | spim -file ${base}.s | tail -n +6 > ${base}.out
            else
                spim -file ${base}.test.s | tail -n +6 > ${base}.test.out &
                PROC=$!
                (sleep 0.1; kill $PROC) &
                fg $PROC
                spim -file ${base}.s | tail -n +6 > ${base}.out
            fi
        else
            touch ${base}.test.out
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
