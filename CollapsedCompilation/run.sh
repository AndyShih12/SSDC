#!/bin/bash

set -x

benchmark="DBN_11"
benchmark_p=${benchmark}"_processed/"${benchmark}
litmap=${benchmark_p}".litmap"
uai=${benchmark_p}".uai"
vtree=${benchmark_p}".vtree"
sdd=${benchmark_p}"sdd/"
basecnf=${benchmark_p}".base.cnf"
uaievid=${benchmark_p}".uai.evid"


for i in 400000; do
  for j in 1 2 3 4; do
    comm="run -l "${litmap}" -u "${uai}" -v "${vtree}" -d "${sdd}" -q -1 -t 3 -s "${i}" -H rbvar -O rev -b "${basecnf}" -e "${uaievid}

    cp ./lib/newlib/libsdd.so ./lib/libsdd.so
    export LD_LIBRARY_PATH=./lib/ && sbt "${comm}" > log/${benchmark}_${i}_${j}_new.txt

    cp ./lib/oldlib/libsdd.so ./lib/libsdd.so
    export LD_LIBRARY_PATH=./lib/ && sbt "${comm}" > log/${benchmark}_${i}_${j}_old.txt

  done
done

