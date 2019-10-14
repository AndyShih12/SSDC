# All-Marginals Experiments

Counting the average number of operations (+,-,x,/) on the weighted model count done per All-Marginals pass.

To run, edit the run.sh script to select the network name, and to specify the limit of the constructed SDD sizes.
Then invoke ./run.sh.

The results will be in the log/ folder, of format "name_size_iter_new.txt" (new algorithm) or "name_size_iter_old.txt" (naive algorithm).
For example, "Segmentation_11_400000_1_new.txt" will store the results for the first run on the Segmentation_11 network using the
new algorithm.

The code for the new algorithm can be found in ../WMC_fast/src/sdds/wmc.c , and the code for the naive algorithm can be found in ../WMC/src/sdds/wmc.c

The rest of the README is taken directly from the github repo of this paper: "Approximate Knowledge Compilation by Online Collapsed Importance Sampling".

# Collapsed-Compilation

Requirements: 
Scala
SBT
Jython standalone jar

Most of the necessary preprocessing is handled by run_pipeline. It requires a single argument (the uai file), and will output a directory that looks like DBN_11_processed. The source of run_pipeline.sh should be modified with the appropriate location for your jython jar.

50-20-5_processed is an example of an experiment that has a logical base which we use. The files 50-20-5.sdd and 50-20-5.vtree.out were created by the SDD tool available at http://reasoning.cs.ucla.edu/sdd/. 

Some example experiments from the paper:

For experiments that don't use a logical base:

export LD_LIBRARY_PATH=./lib/ && 
sbt "run -l DBN_11_processed/DBN_11.litmap -u DBN_11_processed/DBN_11.uai -v DBN_11_processed/DBN_11.vtree -d DBN_11_processed/DBN_11sdd/ -q -1 -t 3600 -s 1000000 -H rb -O rev -b DBN_11_processed/DBN_11.base.cnf  -e DBN_11_processed/DBN_11.uai.evid" 

This uses the rao-blackwell variance minimizing heuristic, reverse ordering, a size limit of 100,000 nodes, the last variable as the query, and runs for 1 hour.

For experiments that do use a logical base:
export LD_LIBRARY_PATH=./lib/ &&
sbt "run -l 50-20-5_processed/50-20-5.litmap -u 50-20-5_processed/50-20-5.uai -v 50-20-5_processed/50-20-5.vtree -d 50-20-5_processed/50-20-5sdd/ -q -1 -t 600 -s 1000000 -H fd -O rev -B 50-20-5_processed/50-20-5.sdd -V 50-20-5_processed/50-20-5.vtree.out -M -E -e 50-20-5_processed/50-20-5'.uai.evid' 

This uses the frontier distance heuristic, reverse ordering, a size limit of 100,000 nodes, the last variable as the query, and runs for 10 minutes. It also uses a precompiled oracle for the logical base, and applies entailment checking after conditioning.
