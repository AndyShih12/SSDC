This repository contains code for the paper:

"Smoothing Structured Decomposable Circuits"
Andy Shih, Guy Van den Broeck, Paul Beame, Antoine Amarilli
In Advances in Neural Information Processing Systems 32 (NeurIPS), 2019

Instructions on how to run the code in each directory can be found in the README inside the directories.

===============================

CollapsedCompilation/ directory contains code for collapsed sampling, as taken from this paper:
"Approximate Knowledge Compilation by Online Collapsed Importance Sampling"

The original naive All-Marginals implementation that the above paper uses is from here:
http://reasoning.cs.ucla.edu/sdd/
which we downloaded and modified with our new linear-time All-Marginals subroutine.
The unmodified version is in directory WMC/ and the modified version is in directory WMC_fast/
The SDD package also contains code to extract missing intervals from a structured decomposable circuit (Lemma 1).

The code/datasets in this directory were used to run the experiments in Table 2bcd.

===============================

SimulateIntervals/ directory contains code that constructs smoothing gates given a set of intervals,
and compares the near-linear algorithm with the old quadratic algorithm. The block sizes for the 
near-linear algorithm are currently hard-coded, and could be integrated with the code for block size
calculation in InverseAckermannCalculation/.

The code in this directory was used to run the experiments in Table 2a.

===============================

InverseAckermannCalculation/ directory contains experimental code for calculating the inverse ackermann
of a number. There is some discussion of this on Page 8 of https://www.cs.princeton.edu/~chazelle/pubs/ComputPartialSumsMultiArrays.pdf

===============================

SemigroupRangesum/ directory contains an alternative implementation of the semigroup rangesum 
problem in query mode, which solves each query using a summation over <= 4 intermediate values. 
This implementation may be easier to follow compared to an inverse-ackermann implementation.



For questions, contact us at:
andyshih at cs dot stanford dot edu
