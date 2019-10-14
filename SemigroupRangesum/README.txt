Solves the semigroup rangesum problem with a preprocessing cost of O(n log log n) summations, and query cost of 4 summations.
Combines ideas from "Computing Partial Sums in Multidimensional Arrays" (Chazelle & Rosenberg 1989) and Lemma 6 from "Space-time tradeoff for answering range queries" (Yao 1982).

Lemma 6 shows a way to do O(n log n) preprocessing so that each interval can be expressed as a sum over 2 intermediate values.
The idea here is then to split n into n/log n inner blocks, with each inner block having size log n.

Preprocessing one inner blocks takes O((log n)*(log log n)). There are n/log n inner blocks, so preprocessing all inner blocks takes O(n*(log log n)).

Preprocessing the outer block takes O((n/log n) * (log(n) - (log log n))) < n

Any interval range-sum query can then be expressed as a summation over <= 4 intermediate values.




Currently hardcoded for NUMBER_OF_VARIABLES < 2^64. For any interval range-sum query, we can express the answer as a summation over <= 4 intermediate values.

To run, do:
python semigroup_rangesum.py 
