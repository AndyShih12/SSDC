To run, do
python simulate_intervals.py

It randomly picks intervals and outputs the smoothing gates for those intervals.
To suppress the output and just get the time taken by the algorithm, do
python simulate_intervals.py 2> /dev/null

or do
./run.sh

Change the value of 'w' in the test() function to try out different interval sizes.
Good values to try are w=10,20,30,40,50
