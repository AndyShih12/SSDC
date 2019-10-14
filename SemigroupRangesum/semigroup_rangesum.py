import sys
import math

class RangeSum:
  def __init__(self, N, K):
    self.N = N
    self.K = K
    self.n = ((N+K-1)/K)*K # round up to the nearest multiple of K
    self.gates = [[x, 0] for x in xrange(self.n)] # gate format: [id, m, ch0, ch1, ..., chm]
    self.curid = self.n
    self.num_blocks = self.n / self.K

    self.outer_id = self.num_blocks
    self.FORWARD = 0
    self.BACKWARD = 1
    self.yao_preprocess = [{} for i in xrange(self.num_blocks + 1)]

  def print_gates(self):
    for gate in self.gates:
      print "Gate ID: %u \t Num_children: %d \t Children: %s" % (gate[0], gate[1], str(gate[2:]))

  def debug(self, l, r, gates):
    #print l, r, gates

    elems_seen = []
    queue = gates
    while queue:
      gate_id = queue[0]
      queue = queue[1:]
      for ch_id in self.gates[gate_id][2:]:
        queue.append(ch_id)

      #print self.gates[gate_id]
      if gate_id < self.N:
        elems_seen.append(gate_id)

    assert(len(elems_seen) == r - l)
    elems_seen.sort()
    cur = l
    for e in elems_seen:
      assert(e == l)
      l = l + 1

    print "Output looks good to me!"


  def add_gate(self, num_children, children_array):
    gate = [self.curid, num_children] + children_array
    self.gates.append(gate)
    self.curid += 1
    return gate[0]

  def get_curid(self):
    return self.curid

  def process(self):
    blocks = []

    # process inner blocks
    for i in xrange(self.num_blocks):
      if not (i % 100): print "inner %u" %i
      lo, hi = i*self.K, (i+1)*self.K

      self.process_inner_block(lo, hi, i)
      gate_id = self.add_gate(self.K, [j for j in xrange(lo, hi)])
      blocks.append(gate_id)

    self.process_outer_block(blocks)

  def process_inner_block(self, lo, hi, block_id):
    self.yao_nlogn_preprocess([x for x in xrange(lo, hi)], block_id)

  def process_outer_block(self, id_arr):
    self.yao_nlogn_preprocess(id_arr, self.outer_id)

  def yao_nlogn_preprocess(self, elems, dict_id):
    m_len = len(elems)
    m = self.next_power_of_2(m_len)
 
    self.yao_preprocess[dict_id] = [{}, {}]
    forward_dp = self.yao_preprocess[dict_id][self.FORWARD]
    backward_dp = self.yao_preprocess[dict_id][self.BACKWARD]


    # forward
    s = 1
    while s <= m: # use equality for retrieving edges in outer_block case
      offset = 1
      forward_dp[s] = [None for x in xrange(m_len + offset)]
      for i in xrange(0,m_len):
        if (i % s) == 0:
          gateid = self.add_gate(1, [elems[i]])
        else:
          gateid = self.add_gate(2, [elems[i], forward_dp[s][i-1 + offset]])
        forward_dp[s][i + offset] = gateid
      s = s * 2

    # backward
    s = 1
    while s <= m: # use equality for retrieving edges in outer_block case
      backward_dp[s] = [None for x in xrange(m_len)]
      for i in reversed(xrange(0,m_len)):
        if ((i+1) % s) == 0 or (i+1) == m_len:
          gateid = self.add_gate(1, [elems[i]])
        else:
          gateid = self.add_gate(2, [elems[i], backward_dp[s][i+1]])
        backward_dp[s][i] = gateid
      s = s * 2

    #print "yao preprocessed"
    #print elems, dict_id
    #print self.yao_preprocess

  def next_power_of_2(self, v):
    # gets the smallest power of 2 that is >= than integer v
    # e.g. 7 -> 8, 16 -> 16, 17 -> 32
    assert(v > 0)
    return 1<<(v-1).bit_length()


  def retrieve(self, l, r):
    # retrieves range [l,r)
    # assumes r > l

    K = self.K
    output = []

    # if same block, use inner preprocessing
    if (r / K) == (l / K):
      block_id = l / K
      output = self.yao_retrieve(l % K, r % K, block_id)

    # else use outer block preprocessing
    else:
      left_outer_block, right_outer_block = (l+K-1)/K, r/K
      if left_outer_block != right_outer_block:
        output = self.yao_retrieve(left_outer_block, right_outer_block, self.outer_id)

      # process edges
      m = self.next_power_of_2(K)
      if (l % K):
        block_id = l / K
        left_edge = self.yao_preprocess[block_id][self.BACKWARD][m][l % K]
        output.append(left_edge)
      if (r % K):
        block_id = r / K
        right_edge = self.yao_preprocess[block_id][self.FORWARD][m][r % K]
        output.append(right_edge)

    return output

  def yao_retrieve(self, l, r, block_id):
    d = r - l 
    p = self.next_power_of_2(d)

    if ((r-1)/p) == (l/p):
      p = p / 2

    if p == 0:
      return [self.yao_preprocess[block_id][self.BACKWARD][1][l]]

    return [self.yao_preprocess[block_id][self.BACKWARD][p][l], self.yao_preprocess[block_id][self.FORWARD][p][r]]

def main():
  N = int(raw_input("Enter the number of variables N: "))

  K = 64 # Hardcoded for log(2^64). Did not test cases where K is not a power of 2.

  job = RangeSum(N, K)
  job.process()

  print "Preprocessed %u intermediate gates." % len(job.gates)

  should_print_gates = str(raw_input("Print intermediate gates? (y/n): ")).lower().strip()
  if should_print_gates[0] == 'y':
    job.print_gates()

  while True:
    print "Provide an interval [l,r). Must have 0 <= l < %u and l < r <= %u. Enter l and r as two space separated integers (e.g. '1 2')." % (N,N)
    l, r = map(int, raw_input().split())

    if r <= l:
      print "Must have r > l"
      continue

    gates = job.retrieve(l,r)

    print "We can express the sum of the interval [%u,%u) as the sum of the following gates: %s" % (l, r, ' + '.join(map(str,gates)) )
    job.debug(l, r, gates)

if __name__ == "__main__":
  main()

