import sys, random, math, time

class RangeSum:
  def __init__(self, w, m):
    self.w = w
    self.A = [w,w,w]
    self.m = m
    self.d = [None for _ in self.A]
    self.cnt =  m+1
    self.gates = {}

    self.preprocess([x for x in xrange(self.m)], 0)
    self.toplevel = [[None for _ in xrange(self.A[-1])] for _ in xrange(self.A[-1])]
    self.preprocess_toplevel()
    print "Done Preprocessing"

  def preprocess(self, varz, level):
    if level == len(self.A):
      return
    
    if level == len(self.A)-1:
      self.toplevelvars = varz

    n = len(varz)
    a = self.A[level]
    b = n/a

    nextvarz = []
    g = [{} for _ in xrange(b)]

    for i in xrange(b):
      start, end = a*i, a*(i+1)
      
      g[i]["pre"] = {0: (self.cnt, [varz[start]])}
      self.gates[self.cnt] = [varz[start]]
      self.cnt += 1
      g[i]["post"] = {a-1: (self.cnt, [varz[end-1]])}
      self.gates[self.cnt] = [varz[end-1]]
      self.cnt += 1

      for j in xrange(start+1,end):
        cur = j-start
        g[i]["pre"][cur] = (self.cnt, g[i]["pre"][cur-1][1] + [varz[j]])
        self.gates[self.cnt] = g[i]["pre"][cur-1][1] + [varz[j]]
        self.cnt += 1
      nextvarz.append(self.cnt-1) # since the full postfix is the block sum
      for j in reversed(xrange(start,end-1)):
        cur = j-start
        g[i]["post"][cur] = (self.cnt, g[i]["post"][cur+1][1] + [varz[j]])
        self.gates[self.cnt] = g[i]["post"][cur+1][1] + [varz[j]]
        self.cnt += 1

    self.d[level] = g
    self.preprocess(nextvarz, level+1)

  def preprocess_toplevel(self):
    for i in xrange(self.A[-1]):
      for j in xrange(i,self.A[-1]):
        if j == i:
          self.toplevel[i][j] = (self.cnt, [self.toplevelvars[j]])
          self.cnt += 1
        else:
          self.toplevel[i][j] = (self.cnt, self.toplevel[i][j-1][1] + [self.toplevelvars[j]])
          self.gates[self.cnt] = self.toplevel[i][j-1][1] + [self.toplevelvars[j]]
          self.cnt += 1

  def printgate(self, g, recur):
    if g in self.gates:
      sys.stderr.write(str((g, self.gates[g]))  + "\n")
      if recur:
        for x in self.gates[g]:
          printgate(x)
    else:
      pass
      #print g


  def query(self, n, l, r):
    count_sgates = 0

    for i,a in enumerate(self.A):
      bigl = (l+a-1)/a
      bigr = r/a

      if bigr >= bigl:
        if bigl-1 >= 0 and l%a != 0:
          self.printgate(self.d[i][bigl-1]["post"][l % a][0], 0)
          count_sgates += 1
        if bigr < n/a and r%a != 0:
          self.printgate(self.d[i][bigr]["pre"][r % a][0], 0)
          count_sgates += 1
      else: # since intervals are large, must be toplevel
        if r > l:
          self.printgate(self.toplevel[l][r-1][0], 0)
          count_sgates += 1

      l,r = bigl,bigr
      n /= a

    return count_sgates

class Naive:
  def query(self, n, l, r):
    for i in range(l,r+1):
      sys.stderr.write(str(i) + "\n")
    return r+1-l




def test():
  w = 10
  m = w*w*w
  base = 4*m*round(math.log(m)/math.log(2))

  print "gates: %d, size: %d" % (m, base)

  num_trials = 4
  total_rs_elapsed, total_nv_elapsed = 0, 0
  for _ in xrange(num_trials):
    intervals = []
    while len(intervals) < m:
      l = random.randint(0,m)
      r = random.randint(0,m)

      if r-l < m/2:
        continue
      intervals.append((l,r))


    # range sum
    start = time.time()
    rs = RangeSum(w, m)
    for (i,(l,r)) in enumerate(intervals):
      q = rs.query(m,l,r)
    end = time.time()
    print "Done range sum"
    rs_elapsed = end - start


    # naive
    start = time.time()
    nv = Naive()
    for (i,(l,r)) in enumerate(intervals):
      q = nv.query(m,l,r)
    end = time.time()
    print "Done naive"
    nv_elapsed = end - start

    print "rs: %f, nv: %f" % (rs_elapsed, nv_elapsed)
    total_rs_elapsed += rs_elapsed
    total_nv_elapsed += nv_elapsed

  print "Average:"
  print "avg_rs: %f, avg_nv: %f" % (total_rs_elapsed / num_trials, total_nv_elapsed / num_trials)
  
if __name__ == "__main__":
  test()
