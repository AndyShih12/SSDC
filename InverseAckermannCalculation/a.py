import sys

DELTA_T = 2
DELTA_K = 3



def R(n, t, k, calls=0):
  #print "recur: ", n, t, k, calls

  calls += 1

  # if calls >= n, then for sure we can handle n, so just return n
  if calls >= n:
    return n, calls

  if t < DELTA_T:
    return min(1,n), calls

  if k < DELTA_K:
    return min(4,n), calls

  a, calls = R(n, t, k - DELTA_K, calls)
  b, calls = R(n, t - DELTA_T, a, calls)

  ans = min(a*b, n)
  return ans, calls

def get_inv_ack_params(n):

  t = DELTA_T
  k = DELTA_K

  ans = R(n, t, k)

  while ans[0] < n:
    k += DELTA_K
    if k > t:
      t += DELTA_T
      k = DELTA_K
    ans = R(n, t, k)

  return t, k

def main():
  n = int(raw_input())
  sys.setrecursionlimit(10*n)

  t,k = get_inv_ack_params(n)
  print "n, t, k: ", n, t, k

  qu = [(n, t, k)] # queue
  while qu:
    n, t, k = qu[0]
    qu = qu[1:]
    
    a, calls = R(n, t, k - DELTA_K)
    b = (n + a - 1) / a

    print n, a, b, t, k
    
    if k >= DELTA_K and t >= DELTA_T:
      if b > 1:
        qu.append((a, t, k - DELTA_K))
        qu.append((b, t - DELTA_T, b))
      else:
        # get lowest kk that satisfies a
        ans = 0
        kk = min(0, k - 2*DELTA_K)
        while ans < a:
          kk += DELTA_K
          ans = R(a, t, kk)
        
        qu.append((a, t, kk)) 

if __name__ == "__main__":
  main()
