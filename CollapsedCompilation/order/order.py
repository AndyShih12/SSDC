#!/usr/bin/env jython

import os
import sys
import pybn
import pybn.net_io
import pybn.engines
import java.lang.System.nanoTime as nanoTime

def mkdir_p(path):
    import os, errno
    try:
        os.makedirs(path)
    except OSError, err: # Python >2.5 #AC
        if err.errno == errno.EEXIST:
            pass
        else:
            raise

if len(sys.argv) < 2:
    print "Usage: %s NET" % sys.argv[0]
    exit(1)

filename = sys.argv[1]
if len(sys.argv) >= 3:
    out_path = None
    full_path = sys.argv[2]
else:
    split = os.path.split(filename)
    out_name = os.path.splitext(split[-1])[0] + ".order"
    out_path = os.path.join('orders',split[0])
    full_path = os.path.join(out_path,out_name)

t1 = nanoTime()
print "opening: %s" % filename,
bn = pybn.net_io.open_network(filename)
t2 = nanoTime()
print "(%.4fs)" % ((t2-t1)*1e-9)

print "minfill ...",
t1 = nanoTime()
mf_order,mf_size = pybn.engines.minfill(bn,10)
t2 = nanoTime()
print "(%.4fs once)" % ((t2-t1)*1e-9)

#mf_order,mf_size = pybn.engines.minfill(bn,100)
#mf_order,mf_size = pybn.engines.minfill(bn,100)
#mf_order,mf_size = None,1000
#print "minsize ..."
#ms_order,ms_size = pybn.engines.minsize(bn)

#order,size = (mf_order,mf_size) if mf_size <= ms_size else (ms_order,ms_size)
order,size = mf_order,mf_size
jt_stats = pybn.engines.get_jointree_stats(bn,order)

order_vars = set(order)
for x in xrange(bn.domain().size()):
    if not x in order_vars:
        order.append(x)

#print "size: %.3f" % size
print "jt stats: %.3f %.3f %.3f %.3f" % tuple(jt_stats)

if full_path:
    print "saving", full_path
    if out_path: mkdir_p(out_path)
    f = open(full_path,'w')
    order = [ str(var) for var in order ]
    f.write("%d %s\n" % (len(order), " ".join(order)))
    f.close()
