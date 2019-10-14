#!/usr/bin/env python

import math
import operator
from collections import defaultdict
import random
import learn

"""
CLASSES: INDEXER
"""

class Index:
    def __init__(self,domain,var_sizes,index_type="il1"):
        self.length = len(domain)
        self.domain = domain
        self.sizes = [ var_sizes[var] for var in domain ]
        self.type = index_type
        self.offsets = [1] * self.length
        if index_type == "il1":
            i = self.length - 2
            while i >= 0:
                self.offsets[i] = self.offsets[i+1] * self.sizes[i+1]
                i -= 1
        elif index_type == "il2":
            i = 1
            while i < self.length:
                self.offsets[i] = self.offsets[i-1] * self.sizes[i-1]
                i += 1

    def incr(self,inst):
        if self.type == "il1":
            i = self.length - 1
            while i >= 0:
                inst[i] += 1
                if inst[i] == self.sizes[i]:
                    inst[i] = 0
                else:
                    return False
                i -= 1
        elif self.type == "il2":
            i = 0
            while i < self.length:
                inst[i] += 1
                if inst[i] == self.sizes[i]:
                    inst[i] = 0
                else:
                    return False
                i += 1
        return True

    def __iter__(self):
        inst = [0] * self.length
        done = False
        while not done:
            yield list(inst)
            done = self.incr(inst)

    def index(self,inst):
        index = 0
        for val,offset in zip(inst,self.offsets):
            index += val*offset
        return index

    def indices(self,index2):
        indices1 = list(enumerate(self.domain))
        indices2 = list(enumerate(index2.domain))
        indices1.sort(key=operator.itemgetter(1))
        indices2.sort(key=operator.itemgetter(1))
        index_map = [ (i1,i2) for (i1,var1),(i2,var2)
                      in zip(indices1,indices2) ]
        index_map.sort(key=operator.itemgetter(0))
        for inst2 in index2:
            inst1 = [ inst2[i2] for (i1,i2) in index_map ]
            yield self.index(inst1)

    def inst_to_dict(self,inst):
        return dict(zip(self.domain,inst))

    def dict_to_inst(self,d):
        return [ d[var] for var in self.domain ]

"""
CLASSES: NETWORK
"""

"""This network class does not convert values to float, it maintains
them as strings as they are read from a .uai file.
"""
class Network:
    def __init__(self,network_type,var_sizes,pot_domains,pots):
        self.type = network_type
        self.size = len(var_sizes)
        self.var_sizes = var_sizes
        self.num_pots = len(pots)
        self.pot_domains = pot_domains
        self.parents = [ domain[:-1] for domain in pot_domains ]
        self.pots = pots

    def uai(self):
        st = "%s\n" % self.type
        st += "%d\n" % self.size
        st += " ".join(str(size) for size in self.var_sizes) + "\n"
        st += "%d\n" % self.num_pots
        for domain in self.pot_domains:
            st += "%d " % len(domain)
            st += " ".join(str(var) for var in domain)
            st += "\n"
        st += "\n"
        for pot in self.pots:
            st += "%d\n  " % len(pot)
            st += " ".join(val for val in pot)
            st += "\n"
        return st

    def hugin_net(self):
        st = "net\n{\n}\n\n"
        var_fmt = "x%d"
        for var,size in enumerate(self.var_sizes):
            name = var_fmt % var
            states = " ".join( '"s%d"' % i for i in xrange(size))
            st += "node %s\n{\n    states = (%s);\n}\n" % (name,states)
        st += "\n"
        for var,(domain,pot) in enumerate(zip(self.pot_domains,self.pots)):
            name = var_fmt % var
            parents = [ var_fmt % parent for parent in domain[:-1] ]
            parents = " ".join(parents)
            values = " ".join(pot)
            st += "potential ( %s | %s )\n{\n    data = (%s);\n}\n" % \
                  (name,parents,values)
        return st

    def fastinf(self):
        st = '@Variables\n'
        for x,size in enumerate(self.var_sizes):
            st += 'x%d\t%d\n' % (x,size)
        st += '@End\n\n\n'

        st += '@Cliques\n'
        for x,domain in enumerate(self.pot_domains):
            st += 'cliq%d\t%d\t' % (x,len(domain))
            st += " ".join(str(var) for var in domain)
            st += " \t"
            count,neighbors = 0,list()
            for y,domain_y in enumerate(self.pot_domains):
                if x == y: continue
                if len( set(domain) & set(domain_y) ) > 0:
                    count += 1
                    neighbors.append(y)
            st += "%d\t" % count
            st += " ".join(str(y) for y in neighbors)
            st += " \n"
        st += '@End\n\n\n'

        st += '@Measures\n'
        for x,(pot,domain) in enumerate(zip(self.pots,self.pot_domains)):
            st += 'mes%d\t%d\t' % (x,len(domain))
            st += " ".join(str(self.var_sizes[var]) for var in domain)
            st += ' \t'
            st += " ".join(val for val in pot)
            st += ' \n'
        st += '@End\n\n\n'

        st += '@CliqueToMeasure\n'
        for x in xrange(len(self.pots)):
            st += '%d\t%d\n' % (x,x)
        st += '@End\n\n\n'

        return st

    def mplp_format(self):
        var_sizes = "\n".join(str(size) for size in self.var_sizes) + "\n"

        regions = ""
        # all factor domains
        for domain in self.pot_domains:
            regions += " ".join(str(var+1) for var in domain)
            regions += "\n"
        # all node domains
        for i in xrange(len(self.var_sizes)):
            regions += "%d\n" % (i+1)

        num_pots = len(self.pots)
        region_intersects = ""
        for i,domain in enumerate(self.pot_domains):
            region_intersects += "%d " % (i+1)
            region_intersects += " ".join(str(j+1+num_pots) for j in domain)
            region_intersects += "\n"
        for i in xrange(len(self.var_sizes)):
            region_intersects += "%d\n" % (i+1+num_pots)

        lambdas = ""
        for pot,domain in zip(self.pots,self.domains):
            pot = [ "%.8f" % math.log(float(p)) for p in pot ]
            lambdas += " ".join(pot) + "\n"
        for size in self.var_sizes:
            pot = [ "0" for p in xrange(size) ]
            lambdas += " ".join(pot) + "\n"

        intersects = regions
        return var_sizes,regions,region_intersects,lambdas,intersects

    def wcnf(self,evid={}):
        num_clauses = 0
        for pot in self.pots:
            num_clauses += len(pot)
        num_clauses += len(evid)

        total = 0.0
        for pot in self.pots:
            for p in pot:
                p = float(p)
                if p == 0.0: continue
                total -= math.log(p)
        maxw = 10**int( math.ceil(math.log10(total)) )

        sign = lambda x: -1 if x else 1
        st = "c converted by pybn\n"
        st += "p wcnf %d %d %d\n" % (self.size,num_clauses,maxw)
        for pot,domain in zip(self.pots,self.pot_domains):
            var_sizes = [ self.var_sizes[var] for var in domain ]
            index = Index(range(len(var_sizes)),var_sizes)
            for i,state in enumerate(index): # AC: zip broken?
                clause = [ str(sign(val)*(var+1)) for 
                           var,val in zip(domain,state) ]
                clause = " ".join(clause)
                p = float(pot[i])
                if   p == 0.0:
                    weight = maxw
                elif p == 1.0:
                    weight = 0
                else:
                    weight = -math.log(p)
                if type(weight) is int:
                    st += "%d %s 0\n" % (weight, clause)
                else:
                    st += "%.8e %s 0\n" % (weight, clause)

        varz = evid.keys()
        varz.sort()
        for var in varz:
            st += "%d %d 0\n" % (maxw,sign(evid[var])*(int(var)+1))

        return st

    """wcnf conversion when the probabilities are in log
    """
    def wcnf_alt(self,evid={}):
        num_clauses = 0
        for pot in self.pots:
            num_clauses += len(pot)
        num_clauses += len(evid)

        total = 0.0
        for pot in self.pots:
            for p in pot:
                if p.startswith('-'): p = p[1:]
                total += float(p)
        maxw = 10**int( math.ceil(math.log10(total)) )

        sign = lambda x: -1 if x else 1
        st = "c converted by pybn\n"
        st += "p wcnf %d %d %d\n" % (self.size,num_clauses,maxw)
        for pot,domain in zip(self.pots,self.pot_domains):
            var_sizes = [ self.var_sizes[var] for var in domain ]
            index = Index(range(len(var_sizes)),var_sizes)
            for i,state in enumerate(index): # AC: zip broken?
                clause = [ str(sign(val)*(var+1)) for 
                           var,val in zip(domain,state) ]
                clause = " ".join(clause)
                p = pot[i]
                if p.startswith('-'): p = p[1:]
                st += "%s %s 0\n" % (p,clause)

        varz = evid.keys()
        varz.sort()
        for var in varz:
            st += "%d %d 0\n" % (maxw,sign(evid[var])*(int(var)+1))

        return st

    """wcnf conversion when the probabilities are in log
    """
    def wcnf_alt_iter(self,evid={}):
        num_clauses = 0
        for pot in self.pots:
            num_clauses += len(pot)
        num_clauses += len(evid)

        total = 0.0
        for pot in self.pots:
            for p in pot:
                if p.startswith('-'): p = p[1:]
                total += float(p)
        maxw = 10**int( math.ceil(math.log10(total)) )

        sign = lambda x: -1 if x else 1
        yield "c converted by pybn\n"
        yield "p wcnf %d %d %d\n" % (self.size,num_clauses,maxw)
        for pot,domain in zip(self.pots,self.pot_domains):
            var_sizes = [ self.var_sizes[var] for var in domain ]
            index = Index(range(len(var_sizes)),var_sizes)
            for i,state in enumerate(index): # AC: zip broken?
                clause = [ str(sign(val)*(var+1)) for 
                           var,val in zip(domain,state) ]
                clause = " ".join(clause)
                p = pot[i]
                if p.startswith('-'): p = p[1:]
                yield "%s %s 0\n" % (p,clause)

        varz = evid.keys()
        varz.sort()
        for var in varz:
            yield "%d %d 0\n" % (maxw,sign(evid[var])*(int(var)+1))

    def base_cnf(self):
        zero_count = 0
        for pot in self.pots:
            for p in pot:
                if float(p) == 0.0: zero_count += 1
        st = [ "p cnf %d %d\n" % (self.size,zero_count) ]

        # invert sign
        sign = lambda x: 1 if x == 0 else -1
        for pot,domain in zip(self.pots,self.pot_domains):
            index = Index(domain,self.var_sizes)
            for inst,p in zip(index,pot):
                if float(p) != 0.0: continue
                clause = [ sign(val)*(var+1) for var,val in zip(domain,inst) ]
                clause = [ str(lit) for lit in clause ]
                st.append( " ".join(clause) )
                st.append( " 0\n" )

        return "".join(st)

    def __repr__(self):
        return self.uai()

    """
    LEARNING AND SIMULATION
    """

    @staticmethod
    def _sample(pr,p=None):
        if p is None: p = random.random()

        cum = 0.0
        for val,prob in enumerate(pr):
            cum += prob
            if p < cum: break

        return val

    """ given a variable and an instantiation of its parents, return 
        the corresponding CPT column """
    def cpt_column(self,var,parent_inst):
        var_size = self.var_sizes[var]
        domain = self.pot_domains[var]
        pot = self.pots[var]
        parents = self.parents[var]

        index = Index(parents,self.var_sizes)
        parent_index = index.index(parent_inst)
        start = parent_index * var_size
        end = start + var_size
        column = pot[start:end]
        column = [ float(p) for p in column ]
        return column

    def simulate(self):
        world = [0]*self.size
        for var in xrange(self.size):
            domain = self.pot_domains[var]
            parents = self.parents[var]
            u = project_onto(world,parents)
            pr = self.cpt_column(var,u)
            val = Network._sample(pr)
            world[var] = val
        return world

    def simulate_dataset(self,N,seed=None):
        if seed is not None:
            random.seed(seed)
        worlds = defaultdict(lambda: 0)
        for i in xrange(N):
            world = tuple(self.simulate())
            worlds[world] += 1
        instances = worlds.keys()
        counts = worlds.values()
        return learn.DataSet(instances,counts=counts)

    """
    STATS
    """

    def parameter_count(self):
        count = 0
        for size,pot in zip(self.var_sizes,self.pots):
            count += (len(pot)/size)*(size-1)
        return count

    def zero_parameter_count(self):
        count = 0
        for size,pot in zip(self.var_sizes,self.pots):
            pot = [ float(p) for p in pot ]
            count += pot.count(0.0)
        return count

def project_onto(world,domain):
    sub_world = [0]*len(domain)
    for i,var in enumerate(domain):
        sub_world[i] = world[var]
    return sub_world

"""
FUNCTIONS: SUPPORT
"""

"""
        self.type = network_type
        self.size = len(var_sizes)
        self.var_sizes = var_sizes
        self.num_pots = len(pots)
        self.pot_domains = pot_domains
        self.pots = pots
"""

# for a bn not in topological order, get topological order
def topological_order(bn):
    # map from variable to pot indices
    var2pot = [ set() for x in xrange(bn.size) ]
    for i,pot_vars in enumerate(bn.pot_domains):
        for var in pot_vars:
            var2pot[var].add(i)

    # find leaves
    queue = []
    for var in xrange(bn.size):
        if len(var2pot[var]) == 1: queue.append(var)

    # bottom-up, find leaves
    order = []
    while queue:
        var = queue.pop(0)
        order.append(var)
        i = iter(var2pot[var]).next()
        for pot_var in bn.pot_domains[i]:
            var2pot[pot_var].remove(i)
            if len(var2pot[pot_var]) == 1: queue.append(pot_var)
    order.reverse()

    # check
    if len(order) != bn.size:
        raise Exception("error getting topological order")
    return order

""" this is a stricter check on the BN ordering """
def is_topologically_ordered(bn):
    var2pot = [ set() for x in xrange(bn.size) ]
    for i,pot_vars in enumerate(bn.pot_domains):
        for var in pot_vars:
            var2pot[var].add(i)
    for var in xrange(bn.size-1,0,-1):
        if len(var2pot[var]) != 1: return False
        i = var2pot[var].pop()
        if i != var: return False
        if bn.pot_domains[i][-1] != var: return False
        for pot_var in bn.pot_domains[i][:-1]:
            var2pot[pot_var].remove(i)
    return True

def has_valid_cpts(bn,eps=1e-4):
    sizes = bn.var_sizes
    for var,(pot_domain,pot) in enumerate(zip(bn.pot_domains,bn.pots)):
        for column in tuples(pot,sizes[var]):
            vals = [ float(p) for p in column ]
            err = abs(1.0 - sum(vals))
            if err > eps:
                return " ".join(column)
    return True

def is_bayesian_network(bn,debug=False):
    err_msg = ""

    if bn.type != "BAYES":
        err_msg = "type is declared to be %s" % str(bn.type)
    elif bn.size != bn.num_pots:
        err_msg = "%d variables and %d pots" % (bn.size,bn.num_pots)
    elif not is_topologically_ordered(bn):
        err_msg = "bn does not respect a topological order"
    else:
        err = has_valid_cpts(bn)
        if err is not True:
            err_msg = "some CPT column did not sum to one (col: %s)" % err

    is_bn = err_msg is ""
    if not is_bn and debug: print err_msg
    return err_msg if debug else is_bn

def is_binary_network(bn):
    for size in bn.var_sizes:
        if size != 2:
            return False
    return True

def reorder_network(bn,order=None):
    if order is None: order = topological_order(bn)
    old2new = [0]*len(order)
    for i,var in enumerate(order):
        old2new[var] = i

    network_type = bn.type
    var_sizes = [ bn.var_sizes[var] for var in order ]
    pot_domains = [None]*len(order)
    pots = [None]*len(order)
    for old_domain,old_pot in zip(bn.pot_domains,bn.pots):
        new_domain = [ old2new[var] for var in old_domain ]
        new_domain.sort()
        tmp_domain = [ order[var] for var in new_domain ]
        old_index = Index(old_domain,bn.var_sizes)
        new_index = Index(tmp_domain,bn.var_sizes)
        new_pot = [ old_pot[i] for i in old_index.indices(new_index) ]
        var = new_domain[-1]
        pot_domains[var] = new_domain
        pots[var] = new_pot
    return Network(network_type,var_sizes,pot_domains,pots)

"""
FUNCTIONS
"""

def file_tokenizer(filename):
    f = open(filename,'r')
    for line in f:
        line = line.strip()
        for token in line.split():
            yield token
    f.close()

def parse_uai(filename):
    t = file_tokenizer(filename)
    network_type = t.next()

    size = int(t.next())
    var_sizes = [ int(t.next()) for var in xrange(size) ]
    num_pots = int(t.next())
    pot_domains = []
    for i in xrange(num_pots):
        pot_size = int(t.next())
        domain = [ int(t.next()) for var in xrange(pot_size) ]
        pot_domains.append(domain)

    pots = []
    for i in xrange(num_pots):
        pot_length = int(t.next())
        pot = [ t.next() for val in xrange(pot_length) ]
        pots.append(pot)
    return Network(network_type,var_sizes,pot_domains,pots)

""" for sanity checking input Bayesian networks """
def open_uai(filename):
    bn = parse_uai(filename)
    err_msg = is_bayesian_network(bn,debug=True)
    if err_msg != "":
        raise Exception(err_msg)
    return bn

def parse_order(filename):
    f = open(filename,'r')
    tokens = f.read().split()
    f.close()
    tokens = [ int(x) for x in tokens ]
    if tokens[0] != len(tokens)-1:
        raise Exception('invalid order')
    return tokens[1:]

"""
FUNCTIONS
"""

""" a generator for tuples of size k
    will drop elements at end if there are less than k """
def tuples(my_list,k):
    if my_list is None: return
    it = iter(my_list)
    for x in it:
        cur = [x] + [ it.next() for x in xrange(k-1) ]
        yield tuple(cur)
