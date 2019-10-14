#!/usr/bin/env jython

## AC: there is something wrong with convert_to_il2

import java.lang.System.nanoTime as nanoTime

import os
import math
import operator

import edu.ucla.belief.BeliefNetworkImpl as BeliefNetwork
import edu.ucla.belief.FiniteVariableImpl as FV
import edu.ucla.belief.Table as Table_il1
import edu.ucla.belief.TableShell as TableShell_il1
import edu.ucla.belief.io.NetworkIO as NetworkIO
import il2.model.BayesianNetwork as BN
import il2.model.Domain as Domain
import il2.model.Table as Table
import il2.bridge.Converter as Converter
import il2.util.IntSet as IntSet
import il2.util.IntList as IntList
import java.io.File as File

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
            yield inst
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
        self.pots = pots

    def il1(self):
        network_vars = []
        var_fmt = "x%%0%dd" % (len(str(self.size))-1)
        for var,size in enumerate(self.var_sizes):
            name = var_fmt % var
            instances = [ str(i) for i in xrange(size) ]
            var = FV(name,instances)
            network_vars.append(var)
        pots = []
        for domain,pot in zip(self.pot_domains,self.pots):
            pot_vars = [ network_vars[var] for var in domain ]
            vals = [ float(val) for val in pot ]
            pot = TableShell_il1(Table_il1(pot_vars,vals))
            pots.append(pot)
        var2pot = dict( (var,pot) for var,pot in zip(network_vars,pots) )
        return BeliefNetwork(var2pot)

    def il2(self):
        d = Domain(self.size)
        var_fmt = "x%%0%dd" % (len(str(self.size))-1)
        for var,size in enumerate(self.var_sizes):
            name = var_fmt % var
            d.addDim(name,size)
        pots = []
        for domain,pot in zip(self.pot_domains,self.pots):
            pot_vars = IntSet(domain)
            vals = [ float(val) for val in pot ]
            vals = self.to_il2_values(domain,vals)
            pot = Table(d,pot_vars,vals)
            pots.append(pot)
        return BN(pots)

    """il2 uses a non-standard ordering of potentials/tables.  So we
    have to correct for this in Network.
    """
    def to_il2_values(self,domain,pot):
        domain1,domain2 = domain,sorted(domain)
        index1 = Index(domain1,self.var_sizes,index_type="il1")
        index2 = Index(domain2,self.var_sizes,index_type="il2")
        return [ pot[i] for i in index2.indices(index1) ]

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
        var_fmt = "x%%0%dd" % (len(str(self.size))-1)
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


    def __repr__(self):
        return self.uai()

def var2pot(size,domains):
    v2p = [ [] for i in xrange(size) ]
    for i,domain in enumerate(domains):
        for var in domain:
            v2p[var].append(i)
    return v2p

def values_to_string(vals):
    return [ str(val) for val in vals ]

def python_network(bn):
    network_type = "BAYES"
    if type(bn) is BeliefNetwork:
        order = topological_order(bn)
        cpts = [ var.getCPTShell().getCPT() for var in order ]
        v2i = dict( (var,i) for i,var in enumerate(order) )

        var_sizes = [ var.size() for var in order ]
        pot_domains = [ [ v2i[var] for var in cpt.variables() ] 
                        for cpt in cpts ]
        pots = [ values_to_string(cpt.dataclone()) for cpt in cpts ]
    elif type(bn) is BN:
        d = bn.domain()
        var_sizes = [ d.size(var) for var in xrange(d.size()) ]
        pot_domains = [ cpt.vars().toArray() for cpt in bn.cpts() ]
        pots = [ values_to_string(to_il1_values(cpt)) for cpt in bn.cpts() ]
    return Network(network_type,var_sizes,pot_domains,pots)

"""Returns a Bayesian network object.  If bntype='il1' then the
function returns an edu.ucla.belief.BeliefNetwork object.  If bntype
is anything else, it returns an il2.model.BayesianNetwork object"""
def open_network(filename,bntype='il2'):
    ext = os.path.splitext(filename)[-1]
    if   ext == '.uai':
        bn = parse_uai(filename)
        if bntype == 'il1':
            return bn.il1()
        else:
            return bn.il2()
    elif ext == '.net' or ext == '.dne': # AC: need to make canonical
        bn = NetworkIO.read(filename)
        if bntype == 'il1':
            return bn
        else:
            bn = convert_to_il2(bn)
            return bn
    elif ext == '':
        raise Exception('did not find a file extension')
    else:
        raise Exception('invalid file extension: %s' % ext)

def open_evidence(bn,filename,bntype='il2'):
    ext = os.path.splitext(filename)[-1]
    if   ext == '.evid':
        inst = Uai.evidToInst(filename)
        return Uai.convert(inst,bn)
    elif ext == '.inst':
        inst = Uai.openInst(filename)
        return Uai.convert(inst,bn)
    elif ext == '':
        raise Exception('did not find a file extension')
    else:
        raise Exception('invalid file extension: %s' % ext)

def open_order(filename):
    f = open(filename,'r')
    tokens = f.read().split()
    f.close()
    tokens = [ int(x) for x in tokens ]
    if tokens[0] != len(tokens)-1:
        raise Exception('invalid order')
    return IntList(tokens[1:])

def save_network(bn,filename):
    ext = os.path.splitext(filename)[-1]
    if   ext == '.uai':
        # TODO
        pass
    elif ext == '.net': # AC: need to make canonical
        st = network_as_hugin_net(bn)
    elif ext == '':
        raise Exception('did not find a file extension')
    else:
        raise Exception('invalid file extension: %s' % ext)
    f = open(filename,'w')
    f.write(st)
    f.close()

    # NetworkIO.writeNetwork(bn,File(filename))

def network_as_hugin_net(bn):
    st = "net\n{\n}\n\n"

    if type(bn) is BeliefNetwork:
        # TODO
        pass
    elif type(bn) is BN:
        d = bn.domain()
        for var in xrange(d.size()):
            name = d.name(var)
            size = d.size(var)
            states = d.instanceNames(var)
            states = " ".join('"%s"' % state for state in states)
            st += "node %s\n{\n    states = (%s);\n}\n" % (name,states)
        st += "\n"

        for var,pot in enumerate(bn.cpts()):
            name = d.name(var)
            parents = pot.vars().toArray()[:-1]
            parents = [ d.name(parent) for parent in parents ]
            parents = " ".join(parents)
            pot = values_to_string(to_il1_values(pot))
            values = " ".join(pot)
            st += "potential ( %s | %s )\n{\n    data = (%s);\n}\n" % \
                  (name,parents,values)

    return st

def save_data(bn,data,filename):
    ext = os.path.splitext(filename)[-1]
    if ext == '.csv':
        save_data_as_csv(bn,data,filename)
    elif ext == '.evid':
        save_data_as_uai(bn,data,filename)

def save_data_as_csv(bn,data,filename):
    d = bn.domain()
    f = open(filename,'w')
    header = ",".join( d.name(var) for var in xrange(d.size()) )
    f.write(header)
    f.write("\n")
    for evid in data:
        vals = [ d.instanceName(var,evid.get(var)) 
                 if evid.keys().contains(var) else ""
                 for var in xrange(d.size()) ]
        line = ",".join(vals)
        f.write(line)
        f.write("\n")
    f.close()

def save_data_as_uai(bn,data,filename):
    f = open(filename,'w')
    f.write("%d\n" % len(data))
    for evid in data:
        f.write("%d" % evid.size())
        for var in evid.keys().toArray():
            f.write(" %d %d" % (var,evid.get(var)))
        f.write("\n")
    f.close()

# def file_tokenizer(filename):
#     f = open(filename,'r')
#     tokens = f.read().split()
#     f.close()
#     return iter(tokens)

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

"""il2 uses a non-standard ordering of potentials/tables.  So we
have to correct for this.
"""
def to_il1_values(pot):
    d = pot.domain()
    domain = range(pot.vars().size())
    var_sizes = [ d.size(var) for var in pot.vars().toArray() ]
    pot = pot.values()
    index1 = Index(domain,var_sizes,index_type="il1")
    index2 = Index(domain,var_sizes,index_type="il2")
    return [ pot[index2.index(inst)] for inst in index1 ]

"""il2 uses a non-standard ordering of potentials/tables.  So we
have to correct for this.
"""
def to_il2_values(domain,pot):
    domain1,domain2 = domain,sorted(domain)
    var_sizes = [0] * (max(domain)+1)
    for var,i in zip(pot.variables(),domain):
        var_sizes[i] = var.size()
    index1 = Index(domain1,var_sizes,index_type="il1")
    index2 = Index(domain2,var_sizes,index_type="il2")
    pot = pot.dataclone()
    return [ pot[i] for i in index2.indices(index1) ]

"""
def convert_old(bn,order):
    c = Converter()
    bn = c.convert(bn)
    order = [ c.convert(var) for var in order ]
    return bn,order
"""

def convert(bn):
    if type(bn) is BeliefNetwork:
        return convert_to_il2(bn)
    elif type(bn) is BN:
        return convert_to_il1(bn)
    else:
        raise Exception('unable to convert network type ' + type(bn))

def convert_to_il1(bn):
    d = bn.domain()
    network_vars = []
    for var in xrange(d.size()):
        name = d.name(var)
        instances = d.instanceNames(var)
        var = FV(name,instances)
        network_vars.append(var)
    pots = []
    for pot in bn.cpts():
        pot_vars = [ network_vars[var] for var in pot.vars().toArray() ]
        vals = to_il1_values(pot)
        pot = TableShell_il1(Table_il1(pot_vars,vals))
        pots.append(pot)
    var2pot = dict( (var,pot) for var,pot in zip(network_vars,pots) )
    return BeliefNetwork(var2pot)

"""This version converts il1 to il2 network directly.
"""
def convert_to_il2(bn,order=None):
    if order is None:
        order = topological_order(bn)
    d = Domain(len(order))
    pots = []
    for var in order:
        d.addDim(var.getID(),var.instances())
        pot = var.getCPTShell().getCPT()
        pot_vars = [ d.index(var.getID()) for var in pot.variables() ]
        pot = to_il2_values(pot_vars,pot)
        pot_vars = IntSet(pot_vars)
        pot = Table(d,pot_vars,pot)
        pots.append(pot)
    return BN(pots)

"""Returns a canonical topological ordering of a network's
variables."""
def topological_order(bn):
    count = dict()
    order = []
    cur,next = [],[]

    for node in bn.vertices():
        name = node.getID()
        degree = bn.inDegree(node)
        if degree == 0:
            cur.append(node)
        else:
            count[name] = degree

    while cur:
        cur.sort()
        for node in cur:
            order.append(node)
            for child in bn.outGoing(node):
                name = child.getID()
                count[name] -= 1
                if count[name] == 0:
                    next.append(bn.forID(name))
        cur,next = next,[]

    return order
