#!/usr/bin/env python

import math
import random
import subprocess
# pybn
import net
import util

class DataSet:
    def __init__(self,instances,counts=None):
        self.instances = instances
        if counts is None:
            self.counts = [1]*len(instances)
        else:
            self.counts = counts
        self.N = sum(counts)
        self.num_vars = len(instances[0])

    """log likelihood of a dataset, given the data distribution"""
    def log_likelihood(self):
        ll = 0.0
        for instance,count in zip(self.instances,self.counts):
            ll += count * (math.log(count) - math.log(self.N))
        return ll/self.N

    def __iter__(self):
        for instance,count in zip(self.instances,self.counts):
            yield (instance,count)

    def __len__(self):
        return len(self.instances)

    """
    FILE I/O
    """

    def __repr__(self):
        st = [ "%d %d %d\n" % (self.num_vars,self.N,len(self.instances)) ]
        count_fmt = '%%%dd' % len(str(max(self.counts)))
        for instance,count in zip(self.instances,self.counts):
            st.append( count_fmt % count )
            st.extend( ' %d' % val for val in instance )
            st.append( '\n' )
        return "".join(st)

    def csv(self):
        st = []
        for instance,count in zip(self.instances,self.counts):
            line = [ ','.join( str(val) for val in instance ) ]
            line.append('\n')
            st.extend( line * count )
        return "".join(st)

    """ DNF of the (unique) instances of a dataset """
    def dnf(self):
        username,hostname = util.get_username(),util.get_hostname()
        st = [ 'c Created by %s on %s.\n' % (username,hostname) ]
        st.append( 'p cnf %d %d\n' % (self.num_vars,len(self.instances)) )
        sign = [ "-","" ]
        for instance in self.instances:
            st.extend( '%s%d ' % (sign[x],i+1) for i,x in enumerate(instance) )
            st.append( '0\n' )
        return "".join(st)

    """ given a dataset, compiles an SDD, returning filename """
    def compile_to_sdd(self,dnf_filename,sdd_filename,
                       sdd_path=None,vtree_filename=None,log_filename=None):
        # save DNF to file
        dnf = open(dnf_filename,'w')
        dnf.write(self.dnf())
        dnf.close()

        if sdd_path is None:
            raise Exception("path to sdd compiler must be specified")

        # construct command line
        cmd = [sdd_path,'-d',dnf_filename,'-R',sdd_filename]
        if vtree_filename: cmd.extend( ['-W',vtree_filename] )
        #cmd += ['-t','balanced','-M','-r0'] # AC
        #cmd += ['-t','right','-M','-r0']
        #cmd += ['-t','left','-M','-r0']

        # make call
        logger = None
        if log_filename: logger = open(log_filename,'w')
        code = subprocess.call(cmd,stdout=logger,stderr=logger)
        if log_filename: logger.close()
        return code

    """
    SORT
    """

    def sort_by_count(self):
        sorted_pairs = sorted(zip(self.counts,self.instances),reverse=True)
        self.counts,self.instances = (list(x) for x in zip(*sorted_pairs))

    def sort_by_instance(self):
        sorted_pairs = sorted(zip(self.instances,self.counts))
        self.instances,self.counts = (list(x) for x in zip(*sorted_pairs))

class Learn:
    """ input is a skeleton of the Bayesian network and a dataset.
        after running learn, self.bn is replaced with the learned network. """
    def __init__(self,skeleton,dataset):
        self.bn = skeleton
        self.dataset = dataset
        # set up the indexing
        self.indices = [None]*skeleton.size
        for var,domain in enumerate(skeleton.pot_domains):
            self.indices[var] = net.Index(domain,skeleton.var_sizes)

    """ learn the parameters of a BN from a complete dataset
        psi is a Dirichlet parameter (psi-1 is pseudo-count) """
    def learn(self,psi=1):
        stats = Learn._clone_with_zero_pots(self.bn,psi=psi)

        # learn
        for instance,count in zip(self.dataset.instances,self.dataset.counts):
            for var,(domain,pot) in enumerate(zip(stats.pot_domains,stats.pots)):
                inst = net.project_onto(instance,domain)
                index = self.indices[var].index(inst)
                pot[index] += count

        Learn._normalize_all(stats)
        Learn._vals_to_string(stats,fmt="%.6g")
        self.bn = stats

    def log_likelihood(self):
        ll = 0.0
        bn = self.bn
        for instance,count in zip(self.dataset.instances,self.dataset.counts):
            for var,(domain,pot) in enumerate(zip(bn.pot_domains,bn.pots)):
                inst = net.project_onto(instance,domain)
                index = self.indices[var].index(inst)
                ll += count * math.log(float(pot[index]))

        return ll/self.dataset.N

    """ this creates a clone'd network with zero potentials
        this does not follow the convention that values are strings
        (which is meant to preserve values during format conversions) """
    @staticmethod
    def _clone_with_zero_pots(bn,psi=1):
        network_type = str(bn.type)
        var_sizes = list(bn.var_sizes)
        pot_domains = [ list(domain) for domain in bn.pot_domains ]
        pots = [ [psi-1]*len(pot) for pot in bn.pots ]
        return net.Network(network_type,var_sizes,pot_domains,pots)

    """ normalize all CPT columns """
    @staticmethod
    def _normalize_all(bn):
        for var,pot in enumerate(bn.pots):
            size = bn.var_sizes[var]
            column_count = len(pot)/size
            for col in xrange(column_count):
                start = col * size
                end = start + size
                z = sum(pot[start:end])
                if z == 0:
                    for i in xrange(start,end): pot[i] = 1.0/size
                else:
                    for i in xrange(start,end): pot[i] = float(pot[i])/z

    """ convert bn float values to strings
        this conforms with the convention in pybn.net """
    @staticmethod
    def _vals_to_string(bn,fmt="%.6g"):
        for var,pot in enumerate(bn.pots):
            bn.pots[var] = [ fmt % val for val in pot ]
