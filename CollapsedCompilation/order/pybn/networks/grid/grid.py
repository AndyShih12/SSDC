#!/usr/bin/env python

import os
import optparse
import random
import sys
from math import log,log10

class Grid:
    def __init__(self,m,n,s):
        self.m,self.n = m,n
        self.s = s
        self.ids = []
        self.edges = []
        self.edge_pots = {}

    def __repr__(self):
        st = '%dx%d\n' % (self.m,self.n)
        for id in self.ids:
            st += '  ' + id
            if int(id)%self.n == 0:
                st += '\n'
        return st

class Pots:
    (RANDOM, RANDOM_P,
     RANDOM_ATTRACTIVE, RANDOM_REPULSIVE, RANDOM_FRUSTRATED,
     ATTRACTIVE, REPULSIVE, FRUSTRATED) = range(8)

    __gen  = { RANDOM:            lambda p,r,flip: r,
               RANDOM_P:          lambda p,r,flip: (p*r,1.0-p*r)[flip],
               RANDOM_ATTRACTIVE: lambda p,r,flip: (p*r,1.0-p*r)[flip],
               RANDOM_REPULSIVE:  lambda p,r,flip: (p*r,1.0-p*r)[flip],
               RANDOM_FRUSTRATED: lambda p,r,flip: (p*r,1.0-p*r)[flip],
               ATTRACTIVE:        lambda p,r,flip: (p,1.0-p)[flip],
               REPULSIVE:         lambda p,r,flip: (p,1.0-p)[flip],
               FRUSTRATED:        lambda p,r,flip: (p,1.0-p)[flip] }

    __flip = { RANDOM:            lambda i,s,r,q: False,
               RANDOM_P:          lambda i,s,r,q: random.random() < q,
               RANDOM_ATTRACTIVE: lambda i,s,r,q: i//s == i%s,
               RANDOM_REPULSIVE:  lambda i,s,r,q: i//s != i%s,
               RANDOM_FRUSTRATED: lambda i,s,r,q: (i//s == i%s) == (r<q),
               ATTRACTIVE:        lambda i,s,r,q: i//s == i%s,
               REPULSIVE:         lambda i,s,r,q: i//s != i%s,
               FRUSTRATED:        lambda i,s,r,q: (i//s == i%s) == (r<q) }

    def __init__(self,s,p,q,pot_type):
        self.s = s
        self.p = p
        self.q = q
        if self.p > 0.5: self.p = 1.0-self.p
        # if self.q > 0.5: self.q = 1.0-self.q
        if not hasattr(Pots,pot_type):
            self.type = None
        else:
            self.type = getattr(Pots,pot_type)
        if type(self.type) is not int or not Pots.__gen.has_key(self.type):
            raise Exception('Unknown potential type: %s' % pot_type)

    def gen_edge_pot(self):
        gen  = Pots.__gen[self.type]
        flip = Pots.__flip[self.type]
        r = random.random
        flipr = r()
        s,sq = self.s, self.s*self.s
        p = self.p
        q = 1-self.q
        pot = [ gen(p,r(),flip(i,s,flipr,q)) for i in xrange(sq) ]
        return pot

def gen_grid(opts):
    grid = Grid(opts.m,opts.n,opts.s)
    num = grid.m*grid.n
    digits = int(log10(num))+1
    id = '%%0%dd' % digits
    grid.ids = [ id % (i+1) for i in xrange(num) ]
    grid.edges = gen_grid_edges(grid.m,grid.n,grid.ids,opts.do_toroid)
    grid.edge_pots = [ opts.pots.gen_edge_pot() for edge in grid.edges ]
    return grid

def gen_grid_edges(m,n,ids,do_toroid):
    edges = []
    for i in xrange(m):
        edges.extend([ (ids[i*n+j],ids[i*n+j+1]) for j in xrange(n-1) ])
        if do_toroid: edges.append( (ids[i*n+n-1],ids[i*n]) )
    for j in xrange(n):
        edges.extend([ (ids[i*n+j],ids[(i+1)*n+j]) for i in xrange(m-1) ])
        if do_toroid: edges.append( (ids[(m-1)*n+j],ids[j]) )
    return edges

def gen_grid_edges_orig(m,n,ids,do_toroid):
    edges = []
    for i in xrange(m):
        edges.extend([ (ids[i*n+j],ids[i*n+j+1]) for j in xrange(n-1) ])
        if do_toroid: edges.append( (ids[i*n+n-1],ids[i*n]) )
    for j in xrange(n):
        edges.extend([ (ids[i*n+j],ids[(i+1)*n+j]) for i in xrange(m-1) ])
        if do_toroid: edges.append( (ids[(m-1)*n+j],ids[j]) )
    return edges

def binary_grid_to_wcnf(grid,filename):
    f = open(filename,'w')
    num_vars = grid.n*grid.m
    num_clauses = len(grid.edges) * grid.s*grid.s
    f.write('c ' + ' '.join(sys.argv) + '\n')
    f.write('p wcnf %d %d\n' % (num_vars,num_clauses))
    # skipping max weight
    for (x,y),pot in zip(grid.edges,grid.edge_pots):
        x,y = int(x),int(y)
        f.write('%.8f %d %d 0\n' % (-log(pot[0]),x,y))
        f.write('%.8f %d %d 0\n' % (-log(pot[1]),x,-y))
        f.write('%.8f %d %d 0\n' % (-log(pot[2]),-x,y))
        f.write('%.8f %d %d 0\n' % (-log(pot[3]),-x,-y))
    f.close()

def binary_grid_to_uai(grid,filename):
    f = open(filename,'w')
    num_vars = grid.n*grid.m
    num_pots = len(grid.edges)
    # num_pots = len(grid.edges) + num_vars

    f.write('MARKOV\n')
    f.write('%d\n' % num_vars)
    f.write('2 ' * num_vars + '\n')
    f.write('%d\n' % num_pots)

    for (x,y) in grid.edges:
        x,y = int(x)-1,int(y)-1
        f.write('2 %d %d\n' % (x,y))

    #for x in xrange(num_vars):
    #    f.write('1 %d\n' % x)

    f.write('\n')

    for pot in grid.edge_pots:
        f.write('%d\n' % len(pot))
        f.write('  %.8f %.8f %.8f %.8f\n' % (pot[0],pot[1],pot[2],pot[3]))

    #for x in xrange(num_vars):
    #    f.write('2\n  1.0 1.0\n')

    f.close()

def binary_grid_to_fastinf(grid,filename):
    f = open(filename,'w')
    num_vars = grid.n*grid.m
    num_pots = len(grid.edges) + num_vars
    # num_pots = len(grid.edges) + num_vars

    f.write('@Variables\n')
    for x in xrange(num_vars):
        f.write('V%d\t2\n' % x)
    f.write('@End\n\n\n')

    f.write('@Cliques\n')
    for x in xrange(num_vars):
        f.write('cliq%d\t1\t%d\t' % (x,x))
        count,neighbors = 0,list()
        for i,(a,b) in enumerate(grid.edges):
            a,b = int(a)-1,int(b)-1
            i = i+num_vars
            if a == x or b == x:
                count += 1
                neighbors.append(i)
        f.write('%d\t' % count)
        for n in neighbors:
            f.write('%d ' % n)
        f.write('\n')
    for i,(a,b) in enumerate(grid.edges):
        a,b = int(a)-1,int(b)-1
        i = i+num_vars
        f.write('cliq%d\t2\t%d %d \t' % (i,a,b))
        f.write('2\t%d %d \n' % (a,b))
    f.write('@End\n\n\n')

    f.write('@Measures\n')
    for x in xrange(num_vars):
        f.write('noName\t1\t2\t0.5 0.5 \n')
    for i,(a,b) in enumerate(grid.edges):
        f.write('noName\t2\t2 2 \t0.5 0.5 0.5 0.5\n')
    f.write('@End\n\n\n')

    f.write('@CliqueToMeasure\n')
    for x in xrange(num_vars):
        f.write('%d\t%d\n' % (x,x))
    for i,(a,b) in enumerate(grid.edges):
        i = i+num_vars
        f.write('%d\t%d\n' % (i,i))
    f.write('@End\n\n\n')

    #f.write('@DirectedMeasures\n')
    #f.write('@End\n\n\n')

    f.close()

def parse_options():
    usage = """%prog [options] [output file]

Generates Ising, Potts model, and spin-glass models, as well as
randomly parametrized grid networks.

Example: %prog -n10 grid.wcnf
  will generate a random 10x10 grid instance

Example: %prog -m3 -n4 -t -p0.1 --pot=ATTRACTIVE
  will generate a 3x4 toroidal grid instance with attractive 
  edge potentials with strength 0.1/0.9

Types of Potentials (used in --pot option):
  RANDOM             phi(x,y) in [0,1)
  RANDOM_P           extreme parameters, i.e., in [0,p) or (1-p,1]
  RANDOM_ATTRACTIVE  attractive potentials, where 
                     x==y is given weight (1-p,1], x!=y is given weight [0,p)
  RANDOM_REPULSIVE   random repulsive potentials
  RANDOM_FRUSTRATED  mixed and random attractive/repulsive potentials
  ATTRACTIVE         attractive potentials, where 
                     x==y is given weight 1-p, x!=y is given weight p
  REPULSIVE          repulsive potentials
  FRUSTRATED         mixed attractive/repulsive potentials
"""

    parser = optparse.OptionParser(usage=usage)
    parser.set_defaults(n=10,s=2,p=0.5,q=0.5,type='RANDOM')
    parser.add_option('-n',dest='n',type='int',
                      help='grid height')
    parser.add_option('-m',dest='m',type='int',
                      help='grid width (defaults to n)')
    parser.add_option('-s',dest='s',type='int',
                      help='number of variable states (default 2)')
    parser.add_option('-t','--toroid',dest='do_toroid',action='store_true',
                      help='create a toroidal grid')

    pgroup = optparse.OptionGroup(parser, 'Potential Options')
    pgroup.add_option('-p',dest='p',type='float',
                      help='probability p in [0.0,0.5], for edge potentials')
    pgroup.add_option('-q',dest='q',type='float',
                      help='probability q in [0.0,0.5], for node potentials')
    pgroup.add_option('--pot',dest='type',type='string',
                      help='potential parametrization (default RANDOM)')
    parser.add_option_group(pgroup)

    (opts, args) = parser.parse_args()
    if len(args) != 1: parser.error('requires output file')
    opts.filename = args[0]
    if opts.m is None: opts.m = opts.n
    opts.pots = Pots(opts.s,opts.p,opts.q,opts.type)
    parser.destroy()
    return opts

if __name__ == '__main__':
    opts = parse_options()
    grid = gen_grid(opts)
    if grid.s == 2:
        ext = os.path.splitext(opts.filename)[-1]
        if   ext == '.wcnf':
            binary_grid_to_wcnf(grid,opts.filename)
        elif ext == '.uai':
            binary_grid_to_uai(grid,opts.filename)
        elif ext == '.fastinf':
            binary_grid_to_fastinf(grid,opts.filename)
        else:
            binary_grid_to_wcnf(grid,opts.filename + '.wcnf')
            binary_grid_to_uai(grid,opts.filename + '.uai')
            binary_grid_to_fastinf(grid,opts.filename + '.fastinf')
            
