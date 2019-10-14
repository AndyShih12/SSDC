#!/usr/bin/env jython

import arthur.Data as Data
import arthur.EmUtil as Util
import arthur.EDML as EDML
import arthur.ExpectationMaximizationMap as EM
import arthur.ExpectationMaximizationMapAlt as EMALT
import il2.model.BayesianNetwork as BN
import il2.model.Domain as Domain
import il2.model.Table as Table
import il2.util.IntSet as IntSet
import java.util.Random as Random


def parent_set_iterator(ordering):
    for ind in xrange(2**len(ordering)):
        yield [ parent for i,parent in enumerate(ordering) if (ind/2**i)%2 ]

def structure_iterator(ordering):
    if len(ordering) == 1:
        var = ordering[0]
        yield { var:[] }
    else:
        var = ordering[-1]
        ordering_m1 = ordering[:-1]
        for structure in structure_iterator(ordering_m1):
            for parents in parent_set_iterator(ordering_m1):
                structure[var] = parents
                yield structure

# stolen from http://docs.python.org/library/itertools.html
def permutation_iterator(iterable, r=None):
    pool = tuple(iterable)
    n = len(pool)
    r = n if r is None else r
    if r > n:
        return
    indices = range(n)
    cycles = range(n, n-r, -1)
    yield tuple(pool[i] for i in indices[:r])
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i+1:] + indices[i:i+1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield tuple(pool[i] for i in indices[:r])
                break
        else:
            return

def make_skeleton_bn(domain,ordering,families):
    cpts = []
    for var in ordering:
        parents = families[var]
        parents = [ domain.index(parent) for parent in parents ]
        parents.sort()
        cpt_vars = IntSet(parents + [domain.index(var)])
        cpt = Table(domain,cpt_vars)
        cpts.append(cpt)
    return BN(cpts)

def size_of_bn(bn):
    domain = bn.domain()
    size = 0
    for var,cpt in enumerate(bn.cpts()):
        var_size = domain.size(var)
        cpt_size = len(cpt.values())
        cpt_size = (var_size-1)*(cpt_size/var_size)
        size += cpt_size
    return size

def random_network(bn,seed=0):
    r = Random(seed)
    return BN(Util.randomNetwork(bn,r))

def run_em(bn,data,counts=None,seed=None,iters=1024,eps=1e-4,psi=1,stats={}):
    em = EM(bn,data,counts)
    em.setEmParameters(iters,eps)
    em.setPrior(psi)

    if seed is None: seed = random_network(bn,0)
    cpts = em.em(seed.cpts())
    log_likelihood,log_prior,score = em.logmap,0.0,0.0
    #log_likelihood = Util.logLikelihood(cpts,data,counts)
    #log_prior = Util.logPrior(cpts,em.prior)
    #score = log_likelihood + log_prior
                
    stats['log_map'] = score
    stats['log_likelihood'] = log_likelihood
    stats['log_prior'] = log_prior
    stats['iterations'] = em.iterations
    stats['residual'] = em.residual
    stats['map_residual'] = em.map_residual
    stats['learn_time'] = em.learn_time

    return BN(cpts)

def run_edml(bn,data,counts=None,seed=None,
             iters=1024,eps=1e-4,psi=1,stats={}):
    ed = EDML(bn,data,counts)
    ed.setEdParameters(iters,eps,0.5)
    ed.setPrior(psi)

    if seed is None: seed = random_network(bn,0)
    cpts = ed.compensate(seed.cpts())
    log_likelihood,log_prior,score = ed.logmap,0.0,0.0
    #log_likelihood = Util.logLikelihood(cpts,data,counts)
    #log_prior = Util.logPrior(cpts,ed.prior)
    #score = log_likelihood + log_prior
                
    stats['log_map'] = score
    stats['log_likelihood'] = log_likelihood
    stats['log_prior'] = log_prior
    stats['iterations'] = ed.iterations
    stats['residual'] = ed.residual
    stats['learn_time'] = ed.learn_time

    return BN(cpts)
