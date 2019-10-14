#!/usr/bin/env jython

import os
import math
import java.util.Random as Random
import il2.inf.structure.EliminationOrders as EO

import edu.ucla.belief.inference.JoinTreeInferenceEngineImpl as il1_jt
import edu.ucla.belief.recursiveconditioning.RCInferenceEngine as il1_rc
import il2.inf.jointree.UnindexedSSAlgorithm as ss
import il2.inf.jointree.NormalizedSSAlgorithm as ss_norm
import il2.inf.jointree.UnindexedHuginAlgorithm as hugin
import il2.inf.jointree.UnindexedZCAlgorithm as zc
import il2.inf.jointree.NormalizedZCAlgorithm as zc_norm
import il2.inf.rc.RCEngine as rc
import il2.util.IntList as IntList

import edu.ucla.belief.inference.JEngineGenerator as il1_jt_gen
import edu.ucla.belief.recursiveconditioning.RCEngineGenerator as il1_rc_gen

import java.lang.System.nanoTime as nanoTime

il1_engines = [ il1_jt, il1_rc ]
il2_engines = [ ss, ss_norm, hugin, zc, zc_norm, rc ]
engines = il2_engines

"""For convenience."""
def start_engine(bn,ie_alg=zc_norm):
    if   ie_alg in il1_engines:
        start_il1_engine(bn,ie_alg)
    elif ie_alg in il2_engines:
        start_il2_engine(bn,ie_alg)
    else:
        raise Exception('invalid inference engine')

"""For convenience, return an exact (il1) inference engine for a
BeliefNetwork."""
def start_il1_engine(bn,ie_alg=il1_jt):
    generators = { il1_jt : il1_jt_gen, il1_rc : il1_rc_gen }
    generator = generators[ie_alg]()
    return generator.manufactureInferenceEngine(bn)

"""For convenience, return an exact (il2) inference engine for a
BayesianNetwork."""
def start_il2_engine(bn,ie_alg=zc_norm,order=None):
    cpts = bn.cpts()
    subdomains = list(cpts)
    if order is None:
        order = EO.minFill(subdomains,6,Random(0)).order
        # order = EO.minSize(subdomains).order
    if ie_alg is rc:
        ie = rc.create(cpts,order)
        ie.fullCaching()
    else:
        jt = EO.traditionalJoinTree(subdomains,order)
        # jt = EO.bucketerJoinTree(subdomains,order)
        start = nanoTime()
        ie = ie_alg(jt,cpts)
        ie.compilationTime = (nanoTime()-start)*1e-9
    return ie

"""Run's inflib's minfill algorithm rep number of times."""
def minfill(bn,reps=6):
    cpts = bn.cpts()
    subdomains = list(cpts)
    record = EO.minFill(subdomains,reps,Random(0))
    order = record.order.toArray()
    size = record.size
    return order,size

"""Run's inflib's minsize algorithm."""
def minsize(bn):
    cpts = bn.cpts()
    subdomains = list(cpts)
    record = EO.minSize(subdomains)
    order = record.order.toArray()
    size = record.size
    return order,size

def get_jointree_stats(bn,order):
    cpts = bn.cpts()
    subdomains = list(cpts)
    jt = EO.traditionalJoinTree(subdomains,IntList(order))
    c_stats = jt.getClusterStats()
    s_stats = jt.getSeparatorStats()
    jt_stats = [ c_stats.getMax(), c_stats.getTotal(),
                 s_stats.getMax(), s_stats.getTotal() ]
    jt_stats = [ math.log(x,2) for x in jt_stats ]
    return jt_stats
