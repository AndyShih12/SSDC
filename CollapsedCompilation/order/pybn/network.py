import edu.ucla.belief.BeliefNetworkImpl as BN
import edu.ucla.belief.FiniteVariableImpl as FiniteVariable
import edu.ucla.belief.io.hugin.HuginNodeImpl as HuginNode
import edu.ucla.belief.Table as Table
import edu.ucla.belief.TableShell as TableShell
import java.lang.String as String
import jarray

def node(name,states,ntype=FiniteVariable):
    if   ntype is FiniteVariable:
        var = ntype(name,states)
    elif ntype is HuginNode:
        var = ntype(name,states,{})
    return var

def noisy_or_cpts(effect,causes,params,leak_param=None,divorce=False):
    states = effect.instances()
    sup_names = [ "%s_%s_sup" % (var.ID,effect.ID) for var in causes ]
    and_names = [ "%s_%s_and" % (var.ID,effect.ID) for var in causes ]
    suppressors = [ node(name,states) for name in sup_names ]
    and_effects = [ node(name,states) for name in and_names ]
    cpts = {}
    for suppressor,param in zip(suppressors,params):
        cpts[suppressor] = Table([suppressor],[param,1.0-param])
    vals = [ 0.0,1.0,1.0,0.0,0.0,1.0,0.0,1.0 ]
    for cause,suppressor,and_effect in zip(causes,suppressors,and_effects):
        cpts[and_effect] = Table([cause,suppressor,and_effect],vals)
    if leak_param:
        leak_name = "%s_leak" % effect.ID
        leak_node = node(leak_name,states)
        leak_vals = [leak_param,1.0-leak_param]
        cpts[leak_node] = Table([leak_node],leak_vals)
        and_effects.append(leak_node)
    if divorce and len(and_effects) > 1:
        or_id = 0
        or_vals = [1.0,0.0,1.0,0.0,1.0,0.0,0.0,1.0]
        while len(and_effects) > 2:
            or_name = "%s_or_%d" % (effect.ID,or_id)
            or_id += 1
            or_node = node(or_name,states)
            effect_1 = and_effects[0]
            effect_2 = and_effects[1]
            cpts[or_node] = Table([effect_1,effect_2,or_node],or_vals)
            and_effects = [or_node] + and_effects[2:]
        cpts[effect] = Table(and_effects+[effect],or_vals)
    elif divorce:
        cpts[effect] = Table(and_effects+[effect],[1.0,0.0,0.0,1.0])
    else:
        vals = [1.0,0.0] * (2**len(and_effects) - 1) + [0.0,1.0]
        cpts[effect] = Table(and_effects+[effect],vals)
    return cpts

def noisy_and_cpts(effect,causes,params,leak_param=None):
    states = effect.instances()
    act_names = [ "%s_%s_act" % (var.ID,effect.ID) for var in causes ]
    or_names  = [ "%s_%s_or"  % (var.ID,effect.ID) for var in causes ]
    activators = [ node(name,states) for name in act_names ]
    or_effects = [ node(name,states) for name in or_names ]
    cpts = {}
    for activator,param in zip(activators,params):
        cpts[activator] = Table([activator],[param,1.0-param])
    vals = [ 1.0,0.0,1.0,0.0,1.0,0.0,0.0,1.0 ]
    for cause,activator,or_effect in zip(causes,activators,or_effects):
        cpts[or_effect] = Table([cause,activator,or_effect],vals)
    if leak_param:
        leak_name = "%s_leak" % effect.ID
        leak_node = node(leak_name,states)
        leak_vals = [1.0-leak_param,leak_param]
        cpts[leak_node] = Table([leak_node],leak_vals)
        or_effects.append(leak_node)
    vals = [1.0,0.0] + [0.0,1.0] * (2**len(or_effects) - 1)
    cpts[effect] = Table(or_effects+[effect],vals)
    return cpts

def belief_network(cpts):
    varToPot = dict( (var,TableShell(cpts[var])) for var in cpts.keys() )
    return BN(varToPot)
