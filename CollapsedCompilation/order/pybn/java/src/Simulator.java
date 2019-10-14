import edu.ucla.belief.BeliefNetwork;
import edu.ucla.belief.FiniteVariable;
import il2.model.BayesianNetwork;
import il2.model.Domain;
import il2.model.Table;
import il2.util.IntMap;
import il2.util.IntSet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;

public class Simulator {
    BayesianNetwork bn;
    Domain d;
    il2.util.Graph g;
    List<Integer> roots;
    List<Integer> internal;
    List<Integer> leaves;
    List<Integer> nodes;

    public Simulator(BayesianNetwork bn) {
        this.bn = bn;
        this.d = bn.domain();
        g = bn.generateGraph();
        IntSet rts = g.roots(), lvs = g.leaves(), ins = g.vertices();
        this.roots = toList(rts);
        this.leaves = toList(lvs);
        this.internal = toList(ins.diff(rts).diff(lvs));
        this.nodes = toList(g.vertices());
    }

    public BayesianNetwork getBayesianNetwork() { return bn; }
    public int numRoots()     { return roots.size(); }
    public int numInternal()  { return internal.size(); }
    public int numLeaves()    { return leaves.size(); }
    public int numNodes()     { return nodes.size(); }

	//////////////////////////////////////////////////
	// sampling
	//////////////////////////////////////////////////

    public int[] world(Random r) {
        return Simulator.world(bn,r);
    }

    /**
     * draw sample w (a world) from Pr() induced by network bn
     */
    public static int[] world(BayesianNetwork bn, Random r) {
        Domain d = bn.domain();
        Table[] cpts = bn.cpts();
        int[] w = new int[d.size()];
		
        for (int var = 0; var < d.size(); var++) {
            Table cpt = cpts[var];
            int[] xu = project(w,cpt.vars());
            xu[xu.length-1] = 0;
            // the following only works since x = 0
            int uindex = cpt.getIndexFromFullInstance(xu);
            double[] vals = cptColumn(cpt,uindex);
            normalize(vals);

			w[var] = vals.length-1; //default
			double sum = 0.0;
			double rand = r.nextDouble();
			for ( int i = 0; i < vals.length; i++ ) {
                sum += vals[i];
				if ( rand <= sum ) {
                    w[var] = i;
                    break;
                }
			}
		}
		return w;
	}

	//////////////////////////////////////////////////
	// evidence
	//////////////////////////////////////////////////

    /**
     * convert a world to an il2 evidence (IntMap)
     */
    public static IntMap worldToIntMap(int[] w) {
        IntMap e = new IntMap();
        for (int var = 0; var < w.length; var++)
            if ( w[var] >= 0 ) e.put(var,w[var]);
        return e;
    }

    /**
     * convert a world to an il1 evidence (Map)
     */
    public Map<FiniteVariable,Object> worldToEvidence(BeliefNetwork bn,
                                                      int[] w) {
        Map<FiniteVariable,Object> e = new HashMap<FiniteVariable,Object>();
        for (int var = 0; var < w.length; var++) {
            if ( w[var] < 0 ) continue;
            String varName = d.name(var);
            FiniteVariable var1 = (FiniteVariable)bn.forID(varName);
            String instName = d.instanceName(var,w[var]);
            Object instance = var1.instance(instName);
            e.put(var1,instance);
        }
        return e;
    }

    /**
     * convert a world to an il1 evidence (Map)
     */
    public Map<FiniteVariable,Object> intMapToEvidence(BeliefNetwork bn,
                                                       IntMap w) {
        Map<FiniteVariable,Object> e = new HashMap<FiniteVariable,Object>();
        IntSet vars = w.keys();
        for (int i = 0; i < vars.size(); i++) {
            int var = vars.get(i);
            String varName = d.name(var);
            FiniteVariable var1 = (FiniteVariable)bn.forID(varName);
            String instName = d.instanceName(var,w.get(var));
            Object instance = var1.instance(instName);
            e.put(var1,instance);
        }
        return e;
    }

	//////////////////////////////////////////////////
	// simulating incomplete data
	//////////////////////////////////////////////////

    /**
     * each variable/value pair is "missing" with probability p
     */
    public static void hideAtRandom(int[] world, double p, Random r) {
        for (int var = 0; var < world.length; var++)
            if ( r.nextDouble() <= p )
                world[var] = -1;
    }

    /**
     * each variable/value pair is "missing" with probability p = k/n
     */
    public static void hideAtRandom(int[] world, int k, Random r) {
        double p = (double)k/(double)world.length;
        hideAtRandom(world,p,r);
    }

	//////////////////////////////////////////////////
	// utility methods
	//////////////////////////////////////////////////

    /**
     * extract a column from a CPT
     */
    static double[] cptColumn(Table cpt, int uindex) {
        int var = familyChild(cpt.vars());
        int size = cpt.domain().size(var);
        double[] vals = cpt.values();
        int usize = vals.length/size;

        double[] xvals = new double[size];
        for (int state = 0; state < size; state++)
            xvals[state] = vals[state*usize + uindex];

        return xvals;
    }

    /**
     * returns a family's child variable
     */
    static int familyChild(IntSet vars) {
        int size = vars.size();
        return vars.get(size-1);
    }

    static void normalize(double[] vals) {
        double alpha = 0.0;
        for (int state = 0; state < vals.length; state++)
            alpha += vals[state];
        for (int state = 0; state < vals.length; state++)
            vals[state] /= alpha;
    }

    /**
     * convert IntSet to a list
     */
    List<Integer> toList(IntSet vars) {
        List<Integer> list = new ArrayList<Integer>(vars.size());
        for (int i = 0; i < vars.size(); i++)
            list.add(vars.get(i));
        return list;
    }

    /**
     * project world w onto parents u, where vars=xu (assumes last
     * index of vars is child x)
     */
    static int[] project(int[] w, IntSet vars) {
        int[] x = new int[vars.size()];
        for (int i = 0; i < x.length; i++)
            x[i] = w[vars.get(i)];
        return x;
    }

    /*
	public int[] randomWorld() {
		Domain d = bn.domain();
        int[] w = new int[d.size()];
		for (int var=0; var<d.size(); var++) 
            w[var] = r.nextInt(d.size(var));
		return w;
	}
    */


    /*
    public static int sum(int[] list) {
        int sum = 0;
        for (int i = 0; i < list.length; i++)
            sum += list[i];
        return sum;
    }
    */

    /**
     * generates a complete il2 data set
     */ 
    /*
    public int[][] nextDataSet(int N, Random r) {
        int[][] data = new int[N][];
        for (int i = 0; i < N; i++)
            data[i] = simulateWorld(bn2,r);
        return data;
    }
    */

    /**
     * converts an il2 data set to an il1 data set
     */
    /*
    public List<Map<FiniteVariable,Object>> convertData(int[][] data2) {
        int N = data2.length;
        List<Map<FiniteVariable,Object>> data1 = 
            new ArrayList<Map<FiniteVariable,Object>>(N);
        for (int[] d2 : data2) {
            Map<FiniteVariable,Object> d1 = 
                new HashMap<FiniteVariable,Object>(d2.length);
            for (int var2 = 0; var2 < d2.length; var2++) {
                int val2 = d2[var2];
                if ( val2 < 0 ) continue;
                FiniteVariable var1 = index2var[var2];
                Object val1 = var1.instance(val2);
                d1.put(var1,val1);
            }
            data1.add(d1);
        }
        return data1;
    }
    */

	//////////////////////////////////////////////////
	// generating incomplete data
	//////////////////////////////////////////////////

    /**
     * hide k elements from set in data
     */
    /*
    public static void hideSet(int[][] data, List<Integer> inputSet, int k, 
                               Random r) {
        List<Integer> set = new ArrayList<Integer>(inputSet);

        if ( k > set.size() )
            k = set.size();
        Collections.shuffle(set,r);
        for (int[] world : data) {
            for (int i = 0; i < k; i++) {
                int var = set.get(i);
                world[var] = -1;
            }
        }
    }
    */

    /**
     * hide k elements from set in data
     */
    /*
    public void hideSetAtRandom(int[][] data, List<Integer> set, int k) {
        double p = (double)k/(double)set.size();
        for (int[] world : data) {
            for (int i = 0; i < set.size(); i++) {
                int var = set.get(i);
                if ( r.nextDouble() <= p )
                    world[var] = -1;
            }
        }
    }
    public void hideLeavesAtRandom(int[][] data, int k) 
    { hideSetAtRandom(data,leaves,k); }
    */

    /*
    public void hideRoots(int[][] d, int k, Random r) {hideSet(d,roots,k,r);}
    public void hideInternal(int[][] d, int k, Random r) {hideSet(d,internal,k,r);}
    public void hideLeaves(int[][] d, int k, Random r) {hideSet(d,leaves,k,r);}
    public void hideNodes(int[][] d, int k, Random r) {hideSet(d,nodes,k,r);}
    */

	//////////////////////////////////////////////////
	// stuff
	//////////////////////////////////////////////////

    /*
    static IntMap[] dataToEvidence(int[][] data) {
        IntMap[] evid = new IntMap[data.length];
        for (int i = 0; i < data.length; i++)
            evid[i] = worldToEvidence(data[i]);
        return evid;
    }

    static UniqueData dataToUniqueData(int[][] data) {
        LinkedHashMap<int[],Integer> unique = 
            new LinkedHashMap<int[],Integer>();
        WorldComparator wc = new WorldComparator();
        java.util.Arrays.sort(data,wc);
        int i = 0;
        while ( i < data.length ) {
            int[] di = data[i];
            int count = 1;
            i++;
            while ( i < data.length && wc.compare(di,data[i]) == 0 ) {
                count++;
                i++;
            }
            unique.put(di,new Integer(count));
        }
        int n = unique.size();
        int[][] udata = new int[n][];
        int[] counts = new int[n];
        i = 0;
        for (int[] di : unique.keySet()) {
            udata[i] = di;
            counts[i] = unique.get(di);
            i++;
        }
        return new UniqueData(data.length,udata,counts);
    }
    */

    /**
     * representation of a dataset in terms of its unique examples
     */
    static class UniqueData {
        int N;
        int[][] data;
        int[] counts;
        public UniqueData(int N, int[][] data, int[] counts) {
            this.N = N;
            this.data = data;
            this.counts = counts;
        }
    }

    /**
     * representation of a soft dataset in terms of its unique examples
     */
    static class UniqueSoftData {
        int N;
        double[][] lambdas;
        int[] counts;
        public UniqueSoftData(int N, double[][] lambdas, int[] counts) {
            this.N = N;
            this.lambdas = lambdas;
            this.counts = counts;
        }
    }

    /**
     * for sorting worlds in lexicographic order
     */
    static class WorldComparator implements java.util.Comparator<int[]> {
        public WorldComparator() {}
        public int compare(int[] w1, int[] w2) {
            for (int i = 0; i < w1.length; i++) {
                if      ( w1[i] < w2[i] ) return -1;
                else if ( w1[i] > w2[i] ) return 1;
            }
            return 0;
        }
    }
}
