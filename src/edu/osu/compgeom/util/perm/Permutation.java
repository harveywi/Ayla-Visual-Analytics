package edu.osu.compgeom.util.perm;

import java.util.Arrays;

public class Permutation {
	public static final void perm2ffact(int[] x, int[] fc, LeftRightArray LR)
	// Convert permutation in x[0,...,n-1] into
	// the (n-1) digit factorial representation in fc[0,...,n-2].
	// We have: fc[0]<n, fc[1]<n-1, ..., fc[n-2]<2 (falling radices)
	// This is the so-called "Lehmer code" of a permutation.
	{
		int n = x.length;
		LR.set_all();
		for (int k = 0; k < n - 1; ++k) {
			// i := number of Set positions Left of x[k], Excluding x[k].
			int i = LR.num_SLE(x[k]);
			LR.get_set_idx_chg(i);
			fc[k] = i;
		}
	}
	
	public static final void ffact2perm(int [] fc, int [] x, LeftRightArray LR)
	// Inverse of perm2ffact():
	// Convert the (n-1) digit factorial representation in fc[0,...,n-2].
	// into permutation in x[0,...,n-1]
	// Must have: fc[0]<n, fc[1]<n-1, ..., fc[n-2]<2 (falling radices)
	{
		int n = fc.length;
	    LR.free_all();
	    for (int k=0; k<n-1; ++k)
	    {
	    	int i = LR.get_free_idx_chg( fc[k] );
	        x[k] = i;
	    }
	    int i = LR.get_free_idx_chg( 0 );
	    x[n-1] = i;
	}
	
	public static void main(String [] args) {
		System.out.println("Testing");
		
		int [] x = {2,7,1,6,3,4,5,8,0,9};
		int [] fc = new int[x.length];
		LeftRightArray LR = new LeftRightArray(x.length);
		Permutation.perm2ffact(x, fc, LR);
		int [] y = new int[x.length];
		Permutation.ffact2perm(fc, y, LR);
		System.out.println(Arrays.toString(x));
		System.out.println(Arrays.toString(fc));
		System.out.println(Arrays.toString(y));
	}
}
