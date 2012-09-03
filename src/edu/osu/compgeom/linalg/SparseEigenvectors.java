package edu.osu.compgeom.linalg;

import java.util.Arrays;

import no.uib.cipr.matrix.DenseVector;
import no.uib.cipr.matrix.Matrix;
import no.uib.cipr.matrix.sparse.CompDiagMatrix;
import no.uib.cipr.matrix.sparse.CompRowMatrix;

import org.netlib.util.doubleW;
import org.netlib.util.intW;

public class SparseEigenvectors {
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		CompRowMatrix A = getA();
		CompDiagMatrix B = getB();

		double [][] evecs = new double[4][6];
		double [] evals = new double[4];
		getEigenvectors(A, B, 4, evecs, evals);
	}
	
	private static final void solve(CompDiagMatrix M, DenseVector y) {
		double [] y_data = y.getData();
		for (int i = 0; i < y_data.length; i++) {
			y_data[i] /= M.get(i,i);
		}
	}
	
	private static void flipud(double[] d) {
		double [] ret = new double[d.length];
		for (int i = 0; i < ret.length; i++) {
			ret[i] = d[d.length-i-1];
		}
		for (int i = 0; i < ret.length; i++) {
			d[i] = ret[i];
		}
	}

	private static void setColumn(double [] M, int n, int column, double [] vals) {
		for (int i = 0; i < n; i++) {
			M[n*column + i] = vals[i];
		}
	}
	
	private static double [] getColumn(double [] M, int n, int column) {
		double [] ret = new double[n];
		for (int i = 0; i < n; i++) {
			ret[i] = M[n*column + i];
		}
		return ret;
	}
	
	private static int [] checkIpntr(int [] ipntr, int n) {
		int [] inds = new int [] {ipntr[0]-1, ipntr[1]-1};
		int [] cols = new int [] {inds[0] / n, inds[1] / n};
		int [] rows = new int [] {inds[0] % n, inds[1] % n};
		
		if (rows[0] != 0 || rows[1] != 0) {
			throw new RuntimeException("Error checkIpntr:  One of ipntr[1:2] does not refer" +
					" to the start of a column of workd.");
		}
		
		return cols;
	}
	
	private static CompRowMatrix getA() {
		int [][] nz = new int[6][];
		nz[0] = new int[] {1,2};
		nz[1] = new int[] {0,2};
		nz[2] = new int[] {0,1,3};
		nz[3] = new int[] {2,4,5};
		nz[4] = new int[] {3,5};
		nz[5] = new int[] {3,4};
		CompRowMatrix A = new CompRowMatrix(6, 6, nz);
		for (int i = 0; i < 6; i++) {
			for (int j = 0; j < nz[i].length; j++) {
				A.set(i, nz[i][j], 1);
				A.set(nz[i][j], i, 1);
			}
		}
		return A;
	}
	
	private static CompDiagMatrix getB() {
		CompDiagMatrix ret = new CompDiagMatrix(6, 6, new int [] {0});
		ret.set(0,0,2);
		ret.set(1,1,2);
		ret.set(2,2,3);
		ret.set(3,3,3);
		ret.set(4,4,2);
		ret.set(5,5,2);
		
		return ret;
	}
	
	public static int getEigenvectors(Matrix A, CompDiagMatrix B, int numEvecs, double [][] evecs, double [] evals) {
		return getEigenvectors(A, B, numEvecs, evecs, evals, "LA");
	}
	
	public static int getEigenvectors(final Matrix A, CompDiagMatrix B, int numEvecs, double [][] evecs, double [] evals, String which) {
		MultOp op = new MultOp () {
			public void mult(DenseVector x, DenseVector b) {
				A.mult(x, b);
			}
		};
		return getEigenvectors(op, B, numEvecs, evecs, evals, which);
	}
	
	public interface MultOp {
		/**
		 * Overwrites b
		 * @param x
		 * @param b
		 */
		void mult(DenseVector x, DenseVector b);
	}
	
	public static int getEigenvectors(MultOp A_mult, CompDiagMatrix B, int numEvecs, double [][] evecs, double [] evals, String which) {
		// Compute cholesky factorization of B
		/*
	       % eigs(A,B,k,stringSigma), stringSigma~='SM'
		   % A*x = lambda*B*x
		   % Since we can always Cholesky factor B, follow the advice of
		   % Remark 3 in ARPACK Users' Guide, and do not use mode = 2.
		   % Instead, use mode = 1 with OP(x) = R'\(A*(R\x)) and B = I
		   % where R is B's upper triangular Cholesky factor: B = R'*R.
		   % Finally, V = R\V returns the actual generalized eigenvectors of (A,B).
		*/
		// Compute the Cholesky factorization of B.  In our case, B is just
		// a diagonal matrix with positive entries along the diagonal.  The
		// permutation (AMD) will just be the original ordering (i.e. 1,2,3,...,n).
		CompDiagMatrix RB = (CompDiagMatrix)B.copy();
		for (int i = 0; i < RB.numColumns(); i++) {
			double val = Math.sqrt(RB.get(i, i));
			RB.set(i, i, val);
		}
		CompDiagMatrix RBT = (CompDiagMatrix)RB.copy();
		
		// Various requirements for LAPACK interface
		int mode = 1;
		int k = numEvecs;	// number of eigenvalues/eigenvectors to compute
		int n = B.numRows();	// number of rows (columns) in A
		int p = Math.min(Math.max(2*k,80),n);
		System.out.println("Subspace size:  " + p);
		int lworkl = p*(p+8);
		int maxit = Math.max(6000,(int)Math.ceil(2*n/Math.max(p,1)));
//		double [] d = new double[k];
		double [] v = new double[n*p];
		double [] workd = new double[n*3];
		double [] workl = new double[lworkl];
		int ldv = n;
		int [] ipntr = new int[15];
		intW ido = new intW(0);		// reverse communication parameter
		String bmat = "I";	// standard eigenvalue problem
		intW nev = new intW(k);		// number of eigenvalues requested
		int ncv = p;		// number of Lanczos vectors
		int [] iparam = new int[11];
		iparam[0] = 1;
		iparam[2] = maxit;
		iparam[6] = mode;
		boolean [] select = new boolean[p];
		String whch = which;
		doubleW tol = new doubleW(2.2204e-7);
//		doubleW tol = new doubleW(2.2204e-7);
		double [] resid = new double[n];
		for (int i = 0; i < resid.length; i++) {
			resid[i] = Math.random();
		}
		
		intW info = new intW(1);
		double [] oldRitz = new double[numEvecs];
		int iter = 0;
		while (ido.val != 99) {
			org.netlib.arpack.Dsaupd.dsaupd(ido,bmat,n,whch,nev.val,tol,resid,0,ncv,v,0,ldv,iparam,0,ipntr,0,workd,0,workl,0,lworkl,info);
			
			if (info.val < 0) {
				throw new RuntimeException("Arpack dsaupd error:  info = " + info.val);
			}
			
			int [] cols = checkIpntr(ipntr, n);
			
			switch (ido.val) {
			case -1:
				throw new RuntimeException("ido:  Negative one.");
			case 1:
				/*
				 * % OP(x) = R'\(A*(R\x))
                  workd(:,cols(2)) = ...
                     RBTsolve(Amtimes(RBsolve(workd(:,cols(1)))));
				 */
				//v = RB \ u;
				double [] c = getColumn(workd, n, cols[0]);
				DenseVector cIn = new DenseVector(c, false);
				solve(RB, cIn);
				
				// v = A * u
				DenseVector vectorIn = new DenseVector(c);
				DenseVector vectorOut = new DenseVector(c.length);
				A_mult.mult(vectorIn, vectorOut);
				// v = RBT \ u
				solve(RBT, vectorOut);
				setColumn(workd, n, cols[1], vectorOut.getData());
				break;
			case 99:
				// Arpack is all done.
				break;
			default:
				throw new RuntimeException("Defaulted.");
			}
			
			double [] dispvec = new double[p];
			System.arraycopy(workl, ipntr[5], dispvec, 0, p);
			double [] dispvec2 = new double[numEvecs];
			System.arraycopy(dispvec, (int)Math.max(dispvec.length-k-1, 0), dispvec2, 0, dispvec2.length);
			if (!Arrays.equals(oldRitz, dispvec2) && ido.val != 99) {
				System.out.printf("Iteration %d: a few Ritz values of the %d-by-%d matrix:", iter, p, p);
				System.out.println(Arrays.toString(dispvec2));
				oldRitz = dispvec2;
				iter++;
			}
			
		}
		
		// post-processing
		boolean rvec = true;
		double sigma = 0;
		org.netlib.arpack.Dseupd.dseupd(rvec,"All",select,
				0,evals,0,v,0,ldv,sigma,
				bmat,n,whch,nev,tol.val,resid,0,ncv,
				v,0,ldv,iparam,0,ipntr,0,workd,0,workl,0,lworkl,info);
		
		flipud(evals);
		
		 // v(:,1:k) = v(:,k:-1:1);
		for (int i = 0; i < nev.val; i++) {
			for (int j = 0; j < n; j++) {
				int i2 = nev.val - i - 1;
				int idx = n * i2 + j;
				evecs[i][j] = v[idx];
			}
		}
		
		// Have to run one last RBsolve on all of the eigenvectors
		for (int i = 0; i < nev.val; i++) {
			solve(RB, new DenseVector(evecs[i], false));
		}
		
		// TODO:  Check for non-convergence
		if (info.val != 0) {
			System.err.println("Error with ARPACK routine dseupd.  Error code:  " + info.val);
			return -1;
		} else {
			int nconv = iparam[4];
			if (nconv == 0) {
				System.err.printf("None of the %d requested eigenvalues converged.\n", k);
			} else if (nconv < k) {
				System.err.printf("Only %d of the %d requested eigenvalues converged.\n", nconv, k);
			}
			
			if (nconv < k) {
				// Copy the Ritz values into the eigenvalue vector
				System.arraycopy(oldRitz, 0, evals, 0, evals.length);
				
				// Sort the eigenvectors (descending) by Ritz value
				sortByRitz(evecs, oldRitz);
			}
			
			return nconv;
		}
		
	}

	private static double [][] sortByRitz(double[][] evecs, double[] ritz) {
		class Pair implements Comparable<Pair>{
			double val;
			int idx;
			public int compareTo(Pair o) {
				return Double.compare(o.val, val);
			}
		}
		
		Pair [] pairs = new Pair[ritz.length];
		for (int i = 0; i < ritz.length; i++) {
			pairs[i] = new Pair();
			pairs[i].val = ritz[i];
			pairs[i].idx = i;
		}
		
		Arrays.sort(pairs);
		
		double [][] evecsNew = new double[evecs.length][evecs[0].length];
		for (int i = 0; i < pairs.length; i++) {
			System.out.println(pairs[i].val + "\t" + pairs[i].idx);
			System.arraycopy(evecs[pairs[i].idx], 0, evecsNew[i], 0, evecsNew[i].length);
		}
		
		for (int i = 0; i < pairs.length; i++) {
			System.arraycopy(evecsNew[i], 0, evecs[i], 0, evecs[i].length);
		}
		
		return evecsNew;
		
//		double [][] evecsNew = new double[evecs.length][evecs[0].length];
//		for(Map.Entry<Double, Integer> e : tmm.entries()) {
//			System.out.println(e.getKey() + "\t" + e.getValue());
//			int evecID = e.getValue();
//			System.arraycopy(evecs[evecID])
//		}
	}
}
