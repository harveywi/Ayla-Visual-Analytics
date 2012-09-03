package edu.osu.compgeom.linalg;

import java.util.Arrays;

import no.uib.cipr.matrix.DenseMatrix;
import no.uib.cipr.matrix.DenseVector;
import no.uib.cipr.matrix.Matrices;
import no.uib.cipr.matrix.Vector.Norm;
import no.uib.cipr.matrix.sparse.CompDiagMatrix;

public class PCA {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
//		double [][] data = ProteinIO.readCSV_double("c:\\temp\\K.csv");
//		double [] mean = new double[data.length];
//		Matrix X = getX(data, mean);
//		double [][] pc = getPrincipalComponents1(X, 2);
//		ProteinIO.writeCSV(pc, "c:\\temp\\K_reduce.csv");
	}
	
	public static double [][] getPrincipalComponentsFast(double[][] pointList, int numComponents, double [] mean) {
		int ndims = pointList[0].length;
		DenseMatrix X = getX(pointList, mean);
		
		DenseMatrix XTX = new DenseMatrix(ndims, ndims);
		X.transAmult(X, XTX);
		
		double epsilon = .01;
		
		DenseVector [] evecs = new DenseVector[numComponents];
		for (int p = 0; p < numComponents; p++) {
			DenseVector v = new DenseVector(ndims);
			Matrices.random(v);
			v.scale(1 / v.norm(Norm.Two));
			
			while (true) {
				DenseVector vNew = new DenseVector(ndims);
				XTX.mult(v, vNew);
				
				// Gram schmidt
				DenseVector w = new DenseVector(ndims);
				for (int j = 0; j < p; j++) {
					double dot = vNew.dot(evecs[j]);
					w.add(dot, evecs[j]);
				}
				vNew.add(-1, w);
				vNew.scale(1 / vNew.norm(Norm.Two));
				
				// Check for convergence
				double change = Math.abs(v.dot(vNew) - 1);
				
				v = vNew;
				
				if (change < epsilon)
					break;
				
			}
			evecs[p] = v;
		}
		
		double [][] pc = new double[numComponents][ndims];
		
		for (int i = 0; i < numComponents; i++) {
			System.arraycopy(evecs[i].getData(), 0, pc[i], 0, pc[i].length);
		}
		
		return pc;
	}
	
	public static double [][] getPrincipalComponents(double[][] pointList, int numComponents, double [] mean) {
		int ndims = pointList[0].length;
		int nPts = pointList.length;
		DenseMatrix X = getX(pointList, mean);
		if (nPts < ndims)
			return getPrincipalComponents1(X, numComponents);
		else
			return getPrincipalComponents2(X, numComponents);
	}
	
//	public static double [][] getPrincipalComponents(Matrix X, int numComponents, double [] mean) {
//		getMean(X, mean);
//		int ndims = X.getRowDimension();
//		int nPts = X.getColumnDimension();
//		if (nPts < ndims)
//			return getPrincipalComponents1(X, numComponents);
//		else
//			return getPrincipalComponents2(X, numComponents);
//	}
	
	/**
	 * Data are in columns, i.e.
	 * n points
	 * d dimensions
	 * resulting matrix will be (d x n)
	 * @param pointList
	 * @return
	 */
	private static DenseMatrix getX(double [][] pointList, double [] mean) {
		
		DenseMatrix X = new DenseMatrix(pointList);
		
		// Center X matrix by subtracting the mean from each point
		final int numPts = X.numRows();
		final int numDims = X.numColumns();
		
//		double [] variance = new double[mean.length];
//		for (int i = 0; i < numPts; i++) {
//			for (int j = 0; j < variance.length; j++) {
//				double diff = pointList[i][j] - mean[j];
//				variance[j] += diff*diff;
//			}
//		}
//		
//		for (int j = 0; j < variance.length; j++) {
//			variance[j] /= numPts;
//		}
		
		for (int i = 0; i < numPts; i++) {
			for (int j = 0; j < numDims; j++) {
				X.set(i, j, (X.get(i, j) - mean[j]));
			}
		}
		return X;
	}
	
	public static void getMean(double [][] M, double [] mean) {
		int numPts = M.length;
		int numDims = M[0].length;
		for (int i = 0; i < numPts; i++) {
			for (int j = 0; j < numDims; j++) {
				mean[j] += M[i][j];
			}
		}
		
		for (int j = 0; j < mean.length; j++) {
			mean[j] /= ((double)numPts);
		}
	}
	
	/**
	 * number of dimensions < number of points
	 * @param X
	 * @param numComponents
	 * @return
	 */
	private static double [][] getPrincipalComponents2(DenseMatrix X, int numComponents) {
		
		final int numDims = X.numColumns();
		
		
		DenseMatrix XTX = new DenseMatrix(numDims, numDims);
		X.transAmult(X, XTX);
		
		double [][] evecs = new double[numComponents][numDims];
		double [] evals = new double[numComponents];
		CompDiagMatrix D = new CompDiagMatrix(numDims, numDims, new int[]{0});
		for (int i = 0; i < numDims; i++)
			D.set(i,i,1);
		// TODO:  Fix when finding all eigenvectors of matrix
		@SuppressWarnings("unused")
		int nconv = SparseEigenvectors.getEigenvectors(XTX, D, numComponents, evecs, evals);
		
		Eigenpair [] eigenpairs = new Eigenpair[numComponents];
		for (int i = 0; i < numComponents; i++) {
			eigenpairs[i] = new Eigenpair(evals[i], i);
		}
		Arrays.sort(eigenpairs);
		
		// Principal component vectors
		double [][] pc = new double[numComponents][numDims];
		
		for (int i = 0; i < numComponents; i++) {
			int idx = eigenpairs[i].idx;
			for (int j = 0; j < numDims; j++) {
				pc[idx][j] = evecs[idx][j];
			}
		}
		
		return pc;
	}
	
	public static class Eigenpair implements Comparable<Eigenpair> {
		public double eigenval;
		public int idx;
		
		public Eigenpair(double eigenval, int idx) {
			this.eigenval = eigenval;
			this.idx = idx;
		}
		
		@Override
		public int compareTo(Eigenpair o) {
			return Double.compare(o.eigenval, this.eigenval);
		}
	}
	
	/**
	 * number of points < number of dimensions
	 * @param X
	 * @param numComponents
	 * @return
	 */
	private static double [][] getPrincipalComponents1(DenseMatrix X, int numComponents) {
		int numPts = X.numRows();
		int numDims = X.numColumns();
		
		DenseMatrix XXT = new DenseMatrix(numPts, numPts);
		X.transBmult(X, XXT);
		
		double [][] evecs = new double[numComponents][numPts];
		double [] evals = new double[numComponents];
		CompDiagMatrix D = new CompDiagMatrix(numPts, numPts, new int[]{0});
		for (int i = 0; i < numPts; i++)
			D.set(i,i,1);
		// TODO:  Fix when finding all eigenvectors of matrix
		@SuppressWarnings("unused")
		int nconv = SparseEigenvectors.getEigenvectors(XXT, D, numComponents, evecs, evals);
		
		Eigenpair [] eigenpairs = new Eigenpair[numComponents];
		for (int i = 0; i < numComponents; i++) {
			eigenpairs[i] = new Eigenpair(evals[i], i);
		}
		Arrays.sort(eigenpairs);
		
		// Principal component vectors
		double [][] pc = new double[numComponents][numDims];
		
		// Have to take X*e for all eigenvectors e
		for (int i = 0; i < numComponents; i++) {
			int idx = eigenpairs[i].idx;
			DenseVector ePre = new DenseVector(evecs[idx], false);
			DenseVector e = new DenseVector(numDims);
			X.transMult(ePre, e);
			e.scale(1.0 / e.norm(Norm.Two));
			pc[idx] = e.getData();
//			for (int j = 0; j < numDims; j++) {
//				pc[idx][j] = evecs[idx][j];
//			}
//			DenseVector e = new DenseVector(pc[idx], false);
//			X.mult(e, e);
		}
		
		return pc;
	}
}
