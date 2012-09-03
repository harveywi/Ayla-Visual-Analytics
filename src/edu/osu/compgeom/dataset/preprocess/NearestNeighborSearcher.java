package edu.osu.compgeom.dataset.preprocess;

import java.util.PriorityQueue;

public class NearestNeighborSearcher {
	float [][] pts = null;
	
	double [] mu = null;
	double [] stdev = null;
	
//	double [][] mu2 = null;
//	double [][] stdev2 = null;
//	int [][] d2 = null;
	
	double [][] mu4 = null;
	double [][] stdev4 = null;
	int [] d4 = new int[4];
	
	double [][] mu16 = null;
	double [][] stdev16 = null;
	int [] d16 = new int[16];
	
	public NearestNeighborSearcher(float[][] pts) {
		this.pts = pts;
		
		mu = new double[pts.length];
		stdev = new double[pts.length];
		
//		mu2 = new double[pts.length][2];
//		stdev2 = new double[pts.length][2];
//		d2 = new int[pts.length][2];
		
		mu4 = new double[pts.length][4];
		stdev4 = new double[pts.length][4];
		
		mu16 = new double[pts.length][16];
		stdev16 = new double[pts.length][16];
		
		int nDims = pts[0].length;
		
		for (int idx = 0; idx < pts.length; ++idx) {
			mu[idx] = getMean(idx, 0, nDims);
			stdev[idx] = getStdev(mu[idx], idx, 0, nDims);
			
//			final int mod2 = (int)Math.round(pts.length / 2.0);
//			for (int j = 0; j < 2; ++j) {
//				mu2[idx][j] = getMean(idx, mod2*j, Math.min(mod2*(j+1), pts.length));
//				stdev2[idx][j] = getStdev(mu2[idx][j], idx, mod2*j, Math.min(mod2*(j+1), pts.length));
//				d2[idx][j] = Math.min(mod2*(j+1), pts.length) - mod2*j;
//			}
			
			final int mod4 = (int)Math.round(nDims / 4.0);
			for (int j = 0; j < 4; ++j) {
				mu4[idx][j] = getMean(idx, mod4*j, Math.min(mod4*(j+1), nDims));
				stdev4[idx][j] = getStdev(mu4[idx][j], idx, mod4*j, Math.min(mod4*(j+1), nDims));
				d4[j] = Math.min(mod4*(j+1), nDims) - mod4*j;
			}
			
			final int mod16 = (int)Math.round(nDims / 16.0);
			for (int j = 0; j < 16; ++j) {
				mu16[idx][j] = getMean(idx, mod16*j, Math.min(mod16*(j+1), nDims));
				stdev16[idx][j] = getStdev(mu16[idx][j], idx, mod16*j, Math.min(mod16*(j+1), nDims));
				d16[j] = Math.min(mod16*(j+1), nDims) - mod16*j;
			}
		}
	}
	
	private final double euclidDistSq(int idx1, int idx2, int from, int until) {
		double ret = 0;
		for (int i = from; i < until; ++i) {
			double diff = pts[idx1][i] - pts[idx2][i];
			ret += diff*diff;
		}
		
		return ret;
	}
	
	private class DistPair implements Comparable<DistPair> {
		int idx;
		double distSq;
		public DistPair(int idx, double distSq) {
			this.idx = idx;
			this.distSq = distSq;
		}
		@Override
		public int compareTo(DistPair o) {
			return Double.compare(o.distSq, this.distSq);
		}
		
		@Override
		public String toString() {
			return "(" + idx + "," + distSq + ")";
		}
	}
	
	/**
	 * Returns the k-nearest neighbors of the point with index y.  The list of neighbors
	 * is guaranteed to not contain y.
	 * @param y Index of point whose neighbors you wish to find
	 * @param k Number of nearest neighbors of y to find. 
	 * @return List of k-nearest neighbors of y.  Note that y is not in this list.
	 */
	public int[] getNearestNeighbors(int y, int k) {
		int nDims = pts[y].length;
		final int mod16 = (int)Math.round(nDims / 16.0);
		// Seed
//		int xMin = (int)(Math.random() * pts.length);
//		double minDist = euclidDistSq(xMin, y, 0, nDims);
		
		PriorityQueue<DistPair> pq = new PriorityQueue<DistPair>();
		
		for (int x = 0; x < pts.length; ++x) {
			double minDist = pq.size() <= k ? Double.MAX_VALUE : pq.peek().distSq;
			
			if (lb(nDims, mu[x], stdev[x], mu[y], stdev[y]) >= minDist)
				continue;
			
			if (lb4(x, y) >= minDist) {
				continue;
			}
			
			double bnd = lb16(x, y);
			if (bnd >= minDist) {
				continue;
			}
			
			for (int i = 0; i < 16; ++i) {
				int from = i*mod16;
				int until = Math.min((i+1)*mod16, nDims);
				bnd += euclidDistSq(x, y, from, until) - lb(d16[i], mu16[x][i], stdev16[x][i], mu16[y][i], stdev16[y][i]);
				if (bnd >= minDist) {
					break;
				}
			}
			
//			double check1 = lb16(x, y);
//			double check2 = 0;
//			for (int i = 0; i < 16; i++) {
//				check2 += lb(d16[x][i], mu16[x][i], stdev16[x][i], mu16[y][i], stdev16[y][i]);
//			}
//			
//			double check3 = euclidDistSq(x, y, 0, nDims);
//			double check4 = 0;
//			for (int i = 0; i < 16; i++) {
//				int from = i*mod16;
//				int until = Math.min((i+1)*mod16, nDims);
//				check4 += euclidDistSq(x, y, from, until);
//			}
			
			if (bnd < minDist) {
				double trueDist = euclidDistSq(x, y, 0, nDims);
				pq.add(new DistPair(x, trueDist));
				if (pq.size() > k + 1)
					pq.poll();
			}
		}
		
		int [] ret = new int[k];
		for (int idx = k-1; idx >= 1; --idx) {
			ret[idx-1] = pq.poll().idx;
		}
		
		return ret;
	}
	
	private final double lb16(int x, int y) {
		double ret = 0;
		
		for (int i = 0; i < 16; ++i) {
			ret += lb(d16[i], mu16[x][i], stdev16[x][i], mu16[y][i], stdev16[y][i]);
		}
		
		return ret;
	}
	
	private final double lb4(int x, int y) {
		double ret = 0;
		
		for (int i = 0; i < 4; ++i) {
			ret += lb(d4[i], mu4[x][i], stdev4[x][i], mu4[y][i], stdev4[y][i]);
		}
		
		return ret;
	}
	
	private final double lb(int d, double mean1, double stdev1, double mean2, double stdev2) {
		double diffMean = mean1 - mean2;
		double diffStdev = stdev1 - stdev2;
		return d * (diffMean*diffMean + diffStdev*diffStdev);
	}
	
	private final double getMean(int idx, int from, int until) {
		double ret = 0;
		for (int i = from; i < until; ++i) {
			ret += pts[idx][i];
		}
		ret /= ((double)until - from);
		return ret;
	}
	
	private final double getStdev(double mean, int idx, int from, int until) {
		double ret = 0;
		
		for (int i = from; i < until; ++i) {
			double diff = pts[idx][i] - mean;
			ret += diff*diff;
		}
		ret /= ((double)until - from);
		return Math.sqrt(ret);
	}
}
