package edu.osu.compgeom.util.perm;

//This file is part of the FXT library.
//Copyright (C) 2010, 2012 Joerg Arndt
//License: GNU General Public License version 3 or later,
//see the file COPYING.txt in the main directory.

//whether to include some O(n) methods for testing purposes
//#define LR_WITH_DUMB_METHODS  // default off


//Maintain index array [0,...,n-1], keep track which index is set or free.
//Allows, in time O(log(n)), to
//- find k-th free index (where 0<=k<=num_free())
//- find k-th set index (where 0<=k<=num_set())
//- determine how many indices are free/set to the left/right of
//an absolute index i (where 0<=i<n).
public class LeftRightArray {
 public int [] fl_;  // Free indices Left (including current element) in bsearch interval
 public boolean [] tg_;   // tags: tg[i]==true if and only if index i is free
 public int n_;    // total number of indices
 public int f_;    // number of free indices

 public LeftRightArray(int n)
 {
     n_ = n;
     fl_ = new int[n_];
     tg_ = new boolean[n_];
//     free_all();
 }

 int num_free() { return f_; }
 int num_set() { return  n_ - f_; }
 boolean is_free(int i) { return  tg_[i]; }
 boolean is_set(int i) { return  ! tg_[i]; }

 private final void init_rec(int i0, int i1)
 // Set elements of fl[0,...,n-2] according to all indices free.
 // The element fl[n-1] needs to be set to 1 afterwards.
 // Work is O(n).
 {
     if ( (i1-i0)!=0 )
     {
         int t = (i1+i0)/2;
         init_rec(i0, t);
         init_rec(t+1, i1);
     }
     fl_[i1] = i1-i0+1;
 }
 
 public void free_all()
 // Mark all indices as free.
 {
     f_ = n_;
     for (int j=0; j<n_; ++j)  tg_[j] = true;
     init_rec(0, n_-1);
     fl_[n_-1] = 1;
 }

 void set_all()
 // Mark all indices of as set.
 {
     f_ = 0;
     for (int j=0; j<n_; ++j)  tg_[j] = false;
     for (int j=0; j<n_; ++j)  fl_[j] = 0;
 }


public int get_free_idx(int k) 
 // Return the k-th ( 0 <= k < num_free() ) free index.
 // Return ~0UL if k is out of bounds.
 // Work is O(log(n)).
 {
     if ( k >= num_free() )  return Integer.MIN_VALUE;

     int i0 = 0,  i1 = n_-1;
     while ( true )
     {
         int t = (i1+i0)/2;
         if ( (fl_[t] == k+1) && (tg_[t]) )  return t;

         if ( fl_[t] > k )  // left:
         {
             i1 = t;
         }
         else   // right:
         {
             i0 = t+1;  k-=fl_[t];
         }
     }
 }


 public int get_free_idx_chg(int k)
 // Return the k-th ( 0 <= k < num_free() ) free index.
 // Return ~0UL if k is out of bounds.
 // Change the arrays and fl[] and tg[] reflecting
 //   that index i will be set afterwards.
 // Work is O(log(n)).
 {
     if ( k >= num_free() )  return Integer.MIN_VALUE;

     --f_;

     int i0 = 0,  i1 = n_-1;
     while ( true )
     {
     	int t = (i1+i0)/2;

         if ( (fl_[t] == k+1) && (tg_[t]) )
         {
             --fl_[t];
             tg_[t] = false;
             return t;
         }

         if ( fl_[t] > k )  // left:
         {
             --fl_[t];
             i1 = t;
         }
         else    // right:
         {
             i0 = t+1;  k-=fl_[t];
         }
     }
 }


 public int get_set_idx(int k) 
 // Return the k-th ( 0 <= k < num_set() ) set index.
 // Return ~0UL if k is out of bounds.
 // Work is O(log(n)).
 {
     if ( k >= num_set() )  return Integer.MIN_VALUE;

     int i0 = 0,  i1 = n_-1;
     while ( true )
     {
     	int t = (i1+i0)/2;
         // how many elements to the left are set:
     	int slt = t-i0+1 - fl_[t];

         if ( (slt == k+1) && (tg_[t]==false) )  return t;

         if ( slt > k )  // left:
         {
             i1 = t;
         }
         else   // right:
         {
             i0 = t+1;  k-=slt;
         }
     }
 }

 public int get_set_idx_chg(int k)
 // Return the k-th ( 0 <= k < num_set() ) set index.
 // Return ~0UL if k is out of bounds.
 // Change the arrays and fl[] and tg[] reflecting
 //   that index i will be freed afterwards.
 // Work is O(log(n)).
 {
     if ( k >= num_set() )  return Integer.MIN_VALUE;

     ++f_;

     int i0 = 0,  i1 = n_-1;
     while ( true )
     {
     	int t = (i1+i0)/2;
         // how many elements to the left are set:
     	int slt = t-i0+1 - fl_[t];

         if ( (slt == k+1) && (tg_[t]==false) )
         {
             ++fl_[t];
             tg_[t] = true;
             return t;
         }

         if ( slt > k )  // left:
         {
             ++fl_[t];
             i1 = t;
         }
         else   // right:
         {
             i0 = t+1;  k-=slt;
         }
     }
 }


 // The methods num_[FS][LR][IE](ulong i) return the number of
 // Free/Set indices Left/Right if (absolute) index i, Including/Excluding i.
 // Return ~0UL if i >= n.

 int num_FLE(int i) 
 // Return number of Free indices Left of (absolute) index i (Excluding i).
 // Work is O(log(n)).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }  // out of bounds

     int i0 = 0,  i1 = n_-1;
     int ns = i;  // number of set element left to i (including i)
     while ( true )
     {
         if ( i0==i1 )  break;

         int t = (i1+i0)/2;
         if ( i<=t )  // left:
         {
             i1 = t;
         }
         else   // right:
         {
             ns -= fl_[t];
             i0 = t+1;
         }
     }

     return  i-ns;
 }

 int num_FLI(int i) 
 // Return number of Free indices Left of (absolute) index i (Including i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return num_FLE(i) + (tg_[i] ? 1 : 0);
 }


 int num_FRE(int i) 
 // Return number of Free indices Right of (absolute) index i (Excluding i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return  num_free() - num_FLI(i);
 }

 int num_FRI(int i) 
 // Return number of Free indices Right of (absolute) index i (Including i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return  num_free() - num_FLE(i);
 }


 int num_SLE(int i) 
 // Return number of Set indices Left of (absolute) index i (Excluding i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return i - num_FLE(i);
 }

 int num_SLI(int i) 
 // Return number of Set indices Left of (absolute) index i (Including i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return i - num_FLE(i) + (!tg_[i] ? 1 : 0);
 }


 int num_SRE(int i) 
 // Return number of Set indices Right of (absolute) index i (Excluding i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return  num_set() - num_SLI(i);
 }

 int num_SRI(int i) 
 // Return number of Set indices Right of (absolute) index i (Including i).
 {
     if ( i >= n_ )  { return Integer.MIN_VALUE; }
     return  num_set() - i + num_FLE(i);
 }
}
