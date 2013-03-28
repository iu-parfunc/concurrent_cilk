Devlog for Wavefront (Ivar and non-Ivar) 

                            ***** Algorithm *****

* We create an n X n matrix; call it M. With each element 
  being a "tile" of dimension d X d

* Each tile at this point is a matrix of 1s 

                         ******* Functions ******* 

* `do_tile` takes in an index `(i,j)` and changes the tile to :
  - If i and j are greater then zero, we return the `(i-1,j-1)` entry (tile) of M
    and set the kth row of the new tile t (which is all ones), 
    to be t_k = t_k + ((i-1,j-1))_k.
  - If i = 0, j > 0 then we return the (i, j-1) entry of M and set the kth row 
    of the new tile t (which is all ones) to be t_k = t_k + ((i, j-1))_k
  -  If j = 0, i > 0 then we return the (i-1, j) entry of M and set the kth row  
     of the new tile t (which is all ones) to be t_k = t_k + ((i-1, j))_k
  - If i = j = 0.

                      ******* Traversal Stategies *********

* **Parfor1:**  We spawn the computation on each tile. Where we run each row, 
                column and tile in parallel (i.e cilk_for | cilk_for | spawn)

* **Parfor2:** We spawn the computation on each row. 
               On each row the computation is run serially. 
               (i.e we run each row in parallel as opposed to Parfor1
               in which we run each tile in parallel).

* **Divide and Conquer:** We take in (i,j) (which will be our "split point" 
                          and the width and height of our current tile. 
                          We then split it into 4 equal parts and 
                          call `divide_and_conquer` on each of these 4 new parts 
                          until width = height = 1, at 
                          which point we call `do_tile` on the tile. 
                          Currently we spawn one tile on each call to 
                          `divide_and_conquare`.

                          **** Synopsis ****

* the general algorithm for this is then:
  - For each tile T. We say that
    - T_ij =  
