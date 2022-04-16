# multiple-advections 


pbdBASE is a set of bindings to and extensions for the distributed linear algebra libraries BLACS, PBLAS, and ScaLAPACK.  The package is very low-level, and unless you are very familiar with these libraries (or even if you are...), you are instead recommended to see the pbdDMAT and pbdDEMO packages.

original: https://github.com/RBigData/pbdBASE



## Installation

pbdBASE requires:

* A system installation of MPI
* R version 3.0.0 or higher
* The pbdSLAP and pbdMPI packages, as well as their dependencies.

On the terminal, run the following:

  ```
  R CMD build pbdBASE --no-build-vignettes
  R CMD INSTALL pbdBASE_0.5-3.tar.gz
  ```

## Modifying Codes and Re-installing Package

Fortran code covariance matrix generation is in
```
pbdBASE/src/base/utils/dmat_redist.f
```

C code covariance matrix generation is in
```
pbdBASE/src/base_putil.c
```

