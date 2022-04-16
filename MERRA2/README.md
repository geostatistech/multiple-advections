
## DATA DOWNLOAD LINKS:

- pm2.5 3D: https://disc.gsfc.nasa.gov/datasets/M2I3NVAER_5.12.4/summary

	- subregion for saudi land and ocean region ==> Spatial subset: 26.719,5.625,85.078,42.188 or 33.719,10.625,56.078,33.188
	- wget --http-user=<username> --http-password=<password> --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies --content-disposition -i <url.txt>

- wind 3D: https://disc.gsfc.nasa.gov/datasets/M2I3NPASM_5.12.4/summary
- covariates 3D: https://disc.gsfc.nasa.gov/datasets/M2I3NVASM_5.12.4/summary

## How to run R files

- Make sure all R packages in the file load_packages.R are installed.

- On the terminal run the following codes: 

    ```
    module load intel/2019
    module load gsl/2.6-intel-2019
    module load gcc/10.2.0
    module load openmpi/4.1.0-gcc-10.2.0

    path=`pwd -P`
    SLURM_NTASKS=36

    ESTIMATION_METHOD=REML

    FIT_MODEL=5
    DATASET=pm_20190101

    mpirun -np $SLURM_NTASKS Rscript ./06_estimating_real_data.R  $path $DATASET $SLURM_NTASKS $ESTIMATION_METHOD $FIT_MODEL

    ```
