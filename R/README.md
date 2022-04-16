
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

  DEPENDENCE_IN_SPACE=STRONG
  DEPENDENCE_IN_TIME=STRONG
  DEPENDENCE_IN_VARIABLE=0.5
  DEPENDENCE_IN_ADVECTION=NEGATIVE

  SIMULATION_MODEL=5

  mpirun -np $SLURM_NTASKS Rscript ./01_simulating_data.R  $path $SIMULATION_MODEL $SLURM_NTASKS $DEPENDENCE_IN_SPACE $DEPENDENCE_IN_TIME $DEPENDENCE_IN_VARIABLE $DEPENDENCE_IN_ADVECTION

  ESTIMATION_METHOD=REML
  SET=1

  #Fitting a multiple advections model
  FIT_MODEL=5
  DATASET=simulated_data_bivariate_matern_multiple_advection_spacetime_N_400_T_5_STRONG_DEPENDENCE_IN_SPACE_STRONG_DEPENDENCE_IN_TIME_0.5_DEPENDENCE_IN_VARIABLE_NEGATIVE_DEPENDENCE_IN_ADVECTION

  mpirun -np $SLURM_NTASKS Rscript ./03_estimating_parameters.R  $path $DATASET $SLURM_NTASKS $ESTIMATION_METHOD $FIT_MODEL $SET

  ```
