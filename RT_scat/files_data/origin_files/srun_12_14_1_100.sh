#!/bin/bash -l                                                                            
# Standard output and error:
#SBATCH -o ./jin_CIV.out.%j
#SBATCH -e ./jin_CIV.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J RT_simulation_Jin
#
# Number of nodes and MPI tasks per node:
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
#
#SBATCH --mail-type=none
#SBATCH --mail-user=jinlim.ast@gmail.com
#
# Wall clock limit:
#SBATCH --time=24:00:00

# Load compiler and MPI modules with explicit version specifications,
# consistently with the versions used to build the executable.
module purge
module load intel/19.1.3 impi/2019.9

export MAX_NUM_WINDOWS=1000000

# Run the program:
srun ./CIV_12_14_1_100.out > sim_12_14_1_100.out

