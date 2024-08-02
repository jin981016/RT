#!/bin/bash -l                                                                            
# Standard output and error:
#SBATCH -o ./tjob.out.%j
#SBATCH -e ./tjob.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J RT_bipolar
#
# Number of nodes and MPI tasks per node:
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
#
#SBATCH --mail-type=none
#SBATCH --mail-user=jinlim@gmail.com
#
# Wall clock limit:
#SBATCH --time=24:00:00

# Load compiler and MPI modules with explicit version specifications,
# consistently with the versions used to build the executable.
module purge
module load intel/19.1.3 impi/2019.9

# Run the program:
srun ./sim_test.out > prog_test.out

mpiifort -cpp -DMPI -c *_mod.f90
mpiifort -cpp -DMPI -o CIV.out main_outflow.f90 *.o
