# <p align="center"> PLP - Final Project </p> 
## <p align="center"> MCINTYRE: A Monte Carlo System for Probabilistic Logic Programming </p>  

Sampling as descirbed in http://mcs.unife.it/~friguzzi/Papers/Rig13-FI-IJ.pdf


**Running the sampler**

1. Start a SWI-Prolog session at the command line via `swipl`
2. Load the `montecarlo` file/module via `[montecarlo].`
3. Invoke `montecarlo('path_to_object_program', query, Probability).`, where the first parameter denotes the path to a PLP object program and the second denotes the query to be sampled. After successful sampling, Probability will contain the sampled probability of `query` being true under the given `object_program`.

