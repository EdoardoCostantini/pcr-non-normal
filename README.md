# PCR and non-normal data

Simulation study to compare the out-of-sample prediction performance of Principal Component Regression with non-normal data as input.

## Summary of project

The goal of the study was to understand how different PCR is effect by the non-normality of the distribution of the predictors that are summarised using PCA.
The quality of the representation is assessed based on the prediction error obtained by using the PCs extracted based on the different coding schemes.

### Simulation study procedure

The simulation study procedure involved:

1. Generation of X, the matrix of predictors. This happens based on a reverse SVD computation. 

    - Random sample and orthogonalization of U (true PC scores) and V (true loading matrix) matrices.
    - Compute the true X matrix and added random normal noise with scaled variance to achieve the target cumulative proportion of explained variance.
    - Apply [NORTA](https://edoardocostantini.github.io/posts/series-sampling/norta.html) transformation to obtain multivariate distributions with known rank correlation structure and arbitrary target marginal distributions

2. Generation of y, the variable to be predicted, as a linear combination of either the true components or the NORTA-transformed observed items, with a target proportion of explained variance.
3. Computation of three (true number) PCs.
4. Compute all outcome measures:

    - Out of sample Mean square prediction error (MSE)
    - Similarity (Tucker congruence) between the true and computed pc scores
    - Explained variance by the true number of PCs

### Simulation study fixed factors

These parameters were kept constant in the simulation study:

- sample size (n = 500)
- true number of principal components (q = 3)
- number of principal components computed (npcs = 3)

### Simulation study experimental factors

The simulation study procedure is repeated for each of the conditions resulting by the crossing of the following experimental factors:

- number of items per PC (J = (4, 150), which results in a total of 12 or 450 items)
- marginal distributions of the items (normal, beta, Skewed-t, random mix of these three)
- Proportion of variance explained by the Principal components (PVE = (.5, .8, .9, .99))
- True explained variance by the true predictors in y (R^2 = .3, .7, .9, .99)
- The set of variables (true predictors) used to generate y (\text{tp} = \text{(PCs, items)})
## How to replicate results

### Running the simulation on a PC / Mac

You can also replicate the simulation on a personal computer by following these steps: 

#### 1. Preparation

- Check the script `code/init.R` - 
  This script contains the information of how everything in the simulation study is set up. 
  It stores the definition of the fixed and experimental factors. Before doing anything else,
  check that every value here is what you want it to be.

#### 2. Running the simulation

- Open the script `code/run_sim.R` and set your working directory to its location
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `code/run_sim.R`

#### 3. Getting the results

- Open the script `code/script_pooling.R` and run it to pool the results. 
  Pay attention that the script is reading the latest file saved in the 
  output folder.
- Open the script `code/script_analysis.R` to obtain the plots

## Understanding the codebase

If you want to play around with this simulation study and include conditions of your liking keep in mind the following simulation structure:
- Fixed and experimental factors are provided exclusively by in the
  `init.R`.
- `run_sim.R` is a script that runs in parallel different calls of 
  the subroutine `doRep()` (located: `code/subroutines/doRep.R()`).
  The script calls one instance of `doRep()` for every repetition 
  desired. A *repetition* here is a cycle through all the conditions.
- `doRep()` is a subroutine that calls `runCell()` for every condition 
  in a sequential loop. 
  In this set up, parallelization happens at the level of the repetitions,
  not at the level of the conditions.
- `runCell()` is a subroutine calling a collection of functions to
  actually perform the steps of the simulation:
  1. Generation of X - `generateXTP()` generates the true principal components (T) and observed items (X)
  2. Transformation of X - `norta()` transforms the marginal distribution of the variables to targets
  3. Generation of y - `generateDV()` generates the dependent variable as a linear combination of the true components or of the transformed X, depending on the condition
  4. PC extraction - `extractPCs()` function performs PCA according
  5. Outcome measures are computed - `extractMSE()` the MSE and other desired outcomes are extracted from previously created objects
  6. Storing results - An object containing the outcome measures is stored as .rds file at every repetition for every condition in a temporary output folder.

### Result files

- 20221115_101341.tar.gz 
  - PCA computed using SVD
  - No Spearman correlation used
- 20221121_112535.tar.gz
  - PCA computed using EVD
  - Both Pearson and Spearman correlations used
