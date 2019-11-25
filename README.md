# Flow Cytometry Analysis

## Usage
To use the code for each of the chapters detailed below the following packages
need to be installed. The code below will install, where needed, and load all 
of the required packages.

```r
# Install CRAN packages
pkgs <- c('ggplot2', 'grid', 'reshape', 'scales', 'bmp',
          'RColorBrewer', 'SDMTools', 'stargazer', 'devtools',
          'rpart', 'rattle', 'rpart.plot')
pkgs_not_installed <- pkgs[!(pkgs %in% installed.packages())]
for (pkg in pkgs_not_installed) {
  install.packages(pkg, dependencies = TRUE, 
                   character.only = TRUE, type = 'binary')
}

# Install FlowClust
devtools::install_bioc('flowClust', type = 'binary')

# Load all packages
lapply(c(pkgs, 'flowClust'), library, character.only = TRUE)
```

Additionally, to remove scientific notation of the numeric display values run:
```r
options(scipen = 9999)
```

## Chapter 1: Introduction
Produces a plot which displays:
1. Rituximab data;
2. Subjective Manual Gating;
3. FlowClust Gating;
4. Lattice Structure of data;
5. Probability Map using Multi-Resolution Analysis;
6. Clusters using Multi-Resolution Analysis.

## Chapter 2: Flow Cytometry
Produces plots of `flowClust` clustering on the Rituximab and GvHD Control 
cytometry datasets.

## Chapter 3: Bernoulli Random Variables
Introduces the Generalised Binomial distribution with functions to compute PDF,
CDF, quantiles and random samples. Generates plots which display:
- properties of the Generalised Binomial Distribution;
- statistical properties of the distribution for given sized structures;
- compare analytical and simulation results;

*Note:* the code in this file will take considerable time to run since the
simulations are considerably large.

## Chapter 4: Adapted MCMC
Using MCMC methods, and the methodology derived in Chapter 3 to clean and
segment two image files.

## Chapter 5: Hierarchical Approach
Implementing multi-resolution components on top of the core methodology of 
Chapter 3 and the adapted MCMC approach of Chapter 4.

## Chapter 6: Application to Flow Cytometry
Applies the methods discussed and shown in Chapters 3, 4, and 5 to the 
rituximab and GvHD positive and control flow cyotometry samples. A comparison
to the methods of Lo et al. is also included.