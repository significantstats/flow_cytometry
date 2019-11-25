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

## Chapter 2: Flow Cytometry
Produces plots of `r flowClust` clustering on the Rituximab and GvHD Control 
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

## Chapter 5: Hierarchical Approach

## Chapter 6: Application to Flow Cytometry
