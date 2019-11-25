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

## Chapter 1: Introduction

## Chapter 2: Flow Cytometry

## Chapter 3: Bernoulli Random Variables

## Chapter 4: Adapted MCMC

## Chapter 5: Hierarchical Approach

## Chapter 6: Application to Flow Cytometry
