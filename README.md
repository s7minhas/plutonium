## Replication instructions for the project

The replication archive is organized into three main directories:

- **01_prepData**: contains the scripts necessary to process data downloaded from the variables that feed into the network preference generation models. The inputs and outputs from this directory are all included in the **data** directory.
  - 01_buildSample.R: Steps for building the sample utilized in this analysis
  - 02_[]: Each of these scripts processes raw data on alliances (02_atop.R), trade agreements (02_desta.R), etc that will be fed into our network models
  - 03_buildFrame.R: In this script, we merge together the various data inputs into a directed-dyad-year data.frame object.
  - 04_[]: These scripts organize the data into a format that will be readable to the network models that we will use for preference estimation.
- **02_runMLTR**: contains the scripts necessary to run and then extract relevant estimates from the network models. The key input for the scripts in this directory is **frame.rda** from the **data** directory and all of the outputs are in **results**.
  - 01_runLFM.R: This script generates economic, diplomatic, and ICEWS preference indices between countries over time using a latent factor model.
  - 01_runMLTR_ML.R: This script generates the same preference indices using the multilinear tensor regression (MLTR) model estimated via maximum likelihood. The ML version is run mostly for testing purposes, the results from this estimation procedure do not show up in the proposal document.
  - 01_runMLTR.R: This script parallels the above but employs a Bayesian estimation of the MLTR framework.
  - 02_[]: In these scripts, we extract the preference measurements from each of the models and organize into a directed-dyad-year data.frame object that we will use in the downstream analyses.
- **03_downstreamAnalyses**: contains the scripts and associated data files to conduct the downstream analysis described in our proposal. This directory takes as inputs **frame.rda** from the **data** directory and modeling output from the **results** directory.
  - 01_mergeDyadFiles.R: Simply merges together the various model outputs as well as inputs.
  - 02_systemLevel.R: Conducts the simple downstream analysis described in the proposal.
  - main1_final.csv: This is the only file here that is not generated using the scripts in this archive. This file contains variables that we use to proxy US distraction and the files that it is built from are contained in the **03_downstreamAnalyses/data_us_commit** directory.
  - aggData.csv: This file contains the dataset that we use for our downstream analysis. The associated codebook is labeled data_dictionary.xlsx.

Other files/directories included in this archive:

- **setup.R**:
  - Workspace setup code that is called at the top of each script in the project
- **data**:
  - Storage of all inputs used in the network models.
- **Funcs/mltrFuncs**:
  - Various scripts that are necessary to estimate the MLTR model
- **results**:
  - Output files from the network models

## Path management

The **here** library is used to manage paths in this project. The .here file is located at the root directory of the project.

## Setup information

All of the analyses reported are run with the following specification (further information on packages used in the analysis is included at the end of the README):

```
R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default
locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
[5] LC_TIME=English_United States.1252

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_4.0.3 tools_4.0.3
```

## R package build notes

Below I provide the version of each of the libraries that the project relies on (each library was built using R 4.0.3). Additionally, please note that I use a tailored version of [Peter Hoff's AMEN package](http://pdhoff.github.io/amen/). Installing the version of AMEN that is used in this analysis can be done with the `devtools` package by running the following command in an R session: `devtools::install_github('s7minhas/amen')`. Also please note that the version of countrycode used in this work is much older than the current version available on CRAN.

|                    |               |                |                  |
|:-------------------|:--------------|:---------------|:-----------------|
|abind: 1.4-5        |amen: 1.4      |Cairo: 1.5-12   |countrycode: 0.16 |
|doParallel: 1.0.15  |dplyr: 0.8.5   |extrafont: 0.17 |foreach: 1.5.0    |
|ggplot2: 3.3.0      |ggplot2: 3.3.0 |here: 1.0.1     |imfr: 0.1.9.1     |
|RColorBrewer: 1.1-2 |readr: 1.3.1   |reshape2: 1.4.4 |tidyr: 1.0.2      |
|tidyverse: 1.3.0    |WDI: 2.7.0     |                |                  |

If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.
