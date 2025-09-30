####################################
# install and load required pkgs
# with specific versions
####################################

# set cran repository
r = getOption('repos')
r['CRAN'] = 'http://cran.rstudio.com/'
options(repos = r)
rm(r)

# remotes
if(!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes", repos='http://cran.rstudio.com/')
}

# devtools
if(!requireNamespace("devtools", quietly = TRUE)){
  install.packages("devtools", repos='http://cran.rstudio.com/')
}


# install amen from s7minhas/amen
devtools::install_github('s7minhas/amen')

# define package versions from the original environment
package_versions = list(
  'abind' = '1.4-5',
  'Cairo' = '1.5-12',
  'countrycode' = '0.16',
  'doParallel' = '1.0.15',
  'dplyr' = '0.8.5',
  'extrafont' = '0.17',
  'foreach' = '1.5.0',
  'ggplot2' = '3.3.0',
  'here' = '1.0.1',
  'imfr' = '0.1.9.1',
  'RColorBrewer' = '1.1-2',
  'readr' = '1.3.1',
  'reshape2' = '1.4.4',
  'tidyr' = '1.0.2',
  'tidyverse' = '1.3.0',
  'WDI' = '2.7.0'
)

# helper func to check if correct version is installed
check_version = function(pkg_name, target_version = NULL){
  if(pkg_name %in% installed.packages()[,1]){
    current_version = as.character(packageVersion(pkg_name))
    if(!is.null(target_version)){
      return(current_version == target_version)
    } else {
      return(TRUE)
    }
  }
  return(FALSE)
}

# helper func to load pkgs with specific versions
loadPkg = function(toLoad, versions = NULL){
  for(lib in toLoad){
    # get target version if specified
    target_version = NULL
    if(!is.null(versions) && lib %in% names(versions)){
      target_version = versions[[lib]]
    }
    
    # check if correct version is installed
    needs_install = FALSE
    if(!(lib %in% installed.packages()[,1])){
      needs_install = TRUE
      cat(paste0("Package '", lib, "' not found. Installing...\n"))
    } else if(!is.null(target_version)){
      current = as.character(packageVersion(lib))
      if(current != target_version){
        needs_install = TRUE
        cat(paste0("Package '", lib, "' version mismatch. ",
                   "Current: ", current, ", Target: ", target_version, "\n"))
      }
    }
    
    # install if needed
    if(needs_install){
      if(!is.null(target_version)){
        cat(paste0("Installing ", lib, " version ", target_version, "...\n"))
        tryCatch({
          remotes::install_version(lib, 
                                   version = target_version,
                                   repos = 'http://cran.rstudio.com/',
                                   upgrade = "never",
                                   quiet = TRUE)
        }, error = function(e){
          cat(paste0("Failed to install specific version. Trying current version...\n"))
          install.packages(lib, repos='http://cran.rstudio.com/')
        })
      } else {
        cat(paste0("Installing ", lib, " (latest version)...\n"))
        install.packages(lib, repos='http://cran.rstudio.com/')
      }
    }
    
    # load the package
    suppressMessages(suppressWarnings(library(lib, character.only=TRUE)))
  }
}

# 
core_packages = c(
  # data wrangling
  'tidyverse',      # data toolkit
  'plyr',           # data manipulation (load before dplyr)
  'reshape2',       # reshaping data
  'lubridate',      # date/time handling
  'tidyr',          # separate from tidyverse to control version
  'dplyr',          # separate from tidyverse to control version
  'readr',          # separate from tidyverse to control version
  
  # stats modeling
  'lme4',           # mixed effects models
  'lmerTest',       # p-values for lmer
  'amen',           # latent factor models for networks
  
  # network analysis
  'network',        # network objects
  
  # viz
  'ggplot2',        # plotting
  'gridExtra',      # arranging plots
  'viridis',        # color palettes
  'scales',         # scale funcs
  'cowplot',        # pub-ready plots
  'RColorBrewer',   # color palettes
  'Cairo',          # graphics device
  'sf',             # spatial features
  'extrafont',      # additional fonts
  
  # tables
  'stargazer',      # regression tables
  'xtable',         # latex tables
  
  # data import
  'haven',          # spss/stata/sas files
  'here',           # project-relative paths
  'imfr',           # IMF data
  
  # country/geo data
  'countrycode',    # country name standardization
  'cshapes',        # country shapes and distances
  
  # parallel computing
  'parallel',       # base parallel computing
  'doParallel',     # parallel backend for foreach
  'foreach',        # parallel loops
  
  # other utils
  'WDI',            # world bank data download
  'abind'           # array binding
)

# packages with specific versions (from the comment)
versioned_packages = c(
  'abind', 'Cairo', 'countrycode', 'doParallel', 
  'dplyr', 'extrafont', 'foreach', 'ggplot2', 'here', 
  'imfr', 'RColorBrewer', 'readr', 'reshape2', 'tidyr', 
  'tidyverse', 'WDI'
)

# packages without specific versions
unversioned_packages = setdiff(core_packages, versioned_packages)

# install packages with specific versions first
for(pkg in versioned_packages){
  if(pkg %in% core_packages){
    loadPkg(pkg, versions = package_versions)
  }
}

# install packages without specific versions
loadPkg(unversioned_packages, versions = NULL)


# summary of installed pkgs with version check
installed = core_packages[core_packages %in% installed.packages()[,1]]
not_installed = core_packages[!core_packages %in% installed.packages()[,1]]

# check version compliance
for(pkg in versioned_packages){
  if(pkg %in% installed.packages()[,1]){
    current = as.character(packageVersion(pkg))
    target = package_versions[[pkg]]
    if(!is.null(target)){
      if(current == target){
        cat(paste0("✓ ", pkg, ": ", current, " (correct)\n"))
      } else {
        cat(paste0("✗ ", pkg, ": ", current, " (target was ", target, ")\n"))
      }
    }
  } else {
    cat(paste0("✗ ", pkg, ": not installed\n"))
  }
}

## system and package information
# sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
#
# Matrix products: default
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
# [5] LC_TIME=English_United States.1252
#
# locale:
# [1] C
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# loaded via a namespace (and not attached):
# [1] compiler_4.0.3 tools_4.0.3

# packs = c(
# 	'countrycode', 'reshape2', 'tidyr', 'dplyr', 'abind',
# 	'ggplot2', 'tidyverse', 'ggplot2', 'extrafont', 'Cairo',
# 	'RColorBrewer', 'doParallel', 'foreach', 'amen', 'WDI',
# 	'readr', 'imfr', 'here'
# )
# packs = sort(packs)
# info = installed.packages()[packs,'Version']
# toTab = paste0(packs, ': ', info)
# names(toTab) = NULL
# toTab = c(toTab, '', '')
# toTab = matrix(toTab, nrow=5, ncol=4, byrow=TRUE)
# knitr::kable(toTab)
# |                    |               |                |                  |
# |:-------------------|:--------------|:---------------|:-----------------|
# |abind: 1.4-5        |amen: 1.4      |Cairo: 1.5-12   |countrycode: 0.16 |
# |doParallel: 1.0.15  |dplyr: 0.8.5   |extrafont: 0.17 |foreach: 1.5.0    |
# |ggplot2: 3.3.0      |ggplot2: 3.3.0 |here: 1.0.1     |imfr: 0.1.9.1     |
# |RColorBrewer: 1.1-2 |readr: 1.3.1   |reshape2: 1.4.4 |tidyr: 1.0.2      |
# |tidyverse: 1.3.0    |WDI: 2.7.0     |                |                  |
