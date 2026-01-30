# When the Hegemon is Distracted: US Foreign Policy Attention and Global Realignment toward China

Authors
---
[Ha Eun Choi](https://www.haeunchoi.com/)
[Max Gallop](https://www.strath.ac.uk/staff/gallopmaxmr/)
[Scott de Marchi](https://scholars.duke.edu/person/scott.demarchi)
[Shahryar Minhas](http://s7minhas.com/)

Abstract
---

This paper examines how US foreign policy distraction affects global realignment patterns toward China. Using latent factor models (LFM) to measure diplomatic alignment through UN voting data, I demonstrate that periods of heightened US military engagement and defense spending correspond with shifts in global diplomatic alignments. The analysis reveals heterogeneous effects across regime types, with democracies and autocracies responding differently to US distraction. These findings contribute to our understanding of how great power competition shapes international relations in a multipolar world.

Publication Outlet
---

British Journal of Political Science

## Replication instructions for the paper

The base directory of the replication archive contains all scripts necessary to produce the figures shown in the paper. The archive includes the following subdirectories (note that these files are also available on Github at [https://github.com/s7minhas/plutonium](https://github.com/s7minhas/plutonium)):

- **data**: Contains all data files used in the analysis including UN voting data, US constraint measures, and country characteristics
- **results**: Contains results from model runs including LFM estimates and regression outputs
- **graphics**: Contains PDF and high-resolution PNG files (600 DPI) for all figures in the manuscript and appendix
- **funcs**: Contains R scripts with helper functions for data processing and visualization
- **appendix**: Contains code for robustness checks and supplementary analyses
- **appendix/dstream_russia**: Contains complete pipeline for Russia alignment robustness check

Replicating the figures in the **main** text will take approximately 4-6 hours on a standard laptop, with the LFM estimation being the most computationally intensive component (2-3 hours).

#### Setup information

All analyses reported in the manuscript and appendix use the following specification (further information on packages used in the analysis is included at the end of the README):

##### R information

```
R version 4.0.0 or higher
Platform: x86_64
Required RAM: 8GB minimum (16GB recommended)

Key packages:
- amen 1.4 (latent factor models)
- lme4 1.1-37 (mixed effects models)
- lmerTest 3.1-3 (p-values for lmer)
- countrycode 0.16 (country code standardization)
- cshapes 2.0 (country shapes and distances)
```

#### Reproducing figures in the manuscript

**Quick Start:** To replicate all analyses and figures at once, simply run:

```bash
bash run_replication.sh
```

This will automatically execute all scripts in the correct order, handle dependencies, and generate all figures. The complete replication takes approximately 4-6 hours.

**Manual Execution:** If you prefer to run scripts individually, all paths are managed through `setup.R` which sets the working directory and defines path shortcuts (dpth for data/, rpth for results/, gpth for graphics/). Scripts should be run in numerical order. Note: `04_larVarMapLegend.R` should be run early as it generates `graphics/mapCol.rda` which is required by scripts 03, 05, and 06.

**Important Notes on Script Execution:**
- All graphics are generated in both PDF format and high-resolution PNG format (600 DPI) suitable for publication

##### Main Analysis Pipeline

- **01_factor_score.R**: Generates US constraint factor scores from defense and conflict data. Inputs: `data/main_constraint_1.dta`, `data/factResults.csv`. Outputs: `graphics/facViz.pdf/.png`, `graphics/screeViz.pdf/.png` (600 DPI).

- **01_run_lfm_un.R**: Estimates latent factor models on UN voting data using the amen package. This is the most computationally intensive step (2-3 hours). Input: `data/arrList_lfm.rda`. Output: `results/unMods.rda`.

- **02_org_lfm_results.R**: Organizes and reduces LFM outputs for downstream analysis. Input: `results/unMods.rda`. Output: `results/modsForApp.rda`.

- **03_distCalcsForApp.R**: Calculates dyadic distances between countries based on LFM positions. Inputs: `results/modsForApp.rda`, `graphics/mapCol.rda`. Output: `results/unDist.rda`.

- **04_larVarMapLegend.R**: Creates map legend for network visualizations. Input: `data/frame.rda`. Outputs: `graphics/mapLeg.png`, `graphics/mapCol.rda`.

- **05_uvPlots.R**: Generates UN voting network visualizations for 2000 and 2019. Inputs: `results/modsForApp.rda`, `graphics/mapCol.rda`. Outputs: `graphics/un00.pdf/.png`, `graphics/un19.pdf/.png` (600 DPI).

- **06_distViz.R**: Creates dyadic distance trend visualizations showing alignment over time. Inputs: `results/modsForApp.rda`, `graphics/mapCol.rda`. Output: `graphics/distViz.pdf/.png` (600 DPI).

- **07_merge_undist_to_frame.R**: Merges UN voting distances with country-year panel data. Inputs: `data/frame.rda`, `results/unDist.rda`. Output: `data/dyadData.rda`.

- **08_make_model_data.R**: Prepares final regression dataset combining all covariates. Inputs: `data/dyadData.rda`, `data/wbData.rda`, `data/p5v2018.csv`, `data/natural_security_index.csv`, `data/main1_final.csv`, `data/codelist.rda`, `data/geoInfo.rda`. Output: `data/modData.rda`.

- **09_mod_setup.R**: Configures model specifications for mixed-effects regression. Input: `data/modData.rda`. Output: `results/modelInfoFin.rda`.

- **10_downstream_lmer.R**: Runs mixed-effects regression models using lme4. Inputs: `data/modData.rda`, `results/modelInfoFin.rda`. Output: `results/lmerModsFin.rda`.

- **11_mod_summ.R**: Generates coefficient plots from regression results. Inputs: `results/modelInfoFin.rda`, `results/lmerModsFin.rda`. Outputs: `graphics/agreeFixedDistract.pdf/.png`, `graphics/agreeVarDistract.pdf/.png` (600 DPI).

- **12_reMap.R**: Creates spatial maps showing regression effects across countries. Inputs: `results/modelInfoFin.rda`, `results/lmerModsFin.rda`. Outputs: `graphics/eMaps.pdf/.png`, `graphics/eMapsv2.pdf/.png` (600 DPI).

#### Reproducing figures in the appendix

All scripts necessary to reproduce appendix figures are located in the `appendix/` directory.

- **factLoadings.R**: Visualizes factor loadings from constraint analysis. Inputs: `data/main_constraint_1.dta`, `data/factLoadings.csv`. Output: `graphics/loadViz.pdf/.png` (600 DPI).

- **lfm_irt_sim.R**: Simulation study validating LFM-IRT model properties. Generates synthetic data and tests model recovery. Output: Simulation results (not saved).

- **models_cname_fe.R**: Country name fixed effects robustness check. Inputs: `data/modData_cname_test.rda`, `results/modelInfoFin.rda`. Output: `graphics/agreeVarDistract_cname.pdf/.png` (600 DPI).

- **models_k5.R**: Robustness check using k=5 latent dimensions instead of k=2. Inputs: `data/modData.rda`, `results/modelInfoFin.rda`. Outputs: `graphics/agreeFixedDistract_k5.pdf/.png`, `graphics/agreeVarDistract_k5.pdf/.png` (600 DPI).

##### Russia Alignment Robustness (appendix/dstream_russia/)

Complete pipeline examining alignment toward Russia instead of China:

- **01_modData_Russia.R**: Prepares data with Russia as focal country. Inputs: `data/dyadData.rda`, `data/wbData.rda`, `data/codelist.rda`, `data/main1_final.csv`, `data/p5v2018.csv`, `data/natural_security_index.csv`, `data/geoInfo.rda`. Output: `data/modData_russia.rda`.

- **02_modSetup_Russia.R**: Configures model specifications for Russia analysis. Input: `data/modData_russia.rda`. Output: `results/modelInfoFin_russia.rda`.

- **03_downstream_lmer_russia.R**: Runs mixed-effects models for Russia alignment. Inputs: `data/modData_russia.rda`, `results/modelInfoFin_russia.rda`. Output: `results/lmerModsFin_russia.rda`.

- **04_modSumm_russia.R**: Generates coefficient plots for Russia models. Inputs: `results/modelInfoFin_russia.rda`, `results/lmerModsFin_russia.rda`. Outputs: `graphics/agreeFixedDistract_russia.pdf/.png`, `graphics/agreeVarDistract_russia.pdf/.png` (600 DPI).

- **05_reMap_russia.R**: Creates spatial maps for Russia alignment effects. Inputs: `results/modelInfoFin_russia.rda`, `results/lmerModsFin_russia.rda`. Outputs: `graphics/eMaps_russia.pdf/.png`, `graphics/eMapsv2_russia.pdf/.png` (600 DPI).

#### Key Data Files

**Input Data (data/):**

- `arrList_lfm.rda`: UN voting arrays prepared for LFM estimation
- `p5v2018.csv`: Polity V democracy scores
- `natural_security_index.csv`: US national security strategy indicators
- `main_constraint_1.dta`: US constraint factors (F1: Active conflicts, F2: Defense spending)
- `main1_final.csv`: Main analysis dataset
- `factResults.csv`, `factLoadings.csv`: Factor analysis outputs
- `frame.rda`, `codelist.rda`, `geoInfo.rda`, `wbData.rda`: Supporting datasets

**Generated Results (results/):**

- `unMods.rda`: Raw LFM estimation output
- `modsForApp.rda`: Processed LFM results
- `unDist.rda`: Dyadic distances from LFM positions
- `lmerModsFin.rda`, `modelInfoFin.rda`: Main regression results
- `lmerModsFin_russia.rda`, `modelInfoFin_russia.rda`: Russia robustness results

**Generated Graphics Data (graphics/):**

- `mapCol.rda`: Color mapping data for network visualizations (generated by 04_larVarMapLegend.R)

#### R package requirements

The `amen` package must be installed from GitHub:

```r
devtools::install_github('s7minhas/amen')
```

Run `00_install_pkgs.R` to install all required packages. Key packages and versions:

|                |                   |                 |                  |
|:---------------|:------------------|:----------------|:-----------------|
|amen 1.4        |lme4 1.1-37        |lmerTest 3.1-3   |ggplot2 4.0.0     |
|tidyverse 2.0.0 |Cairo 1.6-2        |haven 2.5.5      |reshape2 1.4.4    |
|countrycode 0.16|WDI 2.7.9          |gridExtra 2.3    |cshapes 2.0       |
|extrafont 0.19  |RColorBrewer 1.1-3 |scales 1.1.1     |patchwork 1.1.1   |
|here 1.0.1      |network 1.17.1     |doParallel 1.0.15|foreach 1.5.2     |

If you find any errors or have any further questions, please address them to Shahryar Minhas at minhassh@msu.edu.
