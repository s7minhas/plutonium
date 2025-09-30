#!/bin/bash

# Run replication for "When the Hegemon is Distracted" paper
# Author: Shahryar Minhas
# Contact: minhassh@msu.edu

echo "========================================="
echo "Starting replication of 'When the Hegemon is Distracted'"
echo "This will take approximately 4-6 hours"
echo "========================================="
echo

# Set working directory to script location
cd "$(dirname "$0")"

# Install packages if needed
echo "Step 0: Installing required R packages..."
echo "----------------------------------------"
Rscript 00_install_pkgs.R
if [ $? -ne 0 ]; then
    echo "Error: Package installation failed"
    exit 1
fi
echo

# Main analysis pipeline
echo "========================================="
echo "MAIN ANALYSIS PIPELINE"
echo "========================================="
echo

echo "Step 1a: Generating US constraint factor scores..."
echo "----------------------------------------"
Rscript 01_factor_score.R
if [ $? -ne 0 ]; then
    echo "Error in 01_factor_score.R"
    exit 1
fi
echo

echo "Step 1b: Estimating latent factor models (2-3 hours)..."
echo "----------------------------------------"
Rscript 01_run_lfm_un.R
if [ $? -ne 0 ]; then
    echo "Error in 01_run_lfm_un.R"
    exit 1
fi
echo

echo "Step 2: Organizing LFM results..."
echo "----------------------------------------"
Rscript 02_org_lfm_results.R
if [ $? -ne 0 ]; then
    echo "Error in 02_org_lfm_results.R"
    exit 1
fi
echo

# Note: 04 must run before 03 to generate mapCol.rda
echo "Step 4: Creating map legend (running before step 3)..."
echo "----------------------------------------"
Rscript 04_larVarMapLegend.R
if [ $? -ne 0 ]; then
    echo "Error in 04_larVarMapLegend.R"
    exit 1
fi
echo

echo "Step 3: Calculating dyadic distances..."
echo "----------------------------------------"
Rscript 03_distCalcsForApp.R
if [ $? -ne 0 ]; then
    echo "Error in 03_distCalcsForApp.R"
    exit 1
fi
echo

echo "Step 5: Generating UN voting network visualizations..."
echo "----------------------------------------"
Rscript 05_uvPlots.R
if [ $? -ne 0 ]; then
    echo "Error in 05_uvPlots.R"
    exit 1
fi
echo

echo "Step 6: Creating distance trend visualizations..."
echo "----------------------------------------"
Rscript 06_distViz.R
if [ $? -ne 0 ]; then
    echo "Error in 06_distViz.R"
    exit 1
fi
echo

echo "Step 7: Merging UN distances with panel data..."
echo "----------------------------------------"
Rscript 07_merge_undist_to_frame.R
if [ $? -ne 0 ]; then
    echo "Error in 07_merge_undist_to_frame.R"
    exit 1
fi
echo

echo "Step 8: Preparing model data..."
echo "----------------------------------------"
Rscript 08_make_model_data.R
if [ $? -ne 0 ]; then
    echo "Error in 08_make_model_data.R"
    exit 1
fi
echo

echo "Step 9: Setting up model specifications..."
echo "----------------------------------------"
Rscript 09_mod_setup.R
if [ $? -ne 0 ]; then
    echo "Error in 09_mod_setup.R"
    exit 1
fi
echo

echo "Step 10: Running mixed-effects models..."
echo "----------------------------------------"
Rscript 10_downstream_lmer.R
if [ $? -ne 0 ]; then
    echo "Error in 10_downstream_lmer.R"
    exit 1
fi
echo

echo "Step 11: Generating coefficient plots..."
echo "----------------------------------------"
Rscript 11_mod_summ.R
if [ $? -ne 0 ]; then
    echo "Error in 11_mod_summ.R"
    exit 1
fi
echo

echo "Step 12: Creating spatial effect maps..."
echo "----------------------------------------"
Rscript 12_reMap.R
if [ $? -ne 0 ]; then
    echo "Error in 12_reMap.R"
    exit 1
fi
echo

# Appendix analyses
echo "========================================="
echo "APPENDIX ANALYSES"
echo "========================================="
echo

echo "Appendix A: Factor loadings visualization..."
echo "----------------------------------------"
Rscript appendix/factLoadings.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/factLoadings.R"
    exit 1
fi
echo

echo "Appendix B: LFM-IRT simulation study..."
echo "----------------------------------------"
Rscript appendix/lfm_irt_sim.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/lfm_irt_sim.R"
    exit 1
fi
echo

echo "Appendix C: Country name fixed effects..."
echo "----------------------------------------"
Rscript appendix/models_cname_fe.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/models_cname_fe.R"
    exit 1
fi
echo

echo "Appendix D: k=5 dimensions robustness check..."
echo "----------------------------------------"
Rscript appendix/models_k5.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/models_k5.R"
    exit 1
fi
echo

# Russia robustness check
echo "========================================="
echo "RUSSIA ALIGNMENT ROBUSTNESS CHECK"
echo "========================================="
echo

echo "Russia Step 1: Preparing data with Russia as focal country..."
echo "----------------------------------------"
Rscript appendix/dstream_russia/01_modData_Russia.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/dstream_russia/01_modData_Russia.R"
    exit 1
fi
echo

echo "Russia Step 2: Setting up model specifications..."
echo "----------------------------------------"
Rscript appendix/dstream_russia/02_modSetup_Russia.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/dstream_russia/02_modSetup_Russia.R"
    exit 1
fi
echo

echo "Russia Step 3: Running mixed-effects models..."
echo "----------------------------------------"
Rscript appendix/dstream_russia/03_downstream_lmer_russia.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/dstream_russia/03_downstream_lmer_russia.R"
    exit 1
fi
echo

echo "Russia Step 4: Generating coefficient plots..."
echo "----------------------------------------"
Rscript appendix/dstream_russia/04_modSumm_russia.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/dstream_russia/04_modSumm_russia.R"
    exit 1
fi
echo

echo "Russia Step 5: Creating spatial maps..."
echo "----------------------------------------"
Rscript appendix/dstream_russia/05_reMap_russia.R
if [ $? -ne 0 ]; then
    echo "Error in appendix/dstream_russia/05_reMap_russia.R"
    exit 1
fi
echo

echo "========================================="
echo "REPLICATION COMPLETE!"
echo "========================================="
echo
echo "All figures have been generated in the graphics/ directory"
echo "- PDF files for all figures"
echo "- High-resolution PNG files (600 DPI) for all figures"
echo
echo "Results are stored in the results/ directory"
echo "Processed data files are in the data/ directory"
echo
echo "For questions, contact: minhassh@msu.edu"
echo "========================================="
