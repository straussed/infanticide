### Infanticide by females is a leading source of juvenile mortality in a large social carnivore
Code and data for Brown et al. 2021

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4889918.svg)](https://doi.org/10.5281/zenodo.4889918)
  
Abstract  
Social animals benefit from their group-mates, so why do they sometimes kill each other’s offspring? Using 30 years of data from multiple groups of wild spotted hyenas, we address three critical aims for understanding infanticide in any species: (1) quantify the contribution of infanticide to overall mortality (2) describe the circumstances under which infanticide occurs and (3) evaluate hypotheses about the evolution of infanticide. We find that, although observed only rarely, infanticide is in fact a leading source of juvenile mortality. Infanticide accounted for 24% of juvenile mortality, and 1 in 10 hyenas born in our population perished due to infanticide. In all observed cases of infanticide, killers were adult females, but victims could be of both sexes. Of four hypotheses regarding the evolution of infanticide, we found the most support for the hypothesis that infanticide in spotted hyenas reflects competition over social status among matrilines.

#### Reproducing the analysis

##### Raw data
This project uses raw data from the Mara Hyena Project. The raw data are pulled using the
00.select_cubs.R script (see below) from the MHP private data repository (v.1.2.88). 

##### Additional data files: 
infanticide_notes.csv - this file includes descriptive information about infanticide events  
cleaned.mortality.csv - this file contains the mortality information for all cases of juvenile mortality in the database. These mortality sources will be merged into the MHP backend. 

##### Scripts
There are three main scripts for producing the analysis  

00.select_cubs.R - This script interacts with a private data repository, so will
not be runable by anyone without access to the data source. However, after pulling
the data from the private repository, this script saves all data used for the analysis
in the Data/cub_data.RData data file. 
----> OUTPUT: cub_data.RData
  
01.causes_of_mortality_and_ages.R - This script sets up the model  of mortality source
as a function of age at death. This model is then used to infer the contribution
of different mortality sources to juvenile mortality.  
----> OUTPUT: known_mortality_cleaned.RData, age_model.RData, prob_mortality_source.png, mortality_source_and_age.png

02.hypothesis_testing.R - This script tests association between mortality source and prey 
density, mortality source and den-dependent cub density, and compares the ranks of killers 
and the mothers of victims. 
----> OUTPUT: prey_and_cub_models.RData, Killer_rank.png, prey_and_cub_mortality.png

##### Supplemental Materials
Supplement.Rmd - this markdown file produces the Supplemental Materials associated with this paper

### Session Info

R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] patchwork_1.1.1  gridExtra_2.3    ggplot2_3.3.2    ggridges_0.5.3   tidyr_1.1.2      here_0.1        
 [7] dplyr_1.0.2      hyenadata_1.2.88 brms_2.15.0      Rcpp_1.0.5      

loaded via a namespace (and not attached):
  [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1       rsconnect_0.8.16     rprojroot_1.3-2     
  [6] markdown_1.1         fs_1.5.0             base64enc_0.1-3      rstudioapi_0.11      farver_2.0.3        
 [11] rstan_2.21.2         remotes_2.2.0        DT_0.16              fansi_0.4.1          mvtnorm_1.1-1       
 [16] bridgesampling_1.0-0 codetools_0.2-16     splines_4.0.3        shinythemes_1.2.0    pkgload_1.1.0       
 [21] bayesplot_1.8.0      projpred_2.0.2       jsonlite_1.7.2       nloptr_1.2.2.2       shiny_1.6.0         
 [26] httr_1.4.2           compiler_4.0.3       backports_1.2.0      assertthat_0.2.1     Matrix_1.2-18       
 [31] fastmap_1.1.0        cli_2.2.0            later_1.1.0.1        htmltools_0.5.1.1    prettyunits_1.1.1   
 [36] tools_4.0.3          igraph_1.2.6         coda_0.19-4          gtable_0.3.0         glue_1.4.2          
 [41] reshape2_1.4.4       V8_3.4.0             vctrs_0.3.6          nlme_3.1-149         crosstalk_1.1.0.1   
 [46] stringr_1.4.0        ps_1.4.0             testthat_2.3.2       lme4_1.1-25          mime_0.9            
 [51] miniUI_0.1.1.1       lifecycle_0.2.0      gtools_3.8.2         devtools_2.3.2       statmod_1.4.35      
 [56] MASS_7.3-53          zoo_1.8-8            scales_1.1.1         colourpicker_1.1.0   promises_1.1.1      
 [61] Brobdingnag_1.2-6    parallel_4.0.3       inline_0.3.16        shinystan_2.5.0      gamm4_0.2-6         
 [66] curl_4.3             memoise_1.1.0        loo_2.3.1            StanHeaders_2.21.0-6 stringi_1.5.3       
 [71] dygraphs_1.1.1.6     desc_1.2.0           checkmate_2.0.0      boot_1.3-25          pkgbuild_1.1.0      
 [76] rlang_0.4.10         pkgconfig_2.0.3      matrixStats_0.57.0   lattice_0.20-41      purrr_0.3.4         
 [81] labeling_0.4.2       rstantools_2.1.1     htmlwidgets_1.5.3    processx_3.4.4       tidyselect_1.1.0    
 [86] plyr_1.8.6           magrittr_2.0.1       R6_2.5.0             generics_0.1.0       pillar_1.4.7        
 [91] withr_2.3.0          mgcv_1.8-33          xts_0.12.1           abind_1.4-5          tibble_3.0.4        
 [96] crayon_1.3.4         usethis_1.6.3        callr_3.5.1          threejs_0.3.3        digest_0.6.27       
[101] xtable_1.8-4         httpuv_1.5.5         RcppParallel_5.0.2   stats4_4.0.3         munsell_0.5.0       
[106] sessioninfo_1.1.1    shinyjs_2.0.0       
