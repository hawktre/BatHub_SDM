This folder contains all the code.  Numbered scripts are meant to be executed in order to recreate this analysis. 
  
File | Description
---|---------------------------------------------------------------------
00_covariate_retrieval_processing.R | Code for retrieval and formatting of environmental covariates
01_compile_spp_tables.R | This code is used to create species history matrices by querying the NABat Acoustic Database.
02_MaxEnt_Prep.R | Handles final formatting steps of species history matrices and environmental covariates and runs MaxEnt. Output is `.rds` file containing model results. 
03_results.R | Exploration and visualization of model results
05_Application.R | Uses model results in a worked example for presentation given to NABat partners (non-essential).
Archive | contains archived code, not needed for analysis.
maxent | local installation of maxent.jar needed to run original implementation of maxent.
