#### Main script to process the meta-analysis of exudation effect on soil carbon decomposition and nutrient mineralization#
#### Code developed by: Mingkai Jiang
#### Project leads: Mingkai Jiang + Jiquan Li
####

################################# Set-up ################################# 

#### clear wk space
rm(list=ls(all=TRUE))

############## source all libraries
source("prepare.R")


################################# Get input ################################# 
#### There are many different types of experiments:
#### 1. Exudation:          Studies with real root exudation explicitily estimated, 
####                        and the associated causal-response relationships.
####                        These studies must be performed in well-controlled environment. 
#### 2. Substrate Addition: A proxy substrate is used to simulate the effect of root exudation,
####                        with the associated plant/ecossytem causal-response relationships.
#### 3. Exclusion/Girdling: Implicitly estimate the effect of root exudation by excluding plants/root C release.
####                        Huge assumption must be made, hence potentially not useful.
#### 4. Stable isotope:     Studies with the ability to trace the movement of added element,
####                        potentially useful to estimate the turnover time and fraction absorption.
#### 5. Perturbation:       Studies that perturb the environment/plant (e.g. drought).
####                        We can still extract useful information under ambient treatment. 


############## read input files and assign column names
process_individual_study_and_save_new_table()


### results so far summary:
### 1. There is a relationship between priming effect (cumulative) with time and total substrate addition,
###    which is expected. 
### 2. The dataset is not large, and is further complicated by data quality and consistency issues:
###    2.1. Substrate type;
###    2.2. Substrate addition rate and frequency;
###    2.3. Response varaible (cumulative and one-off measurement);
### 3. There are also other issues to consider:
###    3.1. Priming effect vs. CO2 efflux;
###    3.2. Microbial biomass and response ratios;
###    3.3. Lack of information on soil SOC and nutrient conditions;
### 4. We will need a robust and consistent method for the incubation experiment.
### 5. Further analyses should focus on:
###    5.1. Normalize the response to per unit substrate added;
###    5.2. Calculate priming effect wherever possible;
###    5.3. Add exudation and mineralization relationship based on the other dataset;
###    5.4. Check outlier datasets;
###    5.5. Fit a non-linear relationship to each individual dataset, between priming effect and
###         time. Compare the coefficients across different datasets.
###











### Convert unit whenever possible, 
### whilst maintaining the same dataset structure and variable
myDF1 <- convert_response_variable_unit(inDF=myDF1)


#### to do list:
### 1. add additional soil property information, e.g bulk density, disturbed, plant age, nutrient addition
### 2. consistent unit
### 3. convert response variables where possible
### 4. Link between microbial biomass response, enzyme activities, and C release/mineralization
### 5. preliminary analysis
### 6. code from ogle
### 7. add other dataset, esp. nutrient manipulation
### 8. write notes about experiment


### Calculate all the effect size (abs diff and ratio)
myDF1 <- calculate_effect_size(inDF=myDF1)


### only look at priming effect
response.variables <- unique(myDF1$Response_variable)



### end.
