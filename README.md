# Attribute translations promote pro-environmental choice through distinct cognitive mechanisms
This repository contains the code for the project "Attribute translations promote pro-environmental choice through distinct cognitive mechanisms". 
The results are published in the article: **TBA**. 
Authors are Barbara Oberbauer, Ulf J.J. Hahnel, and Sebastian Gluth.

The osf-project and preregistration of Study 2 can be found here: https://doi.org/10.17605/OSF.IO/Z2C5E.

The task code of Study 2 can be found here: https://github.com/barbaraoberbauer/meat_task. 

The dataset of Study 1 was collected by Mertens et al. (2020) and results on choice behavior and information acquisition are reported in their [publication](https://doi.org/10.1017/S1930297500006896). The raw data set was publicly made available on the Open Science Framework (https://osf.io/fqdra/). The raw data from Study 2, all preprocessed behavioral data, all parameter estimation results as well as simulated behavioral data of both studies reported are available at https://doi.org/10.5281/zenodo.20792856. These data allow reproducing all results figures provided with this paper.

Up until publication, this repository will be frequently updated.

----


### Content of Repository
This repository contains the R skripts for preprocessing the behavioral data (following the procedure by Mertens et al., 2020), performing additional behavioral analyses, and fitting a Bayesian hierarchical multi-attribute attentional Drift Diffusion Model [(Yang & Krajbich, 2023)](https://psycnet.apa.org/buy/2022-20750-001). 

We have estimated parameters for two versions of the model: one version bounds the attentional parameters theta and phi between 0 and 1, the other version allows for the parameters to take on any value. The results published in the manuscript stem from the **unbounded** version, the current code allows for the bounded version to be fitted and compared to the bounded model.  

Overview of the repositorys contents:
- **R** - contains all functions as well as the plot theme
- **scripts** - contain scripts for performing all analyses 

    - **01_Preprocessing** - contains all scripts used for preprocessing
    - **02_Behavioral_Analysis** - contains all scripts for performing the behavioral and RT analyes including code to generate figures
    - **03_Process_Tracing_Analysis** - contains all scripts for performing the process-tracing analyses including code to generate figures
    - **04_Modeling** - contains all scripts to estimate parameters, assess the diagnostics of fits, validate fits, perform the parameter recovery, compare models, and analyze the parameters, alongside text files specifying the models

Model fitting is performed using [JAGS](https://mcmc-jags.sourceforge.io/). 


---

### Installation

- set up RStudio and JAGS, dowload JAGS Wiener module (see versions and links below)
- **clone the repository** 
- set up the folders "**data**" (by downloading the data, see above) and "**figures**" within the repository (will be ignored by git, see .gitignore)
- add **DBDA2E-utilities.R** (by John Kruschke) to folder "functions" (download here: https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R; will be ignored by git, see .gitignore)


---

### Common Parameters

#### Built With

- RStudio 2024.09.0
- [JAGS 4.3.2](https://mcmc-jags.sourceforge.io/)
- [JAGS Wiener 4.3.1](https://sourceforge.net/projects/jags-wiener/files/Windows/) 
- Package [runjags](https://cran.r-project.org/web/packages/runjags/index.html)

#### Corresponding Author

Barbara Oberbauer, barbara.oberbauer@uni-hamburg.de

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17341128.svg)](https://doi.org/10.5281/zenodo.17341128)



