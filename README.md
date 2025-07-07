# Computational Mechanisms of Attribute Translations
This repository contains the code for the project "Computational Mechanisms of Attribute Translations". 
The results are published in the article: **TBA**. 
Authors are Barbara Oberbauer, Ulf Hahnel, and Sebastian Gluth.

The results of the behavioral analyses have previously been published by [Mertens et al. (2020)](https://doi.org/10.1017/S1930297500006896). Data are publicly available on [OSF](https://osf.io/fqdra/).

Up until publication, this repository will be frequently updated.

----


### Content of Repository
This repository contains the R skripts for preprocessing the behavioral data (following the procedure by Mertens et al., 2020), performing additional behavioral analyses, and fitting a Bayesian hierarchical multi-attribute attentional Drift Diffusion Model [(Yang & Krajbich, 2023)](https://psycnet.apa.org/buy/2022-20750-001). 

We have estimated parameters for two versions of the model: one version bounds the attentional parameters theta and phi between 0 and 1, the other version allows for the parameters to take on any value. The results published in the manuscript stem from the **unbounded** version, the current code allows for the bounded version to be fitted, plotted and used for simulating data.  

Overview of the repositorys contents:
- **01_Preprocessing** - contains the script for preprocessing the behavioral data
- **02_Behavioral_Analysis** - contains scripts for analysizing the behavioral and process-tracing data as well es for Figure 3 - Choice Probabilities and Figure 4 - Effects on Dwell Time and RT. 
- **04_Modeling** - contains scripts for estimating parameters, simulating data based on estimations, performing model comparison and parameter recovery as well as for several figures
    - **bayes_models** - contains text files specifying the models (_bounds refers to a model version in which the attentional parameters theta and phi are bounded between 0 and 1; _nobounds refers to the model verison without bounds)
    - **scripts_figures** - contains scripts for creating the figures 5, 6, and 7
    - **parameter estimation** is performed using two scripts, *parameter_estimation_sim* to simulate data sets based on the ten most likely parameter sets and *parameter_estimation_fit* to fit parameters to the simulated data

Model fitting is performed using [JAGS](https://mcmc-jags.sourceforge.io/). 


---

### Installation

- set up RStudio and JAGS, dowload JAGS Wiener module (see versions and links below)
- **clone the repository** 
- set up the folders "**data**" and "**figures**" within the repository (will be ignored by git, see .gitignore)
- add **DBDA2E-utilities.R** (by John Kruschke) to folder "functions" (download here: https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R; will be ignored by git, see .gitignore)


---

### Common Parameters

#### Built With

- RStudio 2024.09.0
- [JAGS 4.3.2](https://mcmc-jags.sourceforge.io/)
- [JAGS Wiener 4.3.1](https://sourceforge.net/projects/jags-wiener/files/Windows/) 
- Package [runjags](https://cran.r-project.org/web/packages/runjags/index.html)

#### Author

Barbara Oberbauer, barbara.oberbauer@uni-hamburg.de



