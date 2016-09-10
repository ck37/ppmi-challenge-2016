Michael J. Fox Foundation & Parkinson's Progression Marker Initative data science challenge, 2016

## Authors

Chris J. Kennedy, Lina Montoya, Nerissa Nance

## Abstract

While much is known about factors associated with onset of Parkinson’s disease (PD), relatively little is known to predict future disease progression. Rigorously identifying the most important predictors of progression holds the promise of yielding new insights into the etiology of Parkinson’s disease, and may even suggest new treatments that could slow or stop progression.

Our study addresses this goal by directly estimating variable importance and ranking baseline covariates associated with changes in progression, a more accurate method than predicting progression and then ranking influential variables. We leverage targeted learning, a flexible semiparametric estimation framework with no assumptions of functional form, to estimate the importance of each baseline covariate in the dataset with asymptotic inference  and associated p-values (statistically proven to be valid in write-up references), unlike classical approaches such as examining the p-values on OLS coefficients (e.g. with lasso or stepwise selection) or using random forest with covariate permutation. Our variable importance algorithm is implemented in a new R software package, now available publicly on Github for future use in PD research.

We analyzed over 300 baseline covariates and found a total of 45 important factors consistently related to disease progression.  Important baseline variable groups included: depression/anxiety scores, cognitive testing, physical testing, biomarkers, and concurrent medical condition status.  Our findings both support prior literature, and identify new areas of investigation.

Predictors of PD progression are best identified through variable importance estimation methodology with flexible algorithms operating within a targeted learning framework. Our results shed new light on the etiology of PD and can help identify new treatments that may slow disease progression in patients.
	
## Report
	
Available upon request.
