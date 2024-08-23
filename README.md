# MUVR2
**Multivariate methods with Unbiased Variable selection in R**  
PhD candidate Yingxiao Yan <yingxiao@chalmers.se>  
Associate Professor Carl Brunius  <carl.brunius@chalmers.se>  
Department of Life Sciences,
Chalmers University of Technology www.chalmer.se

## General description
The MUVR package allows for predictive multivariate modelling with minimally biased variable selection incorporated into a repeated double cross-validation framework. The MUVR procedure simultaneously produces both minimal-optimal and all-relevant variable selections.

The MUVR2 package is developed with new functionalities based on the MUVR package.

An easy-to-follow tutorial on how to use the MUVR package for classification, regression and multilevel analysis can be found at this repository at [inst/Tutorial/MUVR_Tutorial.docx](https://github.com/MetaboComp/MUVR2/blob/master/inst/Tutorial/MUVR2_tutorial.pdf)

In brief, MUVR2 proved the following functionality:
- Types: Classification, regression and multilevel.
- Model cores: PLS,  Random Forest, Elastic net.
- Validation: Repeated double cross-validaiton (rdCV; Westerhuis et al 2008, Filzmoser et al 2009).
- Variable selection: Recursive feature elimination embedded in the rdCV loop.
- Resampling tests and permutation tests: Assessment of modelling fitnness and overfitting.

## Installation
- You will need to have installed R (https://www.r-project.org/)
- Normally, you will want to work in RStudio (https://rstudio.com/) or some other IDE

You also need to have the `remotes` R package installed. Just run the following from an R script or type it directly at the R console (normally the lower left window in RStudio):
```
install.packages('remotes')
```
When `remotes` is installed, you can install the `MUVR2` package by running:
```
library(remotes)
install_github('MetaboComp/MUVR2')
```

In addition to functions relevant for crossvalidated, predictive multivariate modelling, the MUVR2 package also provides data to accurately reproduce figures from the original *Shi et al* paper (below).

## References
- *Yan Y, Schillemans T, Skantze V, Brunius C. Adjusting for covariates and assessing modeling fitness in machine learning using MUVR2. Bioinformatics Advances. 2024, 4(1), vbae051.*
- *Shi L, Westerhuis JA, Rosén J, Landberg R, Brunius C. Variable selection and validation in multivariate modelling. Bioinformatics. 2019, 35(6), 972–80.*
- *Filzmoser P, Liebmann B, Varmuza K. Repeated double cross validation. Journal of Chemometrics. 2009, 23(4), 160-171.*
- *Westerhuis JA, Hoefsloot HCJ, Smit S, Vis DJ, Smilde AK, Velzen EJJ, Duijnhoven JPM, Dorsten FA. Assessment of PLSDA cross validation. Metabolomics. 2008, 4(1), 81-89.*

