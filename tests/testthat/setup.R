
set.seed(42)

nRep <- 2
nOuter <- 4
varRatio <- 0.6

############# Regression (PLS) ##############

regrModel <- MUVR2(X = XRVIP2,
                   Y = YR2,
                   nRep = nRep,
                   nOuter = nOuter,
                   varRatio = varRatio,
                   method = "PLS",
                   modReturn = TRUE)

################### Classification (Elastic Net) ######################
classModel <- MUVR2_EN(X = Xotu,
                       Y = Yotu,
                       nRep = nRep,
                       nOuter = nOuter,
                       DA = TRUE,
                       modReturn = TRUE)

## Elastic net models only gain their min/mid/max variable selections
## (`nVar`, `Var`, `outlier_info`, `fit_curve`) after getVar(). Plot functions
## need those, so keep a "got" version around for the plotting tests.
classModelVar <- getVar(classModel)

################### Classification (PLS) ######################
classModelPLS <- MUVR2(X = Xotu,
                       Y = Yotu,
                       nRep = nRep,
                       nOuter = nOuter,
                       varRatio = varRatio,
                       method = "PLS",
                       DA = TRUE,
                       modReturn = TRUE)

######### Multilevel (RF) ##########
MLModel <- MUVR2(X = crispEM,
                 ML = TRUE,
                 nRep = nRep,
                 nOuter = nOuter,
                 varRatio = varRatio,
                 method = "RF",
                 modReturn = TRUE)
