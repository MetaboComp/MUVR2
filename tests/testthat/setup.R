
nRep <- 2
nOuter <- 4
varRatio <-0.6

############# Regression ##############

regrModel <- MUVR2(X = XRVIP2,
                   Y = YR2,
                   nRep = nRep,
                   nOuter = nOuter,
                   varRatio = varRatio,
                   method = "PLS",
                   modReturn = TRUE)

################### Classification ######################
classModel <- MUVR2_EN(X = Xotu,
                       Y = Yotu,
                       nRep = nRep,
                       nOuter = nOuter,
                       DA = TRUE,
                       modReturn = TRUE)

######### Multilevel ##########
MLModel <- MUVR2(X = crispEM,
                 ML = TRUE,
                 nRep = nRep,
                 nOuter = nOuter,
                 varRatio = varRatio,
                 method = "RF",
                 modReturn = TRUE)

