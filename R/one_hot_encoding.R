#' One_hot_encoding
#' Each factor and character variable with n categories(>2) will be transformed to n variables
#' Each factor and character variable with 2 categories will be transformed to one 01 numeric dummy  variable
#' Each factor and character variable with 1 categories will be transformed to one numeric variable that only has value 1
#' Each factor and character variable with 0 categories will be transformed to one numeric variable that only has value -999
#' Each logical variable will be transformed to one 01 numeric dummy variable
#'@param X Data frame. This data frame could contain numeric, factor, character and logical variable
#'@return
#'new_X_matrix:The new matrix that transform all variables to numeric variables
#'@export
#' @examples
#'#To test the scenario when X has factor and charactor when using PLS
#'#add one factor and one character variable(freelive data X,
#'# which originally has 112 numeric samples and 1147 observations)
#'# factor variable has 3,6,5factors(nearzero varianece),character variable has 7,4 categories
#'factor_variable1<-as.factor(c(rep("33",105),rep("44",3),rep("55",4)))
#'factor_variable2<-as.factor(c(rep("AB",20),rep("CD",10),rep("EF",30),
#'                           rep("GH",15),rep("IJ",25),rep("KL",12)))
#'factor_variable3<-as.factor(c(rep("Tessa",25),rep("Olle",30),rep("Yan",12),
#'                            rep("Calle",25),rep("Elisa",20)))
#'factor_variable4<-as.factor(c(rep(NA,112)))
#'character_variable1<-c(rep("one",16),rep("two",16),rep("three",16),
#'                       rep("four",16),rep("five",16),rep("six",16),rep("seven",16))
#'character_variable2<-c(rep("yes",28),rep("no",28),
#'                          rep("yes",28),rep("no",28))
#'character_variable3<-c(rep("Hahahah",112))
#'character_variable4<-as.character(c(rep(NA,112)))
#'logical_variable1<-c(rep(TRUE,16),rep(FALSE,16),rep(TRUE,16),
#'rep(FALSE,16),rep(TRUE,16),rep(FALSE,32))
#'logical_variable2<-c(rep(TRUE,28),rep(FALSE,28),rep(TRUE,28),rep(FALSE,28))
#'
#'  X<-data.frame(row.names<-1:112)
#'  X<-cbind(X,XRVIP,
#'        factor_variable1,factor_variable2,factor_variable3,factor_variable4,
#'        character_variable1,character_variable2,character_variable3,character_variable4,
#'         logical_variable1,logical_variable2)
#'   onehotencoding(X)



onehotencoding <- function(X) {
  if (ncol(as.data.frame(X[, which(sapply(X, class) %in% c('factor', 'character', 'logical'))])) ==
      0)
  {
    message("all ", ncol(X), " variables are numeric.   ", "\n")
    new_X_matrix <- as.matrix(X)
  } else{
    message("This is onehot encoding. All variables are transformed to numeric.   ",
        "\n")
    #find factor and character and logical variable
    X[, which(sapply(X, class) %in% c('factor'))]
    X[, which(sapply(X, class) %in% c('character'))]
    X[, which(sapply(X, class) %in% c('logical'))]
    #store names
    X_factor_names <-
      colnames(X)[which(sapply(X, class) %in% c('factor'))]
    X_character_names <-
      colnames(X)[which(sapply(X, class) %in% c('character'))]
    X_logical_names <-
      colnames(X)[which(sapply(X, class) %in% c('logical'))]
    #######
    #transform character variable into factor variables and store them in a dataframe
    X_character_frame <-
      as.data.frame(X[, which(sapply(X, class) %in% c('character'))])
    if (ncol(X_character_frame) != 0)
    {
      message("There is/are ",
          ncol(X_character_frame),
          " character variable(s).   ",
          "\n")
      for (c in 1:ncol(X_character_frame))
      {
        X_character_frame[, c] <- as.factor(X_character_frame[, c])
      }
    }

    colnames(X_character_frame) <- X_character_names
    ########
    #transform factor variable into numeric and store them in a dataframe
    X_factor_frame <-
      as.data.frame(X[, which(sapply(X, class) %in% c('factor'))])
    if (ncol(X_factor_frame) != 0)
    {
      message("There is/are ",
          ncol(X_factor_frame),
          " factor variable(s).   ",
          "\n")
    }

    ##put factor variables and factor-transformed character variables in to one data frame and give them previous names
    X_factor_frame <-
      cbind(as.data.frame(X[, which(sapply(X, class) %in% c('factor'))]), X_character_frame)
    colnames(X_factor_frame) <- c(X_factor_names, X_character_names)

    ##original numeric frame
    X_numeric_frame <-
      as.data.frame(X[, which(sapply(X, class) %in% c('numeric', "integer"))])
    if (ncol(X_numeric_frame) == 1) {
      colnames(X_numeric_frame) <-
        colnames(X)[which(sapply(X, class) %in% c('numeric', "integer"))]
    }

    ######
    #transform logical variable into numeric and store them in a data frame

    X_logical_frame <-
      as.data.frame(X[, which(sapply(X, class) %in% c('logical'))])
    if (ncol(X_logical_frame) != 0)
    {
      message("There is/are ",
          ncol(X_logical_frame),
          " logical variable(s)",
          "\n")
      for (c in 1:ncol(X_logical_frame))
      {
        for (d in 1:nrow(X_logical_frame)) {
          if (X_logical_frame[d, c] == TRUE) {
            X_logical_frame[d, c] <- 1
          } else{
            X_logical_frame[d, c] <- 0
          }
        }
      }
      colnames(X_logical_frame) <- X_logical_names
      rm(c)
      rm(d)
    }

    if (ncol(X_factor_frame) + ncol(X_logical_frame) == 0)
    {
      message("There are no factor, character and logical variables   ", "\n")

    } else{
      message((ncol(X_factor_frame) + ncol(X_logical_frame)), "non-numeric variable(s)", "\n")
      message(ncol(X_numeric_frame), "numeric variable(s)", "\n")
      ###new factor variables data frame that has factor variable with >2 levels
      X_factor3_name <- character()
      X_factor3_frame <- data.frame(row.names = rownames(X))

      if (ncol(X_factor_frame) > 0) {
        for (n in 1:ncol(X_factor_frame))
        {
          if (length(levels(X_factor_frame[, n])) > 2)
          {
            X_factor3_frame <- cbind(X_factor3_frame, X_factor_frame[, n])
            X_factor3_name <-
              c(X_factor3_name, colnames(X_factor_frame)[n])
          }
        }
      }
      colnames(X_factor3_frame) <- X_factor3_name

      ###new factor variables data frame that has factor variable with =2 levels

      X_factor2_name <- character()
      X_factor2_frame <- data.frame(row.names = rownames(X))
      if (ncol(X_factor_frame) > 0) {
        for (n in 1:ncol(X_factor_frame))
        {
          if (length(levels(X_factor_frame[, n])) == 2)
          {
            X_factor2_frame <- cbind(X_factor2_frame, X_factor_frame[, n])
            X_factor2_name <-
              c(X_factor2_name, colnames(X_factor_frame)[n])
          }
        }
      }
      colnames(X_factor2_frame) <- X_factor2_name
      ####new numeric 01 dummy variables data frame that transformed from data frame that has factor variable with =2 levels
      X_numeric2_frame <- matrix(0L,
                                 nrow = nrow(X_factor2_frame),
                                 ncol = ncol(X_factor2_frame))
      if (ncol(X_factor2_frame) > 0) {
        for (i in 1:ncol(X_factor2_frame))
        {
          for (j in 1:nrow(X_factor2_frame))
          {
            if (X_factor2_frame[j, i] == levels(X_factor2_frame[, i])[1])
            {
              X_numeric2_frame[j, i] <- 0
            } else{
              X_numeric2_frame[j, i] <- 1
            }
          }
        }
      }
      colnames(X_numeric2_frame) <- X_factor2_name

      ###new factor variables data frame that has factor variable with 1 level
      X_factor1_name <- character()
      X_factor1_frame <- data.frame(row.names = rownames(X))

      if (ncol(X_factor_frame) > 0) {
        for (n in 1:ncol(X_factor_frame))
        {
          if (length(levels(X_factor_frame[, n])) == 1)
          {
            X_factor1_frame <- cbind(X_factor1_frame, X_factor_frame[, n])
            X_factor1_name <-
              c(X_factor1_name, colnames(X_factor_frame)[n])
          }
        }
      }
      colnames(X_factor1_frame) <- X_factor1_name
      ####new numeric 01 dummy variables data frame that transformed from data frame that has factor variable with 1 levels
      X_numeric1_frame <-
        matrix(0L,
               nrow = nrow(X_factor1_frame),
               ncol = ncol(X_factor1_frame))
      if (ncol(X_factor1_frame) > 0) {
        for (i in 1:ncol(X_factor1_frame))
        {
          for (j in 1:nrow(X_factor1_frame))
          {
            if (X_factor1_frame[j, i] == levels(X_factor1_frame[, i])[1])
            {
              X_numeric1_frame[j, i] <- 1
            }
          }
        }
      }
      colnames(X_numeric1_frame) <- X_factor1_name

      ###new factor variables data frame that has factor variable with 0 level
      X_factor0_name <- character()
      X_factor0_frame <- data.frame(row.names = rownames(X))
      if (ncol(X_factor2_frame) > 0) {
        for (n in 1:ncol(X_factor_frame))
        {
          if (length(levels(X_factor_frame[, n])) == 0)
          {
            X_factor0_frame <- cbind(X_factor0_frame, X_factor_frame[, n])
            X_factor0_name <-
              c(X_factor0_name, colnames(X_factor_frame)[n])
          }
        }
      }
      colnames(X_factor0_frame) <- X_factor0_name
      ####new numeric 01 dummy variables data frame that transformed from data frame that has factor variable with 0 levels
      X_numeric0_frame <-
        matrix(0L,
               nrow = nrow(X_factor0_frame),
               ncol = ncol(X_factor0_frame))
      if (ncol(X_factor0_frame) > 0) {
        for (i in 1:ncol(X_factor0_frame))
        {
          for (j in 1:nrow(X_factor0_frame)) {
            X_numeric0_frame[j, i] <- -999
          }
        }
      }
      colnames(X_numeric0_frame) <- X_factor0_name
    }

    ###Transform new factor variables data frame that has factor variable with >2 levels into numeric
    ###a is the list of the names of variables to be built
    a <- list()
    if (ncol(X_factor3_frame) > 0) {
      for (n in 1:ncol(X_factor3_frame))
      {
        if (length(levels(X_factor3_frame[, n])) > 5)
        {
          message(colnames(X_factor3_frame)[n],
              "has",
              length(levels(X_factor3_frame[, n])),
              "(>5) levels.   ",
              "\n")
        }
        a[[n]] <- character()

        for (m in 1:length(levels(X_factor3_frame[, n])))
        {
          a[[n]][m] <- paste0(colnames(X_factor3_frame)[n],
                              "_",
                              "level",
                              "_",
                              ##NOT sort the level by alphabet order
                              levels(factor(
                                X_factor3_frame[, n], as.character(unique(X_factor3_frame[, n]))
                              ))[m])
        }

      }
    }
    ##b is the list of 0/1 matrix of each variable
    b <- list()
    c <- list()
    if (ncol(X_factor3_frame) > 0) {
      for (n in 1:ncol(X_factor3_frame))
      {
        b[[n]] <- data.frame(row.names = c(1:nrow(X_factor3_frame)))
        c[[n]] <- data.frame(row.names = c(1:nrow(X_factor3_frame)))

        for (i in 1:length(levels(X_factor3_frame[, n])))
        {
          b[[n]] <- cbind(b[[n]], X_factor3_frame[, n])
          c[[n]] <- cbind(b[[n]], X_factor3_frame[, n])
        }
        c[[n]] <- matrix(
          data = 0,
          nrow = nrow(X_factor3_frame),
          ncol = length(levels(X_factor3_frame[, n]))
        )

        for (m in 1:length(levels(X_factor3_frame[, n])))
        {
          for (z in 1:nrow(X_factor3_frame))
          {
            if (b[[n]][z, m] == as.factor(levels(factor(
              X_factor3_frame[, n], as.character(unique(X_factor3_frame[, n]))
            ))[m]))
            {
              c[[n]][z, m] <- 1
            }
          }
        }

        # a is the list of new name of all the new variables that will be used
        # b is an intermediate step to use to check for error
        # c is the new 0-1 matrix
        # What needs to be done now is to combine a and c
        if (length(c) > 0) {
          rownames(c[[n]]) <- rownames(X)
          colnames(c[[n]]) <- a[[n]]
        }
      }
    }
    #Now I need to combine the c matrix with original X dataset X_numeric_frame
    new_X_frame <- X_numeric_frame
    if (length(c) > 0) {
      for (h in 1:length(c)) {
        new_X_frame <- cbind(new_X_frame, c[[h]])
      }
    }
    new_X_frame <- cbind(
      new_X_frame,
      X_numeric2_frame,
      X_numeric1_frame,
      X_numeric0_frame,
      X_logical_frame
    )
    new_X_matrix <- as.matrix(new_X_frame)
  }
  message(
    "There are originally ",
    ncol(X),
    " variables",
    "\n",
    "They are transformed into ",
    ncol(new_X_matrix),
    " variables by onehotencoding",
    "\n"
  )
  return(new_X_matrix)

}
