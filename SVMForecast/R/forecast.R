#' Creates formulas from list
#'
#' Given column names and combinations of columns wanted in model produces vector of strings that can be turned into formulae using as.formula()
#' @param Cols vector of column names
#' @param ind list of vectors, where each vector lists combination of columns to create formula from
#' @return vector of strings ready to be passed to as.formula()
#' @export
get_formulas <- function(Cols, ind){
  sapply( ind, function(i) paste("BTC_USD~", paste(Cols[i], collapse="+")) )
}

#' Fit model
#'
#' Uses svm() from the e1071 package to fit a SVM with our specific desired parameters.
#'@param data data.frame containing training data you want to use to fit an SVM
#'
#'@param formula symbolic description of model to be fit, either as a formula object or as a string, default BTC_USD~ sum of all other columns
#'@param gamma parameter needed for radial basis kernel, default 1/(number of features in model)
#'@param C cost of constraints violation (C constant in regularization term in Lagrange formulation), default 1
#'@param eps epsilon in the insensitive loss function
#'@param k_cross integer >0, defines number of folds with which to perform k-fold cross validation, default is 0 which means it doesn't carry out cross validation
#'
#'@return the result of using e1071::svm(), an object of class "svm"
#'@export
#'@importFrom e1071 svm
#TODO: scale class weights according to amount of data? to handle missing data?
fit_svm <- function(data, formula=NULL , gamma=NULL, C=1, eps=0.1, k_cross=0){
  # Create formula for creating bitcoin using all columns in dataframe, as no formula given (as default)
  if(is.null(formula)){
    cols <- colnames(data)
    cols <- cols[ !cols == "BTC_USD"]
    formula <- get_formulas(cols, list(1:length(cols)))
  }
  # Set gamma to 1/(data dimension) as default
  if(is.null(gamma)){
    dim <- length(attr(terms(as.formula(formula)), "term.labels"))
    gamma <- 1/dim
  }
  # If formula just a string not formula changes to formula
  if(! inherits(formula,"formula")){
    formula <- as.formula(formula)
  }

  # Fit and return our SVM
  e1071::svm(formula, data, type="eps-regression", kernel="radial", gamma= gamma, cost=C, epsilon= eps, cross= k_cross)
}

#' Tuned SVM
#'
#' Performs grid search on hyperparameters for SVM using cross validation to measure performance, where the SVM is based on data and formula given, and returns results. Is basically a wrapper for tune.svm() from the e1071 package
#' @param data data.frame containing training data you want to use to fit an SVM
#' @param formula symbolic description of model to be fit, either as a formula object or as a string, default BTC_USD~ sum of all other columns
#' @param gamma_range range of parameters to perfrom grid search on for gamma hyperparameter needed for radial basis kernel, default is 2^(-1:1)
#' @param C_range range of costs of constraints violation (C constant in regularization term in Lagrange formulation) to perform grid search on, default is 2^(0:4)
#' @param eps_range range of epsilons used in the insensitive loss function to perfrom grid search on, default is 0.1*(0:5)
#' @return the result of using e1071::tune.svm(), a tuning object including the best parameter set
#' @export
#' @importFrom e1071 tune.svm
tune_svm <- function(data, formula=NULL, gamma_range=2^(-3:0), C_range=2^(0:4), eps_range=0.01*(0:5)){
  # Create formula for creating bitcoin using all columns in dataframe, as no formula given (as default)
  if(is.null(formula)){
    cols <- colnames(data)
    cols <- cols[ !cols == "BTC_USD"]
    formula <- get_formulas(cols, list(1:length(cols)))
  }
  # If formula just a string not formula changes to formula
  if(! inherits(formula,"formula")){
    formula <- as.formula(formula)
  }

  tune.svm(formula, data = data,  sampling = "cross", type = "eps-regression", kernel = "radial", gamma= gamma_range, cost=C_range, epsilon= eps_range)
}

#' Prediction
#'
#' A wrapper for the prediction() function from the e1071 package. Calculates prediction then returns it in our desired format (a data.frame)
#' @param model result from fit_svm, ie. object of class "svm"
#' @param predictors data frame containing data points for which we want predictions
#' @param long Boolean indicating if we want to pass the data frame to long_format before returning
#' @return Data frame containing predictions
#' @export
#' @import e1071
predict_svm <- function(model, predictors, long = TRUE){
  out <- predict(model, predictors)
  out <- data.frame("BTC_USD" =out)
  if(long){
    out <- long_format(out)
  }
  out
}


#' Training and Testing data
#'
#' Assigns ranges of row indices for multiple testing and training sets based on arguments given. Note that this simply partitions the datset so all the indices are sequential.
#' @param data a data frame which you want to create test and training indices for
#' @param testing the number of datapoints you want to reserve for testing for each testing, training pair
#' @param n_tt the number of testing and training sets you want to produce
#' @return a list of testing and training sets, where each element is a list containing a test and train list with the testing and training row indices respectfully
#' @export
tt_ranges <- function(data, testing, n_tt){
  sections <- split(1:nrow(data), sort(rep_len(1:n_tt, nrow(data))))
  ranges <- list()
  for (i in 1:n_tt) {
    inds <- sections[[i]]
    ranges[[i]] <- list(train = inds[1:(length(inds)-testing)], test = inds[(length(inds)-testing +1) : length(inds)] )
  }
  ranges
}

#' Fit multiple SVM's
#'
#' Given data and a list of training and testing indices (from tt_ranges()) and a tuning object (from tune_svm()) will fit a SVM model to each testing set and return the resulting list of outputs
#' @param data a data frame containing the data
#' @param tt_inds a vector of lists containing test and train indices (from tt_ranges())
#' @param tuning a tuning object (from tune_svm())
#' @param k_cross number of folds for cross validation, default is 10
#' @return a list of objects of class svm from fit_svm each trained on a portion of the training data
#' @export
fit_multi <- function(data, tt_inds, tuning, k_cross=10){
  out <- vector("list", length(tt_inds))
  for(i in 1:length(tt_inds)){
    out[[i]] <- fit_svm(data[tt_inds[[i]]$train,] , gamma= tuning$best.parameters$gamma, C=tuning$best.parameters$cost, eps=tuning$best.parameters$epsilon, k_cross=k_cross)
    print(out[[i]])
  }
  out
}


#' Predict using multiple SVM's
#'
#' Given data and a list of training and testing indices (from tt_ranges()) and an svm object (from fit_svm()) will return predictions for testing data using their respective svm object
#' @param data a data frame containing the data
#' @param tt_inds a vector of lists containing test and train indices (from tt_ranges())
#' @return a list of objects of class svm from fit_svm each trained on a portion of the training data
#' @export
pred_multi <- function(data, tt_inds, svms){
  out <- list()
  for(i in 1:length(tt_inds)){
    out[[i]] <- predict_svm(svms[[i]], data[tt_inds[[i]]$test,])
  }
  out
}
