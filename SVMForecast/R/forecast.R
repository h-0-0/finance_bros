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

#' Tune
tune_svm <- function(data, formula=NULL, gamma_range=2^(-1:1), C_range=NULL, eps_range=0.1*(1:5)){
  # Create formula for creating bitcoin using all columns in dataframe, as no formula given (as default)
  if(is.null(formula)){
    cols <- colnames(data)
    cols <- cols[ !cols == "BTC_USD"]
    formula <- get_formulas(cols, list(1:length(cols)))
  }
  if(is.null(C_range)){
    C_range <- 2^(0:4)
  }
  # If formula just a string not formula changes to formula
  if(! inherits(formula,"formula")){
    formula <- as.formula(formula)
  }
  ranges <- list(type="eps-regression", kernel="radial", gamma= gamma_range, cost=C_range, epsilon= eps_range)
  tune(svm, formula, data, ranges=ranges, tunecontrol = tune.control(sampling = "fix"))
}

# Fit tuned model
# fit_tune_svm

# Predict
# predict_svm <- function(){
  #predict(model_svm)
#}

#TODO: function that given data and permutations of columns runs fit_svm or tune_svm for all permutations and returns the best
