#' Creates formulas from list
#'
#' Given column names and combinations of columns wanted in model produces vector of strings that can be turned into formulae using as.formula()
#' @param Cols: vector of column names
#' @param ind: list of vectors, where each vector lists combination of columns to create formula from
#' @return vector of strings ready to be passed to as.formula()
#' @export
get_formulas <- function(Cols, ind){
  sapply( ind, function(i) paste("BTC_USD~", paste(Cols[i], collapse="+")) )
}

#' Fit model
#'
#'
#TODO: scale class weights according to amount of data? to handle missing data?
fit_svm <- function(data, formula=NULL , degree=3, gamma=NULL, C=1, eps=0.1, k_cross=0){
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
  e1071::svm(formula, data, type="eps-regression", kernel="radial", degree=degree, gamma= gamma, cost=C, epsilon= eps, cross= k_cross)
}

# Tune
# tune_svm <- function(){}

# Fit tuned model
# fit_tune_svm

# Predict
# predict_svm <- function(){
  #predict(model_svm)
#}
