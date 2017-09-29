#' Step 3: Get prediction breakdown and waterfall chart for a single row of data
#'
#' This function prints the feature impact breakdown for a single data row, and plots an accompanying waterfall chart.
#' @param xgb.model A trained xgboost model
#' @param explainer The output from the buildExplainer function, for this model
#' @param DMatrix The DMatrix in which the row to be predicted is stored
#' @param data.matrix The matrix of data from which the DMatrix was built
#' @param idx The row number of the data to be explained
#' @param type The objective function of the model - either "binary" (for binary:logistic) or "regression" (for reg:linear)
#' @return None
#' @export
#' @import data.table
#' @import xgboost
#' @import waterfalls
#' @import scales
#' @import ggplot2
#' @examples
#' library(xgboost)
#' library(xgboostExplainer)
#'
#' set.seed(123)
#'
#' data(agaricus.train, package='xgboost')
#'
#' X = as.matrix(agaricus.train$data)
#' y = agaricus.train$label
#'
#' train_idx = 1:5000
#'
#' train.data = X[train_idx,]
#' test.data = X[-train_idx,]
#'
#' xgb.train.data <- xgb.DMatrix(train.data, label = y[train_idx])
#' xgb.test.data <- xgb.DMatrix(test.data)
#'
#' param <- list(objective = "binary:logistic")
#' xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=3)
#'
#' col_names = colnames(X)
#'
#' pred.train = predict(xgb.model,X)
#' nodes.train = predict(xgb.model,X,predleaf =TRUE)
#' trees = xgb.model.dt.tree(col_names, model = xgb.model)
#'
#' #### The XGBoost Explainer
#' explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", base_score = 0.5)
#' pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
#'
#' showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  2, type = "binary")
#' showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  8, type = "binary")

showWaterfall = function(xgb.model, explainer, DMatrix, data.matrix, idx, type = "binary"){


  breakdown = explainPredictions(xgb.model, explainer, slice(DMatrix,as.integer(idx)))

  weight = rowSums(breakdown)
  if (type == 'regression'){
    pred = weight
  }else{
    pred = 1/(1+exp(-weight))
  }


  breakdown_summary = as.matrix(breakdown)[1,]
  data_for_label = data.matrix[idx,]

  idx = order(abs(breakdown_summary),decreasing=TRUE)
  breakdown_summary = breakdown_summary[idx]
  data_for_label = data_for_label[idx]

  idx_zero =which(breakdown_summary==0)

  if (length(idx_zero > 0)){
    breakdown_summary = breakdown_summary[-idx_zero]
    data_for_label = data_for_label[-idx_zero]
  }

  intercept = breakdown_summary[names(breakdown_summary)=='intercept']
  data_for_label = data_for_label[names(breakdown_summary)!='intercept']
  breakdown_summary = breakdown_summary[names(breakdown_summary)!='intercept']


  breakdown_summary = c(intercept, breakdown_summary)
  data_for_label = c("", data_for_label)
  labels = paste0(names(breakdown_summary)," = ", data_for_label)
  labels[1] = 'intercept'


  if (!is.null(getinfo(DMatrix,"label"))){
    cat("\nActual: ", getinfo(DMatrix,"label"))
  }
  cat("\nPrediction: ", pred)
  cat("\nWeight: ", weight)
  cat("\nBreakdown")
  cat('\n')
  print(breakdown_summary)

  if (type == 'regression'){

  waterfall(values = round(breakdown_summary,2), labels = labels
            , calc_total = TRUE
            , total_axis_text = "Prediction")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else{

    inverse_logit_trans <- trans_new("inverse logit",
                                     transform = plogis,
                                     inverse = qlogis)

    inverse_logit_labels = function(x){return (1/(1+exp(-x)))}
    logit = function(x){return(log(x/(1-x)))}

    ybreaks<-logit(seq(2,98,2)/100)

    waterfall(values = round(breakdown_summary,2), labels = labels
              , calc_total = TRUE
              , total_axis_text = "Prediction")  + scale_y_continuous(labels = inverse_logit_labels, breaks = ybreaks) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  }
}
