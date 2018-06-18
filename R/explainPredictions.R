#' Step 2: Get multiple prediction breakdowns from a trained xgboost model
#'
#' This function outputs the feature impact breakdown of a set of predictions made using an xgboost model.
#' @param xgb.model A trained xgboost model
#' @param explainer The output from the buildExplainer function, for this model
#' @param data A DMatrix of data to be explained
#' @return A data table where each row is an observation in the data and each column is the impact of each feature on the prediction.
#'
#' The sum of the row equals the prediction of the xgboost model for this observation (log-odds if binary response).
#'
#' @export
#' @import data.table
#' @import xgboost
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
#' explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", base_score = 0.5, trees = NULL)
#' pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
#'
#' showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  2, type = "binary")
#' showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  8, type = "binary")

explainPredictions = function(xgb.model, explainer ,data){

  #Accepts data table of the breakdown for each leaf of each tree and the node matrix
  #Returns the breakdown for each prediction as a data table

  nodes = predict(xgb.model,data,predleaf =TRUE)

  colnames = names(explainer)[1:(ncol(explainer)-2)]

  preds_breakdown = data.table(matrix(0,nrow = nrow(nodes), ncol = length(colnames)))
  setnames(preds_breakdown, colnames)

  num_trees = ncol(nodes)

  cat('\n\nExtracting the breakdown of each prediction...\n')
  pb <- txtProgressBar(style=3)
  for (x in 1:num_trees){
    nodes_for_tree = nodes[,x]
    tree_breakdown = explainer[tree==x-1]

    preds_breakdown_for_tree = tree_breakdown[match(nodes_for_tree, tree_breakdown$leaf),]
    preds_breakdown = preds_breakdown + preds_breakdown_for_tree[,colnames,with=FALSE]

    setTxtProgressBar(pb, x / num_trees)
  }

  cat('\n\nDONE!\n')

  return (preds_breakdown)

}
