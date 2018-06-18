#' Step 1: Build an xgboostExplainer
#'
#' This function outputs an xgboostExplainer (a data table that stores the feature impact breakdown for each leaf of each tree in an xgboost model). It is required as input into the explainPredictions and showWaterfall functions.
#' @param xgb.model A trained xgboost model
#' @param trainingData A DMatrix of data used to train the model
#' @param type The objective function of the model - either "binary" (for binary:logistic) or "regression" (for reg:linear)
#' @param base_score Default 0.5. The base_score variable of the xgboost model.
#' @param trees_idx Default NULL. An integer vector of tree indices that should be parsed. If set to NULL, all trees of the model are parsed.
#' @return The XGBoost Explainer for the model. This is a data table where each row is a leaf of a tree in the xgboost model
#'  and each column is the impact of each feature on the prediction at the leaf.
#'
#'  The leaf and tree columns uniquely identify the node.
#'
#'  The sum of the other columns equals the prediction at the leaf (log-odds if binary response).
#'
#'  The 'intercept' column is identical for all rows and is analogous to the intercept term in a linear / logistic regression.
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


buildExplainer = function(xgb.model, trainingData, type = "binary", base_score = 0.5, trees_idx = NULL){

  col_names = attr(trainingData, ".Dimnames")[[2]]
  cat('\nCreating the trees of the xgboost model...')
  trees = xgb.model.dt.tree(col_names, model = xgb.model, trees = trees_idx)
  cat('\nGetting the leaf nodes for the training set observations...')
  nodes.train = predict(xgb.model,trainingData,predleaf =TRUE)

  cat('\nBuilding the Explainer...')
  cat('\nSTEP 1 of 2')
  tree_list = getStatsForTrees(trees, nodes.train, type = type, base_score = base_score)
  cat('\n\nSTEP 2 of 2')
  explainer = buildExplainerFromTreeList(tree_list,col_names)

  cat('\n\nDONE!\n\n')

  return (explainer)
}
