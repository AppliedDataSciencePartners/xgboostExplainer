
#' @import data.table
#' @import xgboost

buildExplainerFromTreeList = function(tree_list,col_names){
  
  ####accepts a list of trees and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf

  tree_list_breakdown <- vector("list", length(col_names)  + 3)
  names(tree_list_breakdown) = c(col_names,'intercept', 'leaf','tree')

  num_trees = length(tree_list)
  
  cat('\n\nGetting breakdown for each leaf of each tree...\n')
  pb <- txtProgressBar(style=3)
  
  for (x in 1:num_trees){
    tree = tree_list[[x]]
    tree_breakdown = getTreeBreakdown(tree, col_names)
    tree_breakdown$tree = x - 1
    tree_list_breakdown = rbindlist(append(list(tree_list_breakdown),list(tree_breakdown)))
    setTxtProgressBar(pb, x / num_trees)
  }
  
  return (tree_list_breakdown)
  
}