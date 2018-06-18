
#' @import data.table
#' @import xgboost
getTreeBreakdown = function(tree, col_names){

  ####accepts a tree (data table), and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf

  tree_breakdown <- vector("list", length(col_names)  + 2)
  names(tree_breakdown) = c(col_names,'intercept','leaf')

  leaves = tree[leaf==T, Node]

  for (leaf in leaves){

    leaf_breakdown = getLeafBreakdown(tree,leaf,col_names)
    leaf_breakdown$leaf = leaf
    tree_breakdown = rbindlist(append(list(tree_breakdown),list(leaf_breakdown)))
  }

  return (tree_breakdown)
}
