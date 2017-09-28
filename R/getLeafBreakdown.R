
#' @import data.table
#' @import xgboost
getLeafBreakdown = function(tree,leaf,col_names){
  
  ####accepts a tree, the leaf id to breakdown and column names
  ####outputs a list of the impact of each variable + intercept
  
  impacts = as.list(rep(0,length(col_names)))
  names(impacts) = col_names
  
  path = findPath(tree,leaf)
  reduced_tree = tree[Node %in% path,.(Feature,uplift_weight)]
  
  impacts$intercept=reduced_tree[1,uplift_weight]
  reduced_tree[,uplift_weight:=shift(uplift_weight,type='lead')]
  
  tmp = reduced_tree[,.(sum=sum(uplift_weight)),by=Feature]
  tmp = tmp[-nrow(tmp)]
  impacts[tmp[,Feature]]=tmp[,sum]
  
  return (impacts)
}


