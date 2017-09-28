
#' @import data.table
#' @import xgboost
findLeaves = function(tree, currentnode){
  
  if (tree[currentnode,'Feature']=='Leaf'){
    leaves = currentnode
  }else{
    leftnode = tree[currentnode,Yes]
    rightnode = tree[currentnode,No]
    leaves = c(findLeaves(tree,'leftnode',with=FALSE),findLeaves(tree,'rightnode',with=FALSE))
  }
  
  return (sort(leaves))
  
  
}