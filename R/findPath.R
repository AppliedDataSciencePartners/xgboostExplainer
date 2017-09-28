
#' @import data.table
#' @import xgboost
findPath = function(tree, currentnode, path = c()){
  
  #accepts a tree data table, and the node to reach
  #path is used in the recursive function - do not set this
  
  while(currentnode>0){
    path = c(path,currentnode)
    currentlabel = tree[Node==currentnode,ID]
    currentnode = c(tree[Yes==currentlabel,Node],tree[No==currentlabel,Node])
  }
  return (sort(c(path,0)))
  
}

