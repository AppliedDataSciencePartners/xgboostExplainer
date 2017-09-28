
#' @import data.table
#' @import xgboost
getStatsForTrees = function(trees, nodes.train, type = "binary", base_score = 0.5){
  #Accepts data table of tree (the output of xgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in
  
  tree_list = copy(trees)
  tree_list[,leaf := Feature == 'Leaf']
  tree_list[,H:=Cover]
  
  non.leaves = which(tree_list[,leaf]==F)

  
  # The default cover (H) seems to lose precision so this loop recalculates it for each node of each tree
  cat('\n\nRecalculating the cover for each non-leaf... \n')
  pb <- txtProgressBar(style=3)
  j = 0
  for (i in rev(non.leaves)){
    left = tree_list[i,Yes]
    right = tree_list[i,No]
    tree_list[i,H:=tree_list[ID==left,H] + tree_list[ID==right,H]]
    j=j+1
    setTxtProgressBar(pb, j / length(non.leaves))
  }
  

  if (type == 'regression'){
    base_weight = base_score
  } else{
    base_weight = log(base_score / (1-base_score))
  }
  
  tree_list[leaf==T,weight:=base_weight + Quality]
  
  tree_list[,previous_weight:=base_weight]
  tree_list[1,previous_weight:=0]
  
  tree_list[leaf==T,G:=-weight*H]
  
  tree_list = split(tree_list,as.factor(tree_list$Tree))
  num_tree_list = length(tree_list)
  treenums =  as.character(0:(num_tree_list-1))
  t = 0
  cat('\n\nFinding the stats for the xgboost trees...\n')
  pb <- txtProgressBar(style=3)
  for (tree in tree_list){
    t=t+1
    num_nodes = nrow(tree)
    non_leaf_rows = rev(which(tree[,leaf]==F))
    for (r in non_leaf_rows){
        left = tree[r,Yes]
        right = tree[r,No]
        leftG = tree[ID==left,G]
        rightG = tree[ID==right,G]
        
        tree[r,G:=leftG+rightG]
        w=tree[r,-G/H]
        
        tree[r,weight:=w]
        tree[ID==left,previous_weight:=w]
        tree[ID==right,previous_weight:=w]
    }
    
    tree[,uplift_weight:=weight-previous_weight]
    setTxtProgressBar(pb, t / num_tree_list)
  }
  
  return (tree_list)
}