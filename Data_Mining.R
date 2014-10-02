# Utrecht Universiteit, Data Mining
# Marinus Burger
# Joseph Eremondi
# Assignment 1
# October 10, 2014

#debug
#arg : any type
#return : NULL
#prints a, if the function body is not commented out. Useful for turning debugging on and off.
debug = function(arg)
{
  #print(arg)
}


#=====================================================

#Data format for Decision Trees

# We use lists to store trees
# Leaf nodes are singleton lists, of the format [MajorityClass]

# Interior nodes are of the form [isNumeric, column, splitPoint, trueNode, falseNode]
# The first element tells us whether we are splitting on numeric or category data
# We then look at which column we split on
# If numeric, we drop to true node if the current value in the given column is less than or equal to splitPoint
# and drop to the falseNode otherwise
# If not numeric, we drop to the trueNode if the given attribute is 1, falseNode if it is 0

#============================================================================================

# Helper functions for nodes: Constructors

# makeLeaf
# class : {0,1}, the majority class of a data set
# return : a leaf node which classifies all data to the majority class
# Basic constructor for leaf nodes
makeLeaf = function(class){
  return(class)
}

# makeNumericSplit
# column : Int, the column index of the attribute which this node splits on
# splitPoint : Int, the number we compare to for classifying a value
# trueNode : Tree, the sub-tree which we use to classify values less than the given attribute
# falseNode : Tree, same as above but for larger values
# return : Tree, a new tree classfying values based on the given column attribute
# Basic constructor for numeric split nodes
makeNumericSplit = function(column, splitPoint, trueNode, falseNode){
  return(list(TRUE, column, splitPoint,
              trueNode, falseNode))
}

# makeCategorySplit
# column : Int, the column index of the attribute which this node splits on
# trueNode : Tree, the sub-tree which we use to values with 0 as the given attribute
# falseNode : Tree, same as above for 1-valued
# return : Tree, a new tree classfying values based on the given column attribute
# Basic constructor for categorical split nodes
makeCategorySplit = function (column, trueNode, falseNode){
  return(
    #We use -9999 as a placeholder, since there is no split number
    list(FALSE, column, -9999,
         trueNode, falseNode)
  )
}

# isLeaf
# node : Tree, the node to test
# return : Bool, TRUE if the tree is a single node, FALSE otherwise
isLeaf <- function(node){
  return (class(node) == "numeric")
}

# isNumeric
# node : Tree, the node to test, assumed to be non-leaf
# return : Bool, TRUE if the node classifies on a numeric value, false otherwise
isNumeric = function(node){
  return( node[[1]] )
}

# nodeColumn
# node : Tree, assumed to be non-leaf
# return : Int, the column index of the attribute this node splits on
nodeColumn = function(node)
{
  node[[2]]
}

# nodeSplitPoint
# node : Tree, assumed to be non-leaf and numeric
# return : Real, the numeric value which the node compares data values to
nodeSplitPoint = function(node)
{
  node[[3]]
}

# leftChild
# node : Tree, assumed to be non-leaf
# return : Tree, the tree which this node uses to classify <split or 0 cases
leftChild = function(node)
{
  node[[4]]
}

# rightChild
# node : Tree, assumed to be non-leaf
# return : Tree, the tree which this node uses to classify >=split or 1 cases
rightChild = function(node)
{
  node[[5]]
}


# impurity
# y : [{0,1}], a vector of class values
# return : Real, the impurity y using the GINI metric

impurity = function(y) {
  # Since y is a list of 0's and 1's, adding all elements in y is
  # the same as counting the number of 1's in y
  if (length(y) > 0)
  {
    #TODO check this
    p1 <- sum(y) / length(y)
  }
  else
  {
    p1 <- sum(y)
  }
  
  return (p1 * (1-p1))
}

# bestNumericSplit
# x : [Real], a list of numeric values
# y: [{0,1}], a list of classes for each entry in x
# minleaf : the minimum number of data points that must be in each part of a split
# return: (Real, Real)
# The first value returned is the numeric value for the valid split on x
# with greatest impurity reduction
# The second is the value of the impurity reduction
bestNumericSplit <- function(x,y, minleaf) {
  n <- length(y)
  index <- (1:(n-1) )  
  
  
  xsorted <- sort(unique(x))
  
  if (length(xsorted) == 1)
  {
    #If only one element, no improvement from split
    return (c(xsorted[1], 0))
  }
  
  numUnique <- length(xsorted)
  
  xFirsts <- xsorted[1:(numUnique-1)]
  
  xLasts <- xsorted[2:numUnique]
  
  splitPoints <- 0.5*(xFirsts + xLasts)
  
  rootImp <- impurity(y)
  
  leftImps <- -9000
  p0 <- -9000
  rightImps <- -9000
  p1 <- -9000
  
  
  for (i in 1:(numUnique - 1)){
    leftImps[i] <- impurity(y[x <= splitPoints[i]])
    rightImps[i] <- impurity(y[x > splitPoints[i]])
    p0[i] <- length(y[x <= splitPoints[i]]) / length(y)
    p1[i] = 1 - p0[i]
  }
  
  
  
  #p0 = length(y[x <= splitPoints[i]])/n
  #p1 = length(y[x > splitPoints[i]])/n
  
  diffs <- rootImp - (p0*leftImps + p1*rightImps)
  
  #Set diff to 0 for ones lower than minLeaf
  for (i in 1:1:(numUnique - 1))
  {
    split <- splitPoints[i]
    if (length(x[x < split]) < minleaf | length(x[x >= split]) < minleaf )
    {
      diffs[i] <- 0
    }
  }
  
  
  debug("Finding best split")
  debug(x)
  debug(xFirsts)
  debug(xLasts)
  debug(splitPoints)
  debug(paste("Diffs", diffs))
  #find index of diff with max element
  resultIndex = which.max(diffs)
  debug(paste("Result index", resultIndex))
  
  #return both the split, and the delta diff, for comparing with other attrs
  return (c(splitPoints[resultIndex], diffs[resultIndex]))
  
}

# binarySplitDelta
# x : [{0,1}], A vector of input categorical values
# y : [{0,1}], Class values for each x entry
# return : Real, the GINI impurity reduction from splitting based on x values
binarySplitDelta <- function(x,y) {
  
  n <- length(y)
  
  
  leftImp <- impurity(y[x == 0])
  rightImp <- impurity(y[x == 1])
  
  rootImp <- impurity(y)
  
  
  p0 = length(y[x == 0])/n
  p1 = length(y[x == 1])/n
  
  
  
  diff <- rootImp - (p0*leftImp + p1*rightImp)
  
  
  
  #return both the split, and the delta diff, for comparing with other attrs
  return (diff)
  
}

# tree.grow
# x : Matrix, input numeric and categorical data
# y : [{0,1}], class values for each row of x
# nmin : the minimum number of values in any non-leaf node in the result tree
# minleaf : the minimum values in any leaf node in the result tree
# return : Tree
# Given a training data set x and y, and parameters to avoid overfitting
# Return a classification tree trained on that data set
tree.grow <- function(x,y,nmin,minleaf)
{
  
  #number of columns
  numAttributes <- (length(x))
  
  n <- length(x[,1])
  
  #Find out if each column is numeric
  isNumeric <- c(FALSE)
  
  rowNumbers <- (1:n)
  
  for (i in 1:numAttributes)
  {
    isNumeric[i] <- FALSE
    #TODO more R-like way?
    
    for (j in (1:n))
    {
      if ( (x[j,i] != 0) & (x[j,i] != 1) )
      {
        isNumeric[i] <- TRUE
      }
    }
    #Find the index of each element not zero or 1
    #nonZeroOneElements = rowNumbers[x[i, rowNumbers] != 0 || x[i, rowNumbers] != 1]
    #If we found at least one, then this is numeric
    #debug(nonZeroOneElements)
    #if(length(nonZeroOneElements) > 0){
    #  isNumeric[i] <- TRUE
    #}
    
  }
  
  
  tree.growHelper(x,y,nmin, minleaf, numAttributes, isNumeric)
}

# tree.grow
# x : Matrix, input numeric and categorical data
# y : [{0,1}], class values for each row of x
# nmin : the minimum number of values in any non-leaf node in the result tree
# minleaf : the minimum values in any leaf node in the result tree
# return : Tree
# Recursive function to grow a decision tree from a given data-set
# Does the real work for growing a tree
# But is separated from tree.grow so that we can get some data from pre-processing
tree.growHelper <- function (x, y, nmin, minleaf, numAttributes, isNumeric) 
{
  debug("** In Tree grow helper **")
  #number of data points
  #debug("Initial data")
  #debug(x)
  #debug(y)
  
  n <- length(x[,1])
  
  
  
  if (length(y) != n)
  {
    #Impossible
  }
  
  debug("Finding majority class")
  debug(x)
  debug(y)
  
  #Find the majority class
  majorityClass <- 0
  #Check if more than half are 1's
  if (sum(y) >= (length(y) / 2.0))
  {
    majorityClass <- 1
  }
  
  
  #If number of unique in y is 1, then we automatically make a leaf
  #Because we're at maximum purity
  if (length(x[,1]) < nmin | length(unique(y)) == 1)
  {
    
    #Return a new leaf that classifies everything to the majority class
    #print("Cutting off for nmin")
    #print(length(x))
    return (makeLeaf(majorityClass)) #leaf
  }
  else
  {
    #Find best split where each child has at least minleaf data points
    #Then recursively grow each side of the split
    bestColumnForSplit <- 0
    bestSplitPoint <- -9999
    bestSplitDelta <- -9999
    
    #Go through each attribute, finding inpurity gain if we divide on this attribute
    for (i in 1:numAttributes)
    {
      if (isNumeric[i])
      {
        result <- bestNumericSplit(x[,i], y, minleaf)
        splitPoint <- result[1]
        deltaDiff <- result[2]
        
        debug(paste("Column ", i))
        debug (paste("Best Split Delta ", bestSplitDelta))
        debug(paste("Delta ", deltaDiff))
        
        
        if (bestSplitDelta < deltaDiff)
        {
          bestColumnForSplit <- i
          bestSplitPoint <- splitPoint
          bestSplitDelta <- deltaDiff
        }
      }
      else
      {
        deltaDiff <- binarySplitDelta(x[,i], y)
        
        debug(paste("Column ", i))
        debug(paste("Delta ", deltaDiff))
        
        numOnLeft <- length( x[x[bestColumnForSplit] == 0,] )
        numOnRight <- length( x[x[bestColumnForSplit] == 1,] )
        
        if (bestSplitDelta < deltaDiff & numOnLeft >= minleaf & numOnRight >= minLeaf)
        {
          bestColumnForSplit <- i
          bestSplitDelta <- deltaDiff
        }
        
      }
      
    }
    
    debug("Found best column for split:")
    debug(bestColumnForSplit)
    
    #Now that we have the best split, we divide our data
    if (isNumeric[bestColumnForSplit])
    {
      xLeft <- x[x[bestColumnForSplit] <= bestSplitPoint,]
      yLeft <- y[x[bestColumnForSplit] <= bestSplitPoint]
      xRight <- x[x[bestColumnForSplit] > bestSplitPoint,]
      yRight <- y[x[bestColumnForSplit] > bestSplitPoint]
      
      #debug("xLeft")
      #debug(length(xLeft[,1]))
      #debug(xLeft)
      
      #debug("xRight")
      #debug(length(xRight[,1]))
      #debug(xRight)
      
      
      #Recursively build the trees for each data set
      
      if ( (length(xLeft[,1]) == 0) || (length(xRight[,1]) == 0) )
      {
        debug("Making leaf 1, length 0")
        return (makeLeaf(majorityClass))
      }
      
      leftTree <- tree.growHelper(xLeft, yLeft, nmin, minleaf, numAttributes, isNumeric)
      rightTree <- tree.growHelper(xRight, yRight, nmin, minleaf, numAttributes, isNumeric)
      
      return(
        makeNumericSplit(bestColumnForSplit, bestSplitPoint, leftTree, rightTree)
      )
    }
    else
    {
      xLeft <- x[x[bestColumnForSplit] == 0,]
      yLeft <- y[x[bestColumnForSplit] == 0]
      xRight <- x[x[bestColumnForSplit] == 1,]
      yRight <- y[x[bestColumnForSplit] == 1]
      
      #debug("xLeft")
      #debug(length(xLeft[,1]))
      #debug(xLeft)
      
      #debug("xRight")
      #debug(length(xRight[,1]))
      #debug(xRight)
      
      
      if ((length(xLeft[,1]) == 0) || (length(xRight[,1]) == 0))
      {
        #debug("Making leaf, length 0")
        return (makeLeaf(majorityClass))
      }
      
      leftTree <- tree.growHelper(xLeft, yLeft, nmin, minleaf, numAttributes, isNumeric)
      rightTree <- tree.growHelper(xRight, yRight, nmin, minleaf, numAttributes, isNumeric)
      
      return(
        makeCategorySplit(bestColumnForSplit, leftTree, rightTree)
      )
    }
    
    
    
  }
}
# tree.classifyRow
# xRow : [Numeric | {0,1}], a row of input data
# tr : Tree, a decision tree generated by tree.grow
# return : {0,1}, the class assigned to rowX by our decision tree tr
# Assumes tr was trained using data of the same format as xRow
tree.classifyRow <- function(xRow, tr)
{
  if (isLeaf(tr))
  {
    return(tr)
  }
  else
  {
    dataToLookAt = xRow[nodeColumn(tr)]
    if (isNumeric(tr))
    {
      if (dataToLookAt < nodeSplitPoint(tr))
      {
        return (tree.classify(xRow, leftChild(tr)))
      }
      else
      {
        return (tree.classify(xRow, rightChild(tr)))
      }
    }
    else
    {
      if (dataToLookAt == 0)
      {
        return (tree.classify(xRow, leftChild(tr)))
      }
      else
      {
        return (tree.classify(xRow, rightChild(tr)))
      }
    }
  }
}

# tree.classify
# x : Matrix, a table of input data
# tr : Tree, a decision tree generated by tree.grow
# return : [{0,1}], vector the classes assigned to each row of x by our decision tree tr
# Assumes tr was trained using data of the same format as x
tree.classify <- function(x, tr)
{
  ret = c(0)
  for (i in 1:length(x[,1]))
  {
    ret[i] <- tree.classifyRow(x[i,], tr)
  }
  return(ret)
}

# printTreeHelper
# tree : Tree, a node to print
# colNames : [String], a vector of names for the columns of data our tree was trained on
# level : the depth of this node in the tree
# Helper function for recursively pretty-printing a tree
printTreeHelper <- function(tree, colNames, level)
{
  spacer <- ""
  if (level > 0)
  {
    for (i in 0:level)
    {
      spacer <- paste(spacer, "    ")
    }
  }
  if (isLeaf(tree))
  {
    print(paste(spacer, "Leaf", tree))
  }
  else
  {
    stringToPrint <- ""
    
    if (isNumeric(tree))
    {
      stringToPrint <- paste(spacer,  colNames[tree[[2]]], "<", tree[[3]])
    }
    else
    {
      stringToPrint <- paste(spacer, colNames[tree[[2]]], "?")
    }
    
    print(stringToPrint)
    printTreeHelper(tree[[4]], colNames, level + 1)
    printTreeHelper(tree[[5]], colNames, level + 1)
  }
}

# printTreeHelper
# tree : Tree, a node to print
# colNames : [String], a vector of names for the columns of data our tree was trained on
# Pretty-print a tree generated by tree.grow
# very useful for debugging
printTree <- function(tree, colNames)
{
  printTreeHelper(tree, colNames, 0)
  
}
