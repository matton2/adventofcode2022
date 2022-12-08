# day8

library(tidyverse)
library(adventdrob)

ex <- tibble(x = read_lines("day8/ex.txt"))
input <- tibble(x = read_lines('day8/input.txt'))

exGrid <- adventdrob::grid_tidy(ex, x)
inputGrid <- grid_tidy(input, x)

treeSearch <- function(tidyGrid) {
  numTrees <- 0
  totalRows <- max(tidyGrid$row)
  totalCols <- max(tidyGrid$col)
  from <- 2
  #start row position
  for(i in seq(from = from, to = totalRows-1, by = 1)) {
    #and then column position
    rowValues <- tidyGrid |> filter(row == i) |> pull(value)
    for(j in seq(from = from, to = totalCols-1, by = 1)) {
    # i need to look in all four directions to see if this tree is the tallest
      currentValue <- rowValues[j]
      iAmTall <- FALSE
      #pull out left
      left <- tidyGrid |> filter(row == i & col < j) |> pull(value)
      if(all(currentValue>left)) {
        iAmTall <- TRUE
      }
      right <- tidyGrid |> filter(row == i & col > j) |> pull(value)
    #pull out top
      if(all(currentValue>right)) {
        iAmTall <- TRUE
      }
      top <- tidyGrid |> filter(row < i & col == j) |> pull(value)
      #pull out below
      if(all(currentValue>top)) {
        iAmTall <- TRUE
      }
      below <- tidyGrid |> filter(row > i & col == j) |> pull(value)
      if(all(currentValue>below)) {
        iAmTall <- TRUE
      }
      if(iAmTall) {
        numTrees <- numTrees + 1
      }
    }
  }
  
  numTrees <- numTrees + (2*totalRows) + (2*totalCols) - 4
  
  return(numTrees)
  
  
}

debugonce(treeSearch)
treeSearch(exGrid)
treeSearch(inputGrid)

checkFunction <- function(value, test) {
  score <- 0
  for(j in 1:length(test)){
    if(test[j] < value) {
      score <- score + 1
    } else  if (test[j] >= value) {
      score <- score+1
      break
    } else {
      break
    }
  }
  return(score)
}

treeSearch2 <- function(tidyGrid) {
  for(i in 1:NROW(tidyGrid)) {
    # i need to check each position, look for the next tree that is the same size of taller or an edge and do some math
    currentValue <- tidyGrid$value[i]
    currentRow <- tidyGrid$row[i]
    currentCol <- tidyGrid$col[i]
    
    if(currentCol == 1 | currentRow == 1) {
      tidyGrid$scenicScore[i] = 0
    } else if(currentCol == max(tidyGrid$col) | currentRow == max(tidyGrid$row)) {
      tidyGrid$scenicScore[i] = 0
    } else {
      
      left <- tidyGrid |> 
        filter(row == currentRow & col < currentCol) |> 
        pull(value) |> 
        rev() 
      leftScore <- checkFunction(currentValue, left)

      right <- tidyGrid |> filter(row == currentRow & col > currentCol) |> pull(value)
      rightScore <- checkFunction(currentValue, right)
      top <- tidyGrid |> filter(row < currentRow & col == currentCol) |> pull(value) |> rev()
      topScore <- checkFunction(currentValue, top)
      bottom <- tidyGrid |> filter(row > currentRow & col == currentCol) |> pull(value)
      bottomScore <- checkFunction(currentValue, bottom)
      
      
      tidyGrid$scenicScore[i] = leftScore*rightScore*topScore*bottomScore
      
    }
    
  }
  return(tidyGrid)
  
}
debugonce(treeSearch2)
temp <- treeSearch2(exGrid)
in2 <- treeSearch2(inputGrid)
