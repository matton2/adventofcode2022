
library(tidyverse)

exCrates <- read_lines('day5/ex.txt', n_max = 3)
exDirections <- read_lines('day5/ex.txt', skip = 5)
inputCrates <- read_lines('day5/input.txt', n_max = 8)
inputDirections <- read_lines('day5/input.txt', skip = 10)

orderCrates <- function(crates) {
  
  tempCrates <- list()
  lineNumber <- 0
  number = nchar(crates[1])
  
    for(j in seq(1, number, by = 4)) {
      toAdd <- c()
      lineNumber <- lineNumber + 1
      toAppend <- str_sub(crates, start = j, end = j+2)
      for(k in 1:length(toAppend)) {
        if(toAppend[k] != "   ") {
          toAdd <- c(toAdd, toAppend[k])
        } 
      }
      tempCrates[[lineNumber]] <- toAdd
    }
  
  return(tempCrates)
  
}

exOrdered <- orderCrates(exCrates)
inOrdered <- orderCrates(inputCrates)


sortCrates <- function(crates, dir) {
  
  for(i in 1:length(dir)) {
    split <- str_split(dir[i], " ")
    howMany <- as.numeric(split[[1]][2])
    from <- as.numeric(split[[1]][4])
    to <- as.numeric(split[[1]][6])
    
    # now i need to grab the two to move, move them and then delete them
    
    moving <- crates[[from]][howMany:1]
    # moving to
    crates[[to]] <- c(moving, crates[[to]])
    # moving From
    crates[[from]] <- crates[[from]][-c(1:howMany)]
    
    
  }
  return(crates)

}

debugonce(sortCrates)
exSorted <- sortCrates(exOrdered, exDirections)
inSorted <- sortCrates(inOrdered, inputDirections)

printTopCrate <- function(sorted) {
  top <- c()
  
  for(i in 1:length(sorted)) {
    top <- c(top, sorted[[i]][1])
  }
  
  return(top)
  
}

sortCrates9001 <- function(crates, dir) {
  
  for(i in 1:length(dir)) {
    split <- str_split(dir[i], " ")
    howMany <- as.numeric(split[[1]][2])
    from <- as.numeric(split[[1]][4])
    to <- as.numeric(split[[1]][6])
    
    # now i need to grab the two to move, move them and then delete them
    
    moving <- crates[[from]][1:howMany]
    # moving to
    crates[[to]] <- c(moving, crates[[to]])
    # moving From
    crates[[from]] <- crates[[from]][-c(1:howMany)]
    
    
  }
  return(crates)
  
}

exSorted9001 <- sortCrates9001(exOrdered, exDirections)
inSorted9001 <- sortCrates9001(inOrdered, inputDirections)

printTopCrate(inSorted9001)
