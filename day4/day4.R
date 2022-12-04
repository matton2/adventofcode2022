library(tidyverse)

ex <- read_csv('day4/ex.txt', col_names = c('elf1', 'elf2'))
input <- read_csv('day4/input.txt', col_names = c('elf1', 'elf2'))

containedPair <- function(pair1, pair2) {
  pair1Split <- as.numeric(str_split(pair1, "-")[[1]])
  pair2Split <- as.numeric(str_split(pair2, "-")[[1]])
  fullPair1 <- c(pair1Split[1]:pair1Split[2])
  fullPair2 <- c(pair2Split[1]:pair2Split[2])
  
  if (all(fullPair1 %in% fullPair2)) {
    return (TRUE)
  } else if(all(fullPair2 %in% fullPair1)) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}


exSolve1 <- ex |> 
  rowwise() |> 
  mutate(contained = containedPair(elf1, elf2))
 
sum(exSolve1$contained)

inputSolve1 <- input |> 
  rowwise() |> 
  mutate(contained = containedPair(elf1, elf2))

sum(inputSolve1$contained)


anyMatch <- function(pair1, pair2) {
  pair1Split <- as.numeric(str_split(pair1, "-")[[1]])
  pair2Split <- as.numeric(str_split(pair2, "-")[[1]])
  fullPair1 <- c(pair1Split[1]:pair1Split[2])
  fullPair2 <- c(pair2Split[1]:pair2Split[2])
  
  if (any(fullPair1 %in% fullPair2)) {
    return (TRUE)
  } else if(any(fullPair2 %in% fullPair1)) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

exSolve2 <- ex |> 
  rowwise() |> 
  mutate(contained = anyMatch(elf1, elf2))

sum(exSolve2$contained)

inputSolve2 <- input |> 
  rowwise() |> 
  mutate(contained = anyMatch(elf1, elf2))

sum(inputSolve2$contained)
