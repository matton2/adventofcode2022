# day1 puzzle

library(tidyverse)

ex <- read_lines("day1/ex.txt") |> as.numeric()

input <- read_lines("day1/input.txt") |>  as.numeric()
inputTibble <- tibble(cal = input)


solve1 <- function(data) {
  maxElf <- 0
  maxCount <- 0
  currentElf <- 1
  currentCount <- 0
  for(i in 1:length(data)) {
    if(is.na(data[i])) {
      if(currentCount > maxCount) {
        maxCount <- currentCount
        maxElf <- currentElf
      }
      currentElf <- currentElf + 1
      currentCount <- 0
    } else {
      currentCount <- currentCount + data[i]
      
    }
    
  }
  return(paste("Max Count is:", maxCount))
}

solve1(ex)
solve1(input)

figureOutElfs <- function(data) {
  currentElf <- 1
  currentElfList <- c()
  for(i in 1:length(data)) {
    if(is.na(data[i])) {
      currentElf <- currentElf + 1
    } else {
      currentElfList <- c(currentElfList, currentElf)
    }
  }
  return(currentElfList)
}

figureOutElfs(ex)
inputElfs <- figureOutElfs(input)

answer2 <- inputTibble |> 
  filter(!is.na(cal)) |> 
  bind_cols(inputElfs = inputElfs) |> 
  group_by(inputElfs) |> 
  summarize(totalCal = sum(cal)) |> 
  arrange(totalCal) |> 
  top_n(3) |> 
  pull(totalCal) |> 
  sum()
