library(tidyverse)

ex <- read_lines('day6/ex.txt')
input <- read_lines('day6/input.txt')

findSome <- function(string, num) {
  numChar <- NA
  for(i in 1:nchar(string)) {
    curr <- str_sub(string, start = i, end = i+num)
    dups <- any(duplicated(str_split(curr, pattern = "")[[1]]))
    if(dups == FALSE) {
      numChar <- i+num
      break
    }
  }
  return (numChar)
}

findSome(ex[1], 3)
findSome(ex[1], 13)
findSome(ex[2], 3)
findSome(ex[3], 3)
findSome(ex[4], 3)
findSome(ex[5], 3)
findSome(input, 3)
findSome(input, 13)
