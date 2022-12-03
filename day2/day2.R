library(tidyverse)

ex <- read_lines("day2/ex.txt")

exTibble <- tibble(input = ex) |> 
  separate(input, into=c('p1', 'p2'), sep = " ") |> 
  mutate(p2 = case_when(p2 == "X" ~ "A", p2 == "Y" ~ "B", p2 == "Z" ~ "C" ),
         p1Pts = case_when(p1 == "A" ~ 1, p1 == "B" ~ 2, p1 == "C" ~ 3 ),
         p2Pts = case_when(p2 == "A" ~ 1, p2 == "B" ~ 2, p2 == "C" ~ 3 )) |> 
  rowwise() |> 
  mutate(
         p1Outcome = whoWon(p1Pts, p2Pts),
         p2Outcome = whoWon(p2Pts, p1Pts),
         p1Total = p1Pts + p1Outcome,
         p2Total = p2Pts + p2Outcome
  )

input <- read_lines("day2/input.txt")

inputTibble <- tibble(input = input) |> 
  separate(input, into=c('p1', 'p2'), sep = " ") |> 
  mutate(p2 = case_when(p2 == "X" ~ "A", p2 == "Y" ~ "B", p2 == "Z" ~ "C" ),
         p1Pts = case_when(p1 == "A" ~ 1, p1 == "B" ~ 2, p1 == "C" ~ 3 ),
         p2Pts = case_when(p2 == "A" ~ 1, p2 == "B" ~ 2, p2 == "C" ~ 3 )) |> 
  rowwise() |> 
  mutate(
         p1Outcome = whoWon(p1Pts, p2Pts),
         p2Outcome = whoWon(p2Pts, p1Pts),
         p1Total = p1Pts + p1Outcome,
         p2Total = p2Pts + p2Outcome
         )


whoWon <- function(p1, p2) {
  
  points <- case_when(
    p1 == p2 ~ 3,
    p1 == 1 && p2 == 2 ~ 0,
    p1 == 1 && p2 == 3 ~ 6,
    p1 == 2 && p2 == 1 ~ 6,
    p1 == 2 && p2 == 3 ~ 0,
    p1 == 3 && p2 == 1 ~ 0,
    p1 == 3 && p2 == 2 ~ 6,
  )
  
  return(points)
}

inputTibble$p2Total |> sum()

exTibble2 <- tibble(input = ex) |> 
  separate(input, into=c('p1', 'p2'), sep = " ") |> 
  mutate(p1Pts = case_when(p1 == "A" ~ 1, p1 == "B" ~ 2, p1 == "C" ~ 3 ),
         outcome = case_when(p2 == "X" ~ 0, p2 == "Y" ~ 3, p2 == "Z" ~ 6 )) |> 
  rowwise() |> 
  mutate(
    p2Pts = whatToThrow(p1Pts, outcome),
    p2Total = p2Pts + outcome
  )


inputTibble2 <- tibble(input = input) |> 
  separate(input, into=c('p1', 'p2'), sep = " ") |> 
  mutate(p1Pts = case_when(p1 == "A" ~ 1, p1 == "B" ~ 2, p1 == "C" ~ 3 ),
         outcome = case_when(p2 == "X" ~ 0, p2 == "Y" ~ 3, p2 == "Z" ~ 6 )) |> 
  rowwise() |> 
  mutate(
    p2Pts = whatToThrow(p1Pts, outcome),
    p2Total = p2Pts + outcome
  )

whatToThrow = function(p1, outcome) {
  
  rps <- case_when(
    outcome == 3 ~ p1,
    p1 == 1 && outcome == 6 ~ 2,
    p1 == 1 && outcome == 0 ~ 3 ,
    p1 == 2 && outcome == 0 ~ 1 ,
    p1 == 2 && outcome == 6 ~ 3 ,
    p1 == 3 && outcome == 6 ~ 1,
    p1 == 3 && outcome == 0 ~ 2,
  )
  return (rps)
  
}

inputTibble2$p2Total |> sum()

