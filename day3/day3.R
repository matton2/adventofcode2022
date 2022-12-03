library(tidyverse)

ex <- read_lines("day3/ex.txt") 
input <- read_lines("day3/input.txt")

value <- tibble(letter = c(letters, LETTERS)) |> 
  mutate(value = row_number())

exTibble1 <- tibble(input = ex) |> 
  mutate(number = str_length(input),
         splitLoc = number/2,
         half1 = str_sub(input, 1, splitLoc),
         half2 = str_sub(input, splitLoc+1, number),
         split1 = str_split(half1, pattern = ""),
         split2 = str_split(half2, pattern = ""),
         same = map2_chr(split1, split2, intersect)
         ) |> 
  left_join(value, by = c('same' = 'letter')) 

sum(exTibble1$value)

inputTibble1 <- tibble(input = input) |> 
  mutate(number = str_length(input),
         splitLoc = number/2,
         half1 = str_sub(input, 1, splitLoc),
         half2 = str_sub(input, splitLoc+1, number),
         split1 = str_split(half1, pattern = ""),
         split2 = str_split(half2, pattern = ""),
         same = map2_chr(split1, split2, intersect)
  ) |> 
  left_join(value, by = c('same' = 'letter')) 

sum(inputTibble1$value)

inputTibble2 <- tibble(input = input) |> 
  mutate(row = row_number(),
         group = ceiling(row/3),
         elf = rep_len(1:3, length(input))) |> 
  select(-row) |> 
  pivot_wider(names_from = elf, values_from = input) |> 
  mutate(elf1 = str_split(`1`, pattern = ""),
         elf2 = str_split(`2`, pattern = ""),
         elf3 = str_split(`3`, pattern = ""),
         same12 = map2(elf1, elf2, intersect),
         same23 = map2(elf2, elf3, intersect),
         same = map2_chr(same12, same23, intersect)) |> 
  left_join(value, by = c('same' = 'letter')) 

sum(inputTibble2$value)
         