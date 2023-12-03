library(tidyverse)
### part1
#read in data
basefolder <- dirname(rstudioapi::getSourceEditorContext()$path)

day1 <- read.csv(paste0(basefolder, "/day1.txt"), header = F) %>% 
  as.data.frame()

#extract all digits, or rather kick out all letters
day1 <- day1 %>% 
  mutate(numbers = str_replace_all(V1, "[:letter:]", ""))

day1$numbers <- as.numeric(day1$numbers)

#find first and last digit
day1 <- day1 %>% 
  mutate(digit1 = as.numeric(str_sub(numbers, 1, 1)))

day1 <- day1 %>% 
  mutate(digit2 = as.numeric(str_sub(numbers, -1, -1)))

#concatenate the 2 digits
day1 <- day1 %>% 
  mutate(twodigitnumber = as.numeric(paste0(digit1, digit2)))

sum(day1$twodigitnumber)

### part2
#this one was quite tricky, I first tried transforming textual numbers into digit numbers
# (e.g. one -> 1) but it lead to problems with cases like "twoone". I found some inspiration on reddit and
# adapted my script to now turn every textual number into its numeric format and keep the first and last letters
#most other steps are analogous to part1
day1$V2 <- day1$V1
day1$V2 <- day1$V2 %>% 
  str_replace_all(c("one" = "o1e",
                    "two" = "t2o",
                    "three" = "thr3e",
                    "four" = "fo4r",
                    "five" = "f5ve",
                    "six" = "s6x",
                    "seven" = "se7en",
                    "eight" = "ei8ht",
                    "nine" = "ni9e"))
day1 <- day1 %>% 
  mutate(numbers2 = str_replace_all(V2, "[:letter:]", ""))

day1 <- day1 %>% 
  mutate(day1part2dig1number = as.numeric(str_sub(numbers2, 1, 1)))

day1 <- day1 %>% 
  mutate(day1part2dig2number = as.numeric(str_sub(numbers2, -1, -1)))

day1 <- day1 %>% 
  mutate(part2twodigitnumber = as.numeric(paste0(day1part2dig1number, day1part2dig2number)))
sum(day1$part2twodigitnumber)

