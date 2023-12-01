library(stringr)
# part 1
input <- read.table(file = "input/day01.txt")

cal_values <- c()
for (i in 1:nrow(input)) {
  temp <- unlist(str_extract_all(input[i,], "[0-9]"))
  cal_values[i] <- as.numeric(paste0(temp[1], temp[length(temp)]))
}

sum(cal_values) #53974

# part 2
library(stringi)

new_input <- stri_replace_all_regex(input[,],
                pattern = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
                replacement = c("one1one", "two2two", "three3three", "four4four", "five5five", "six6six", "seven7seven", "eigth8eigth", "nine9nine"),
                vectorize=FALSE)

cal_values <- c()
for (i in 1:length(new_input)) {
  temp <- unlist(str_extract_all(new_input[i], "[0-9]"))
  cal_values[i] <- as.numeric(paste0(temp[1], temp[length(temp)]))
}

sum(cal_values) #52840
