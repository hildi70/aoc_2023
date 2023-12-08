library(stringr)
library(numbers)
input <- readLines("input/day08.txt")

directions <- unlist(strsplit(input[1], ""))

node_now <- unlist(str_extract_all(input, "[A-Z]+(?= )"))
node_left <- unlist(str_extract_all(input, "[A-Z]+(?=,)"))
node_right <- unlist(str_extract_all(input, "(?<=, )[A-Z]+"))


# part 1

steps <- 0
index <- grep("AAA", node_now)
direction_index <- 0

while(node_now[index] != "ZZZ") {
  steps <- steps + 1
  ifelse (direction_index+1 > length(directions),
          direction_index <- 1,
          direction_index <- direction_index + 1
  )
  ifelse (directions[direction_index] == "L",
          index <- grep(node_left[index], node_now),
          index <- grep(node_right[index], node_now)
  )
}

steps # 21409


# part 2

starts <- grep("..A", node_now)
all_steps <- c()

for (i in 1:length(starts)) {
  steps <- 0
  direction_index <- 0
  index <- starts[i]
  
  while(!grepl("..Z", node_now[index])) {
  steps <- steps + 1
  ifelse (direction_index+1 > length(directions),
          direction_index <- 1,
          direction_index <- direction_index + 1
  )
  ifelse (directions[direction_index] == "L",
          index <- grep(node_left[index], node_now),
          index <- grep(node_right[index], node_now)
  )
  }
  
  all_steps[i] <- steps
}

options(scipen = 999)
mLCM(all_steps) #21165830176709