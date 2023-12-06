library(stringr)
library(dplyr)

input <- readLines("input/day05.txt")
input <- input[nchar(input)!=0]

# part 1

seeds <- as.numeric(unlist(str_extract_all(input[grepl("seeds", input)], "[0-9]+")))

map_names <- unlist(str_extract_all(input, ".+(?=:)"))[-1]
map_index <- c(which(str_detect(input, ".+(?=:)") == 1)[-1], length(input) + 1)


map <- list()
for (i in seq_along(map_names)) {
  map[[i]] <- input[(map_index[i]+1):(map_index[i+1]-1)]
}


for (i in seq_along(map)) {
  range_length <- as.numeric(unlist(str_extract_all(map[[i]], "(?<= )[0-9]+$")))
  
  destination <- as.numeric(unlist(str_extract_all(map[[i]], "^[0-9]+(?= )")))
  
  source_start <-  as.numeric(unlist(str_extract_all(map[[i]], "(?<= )[0-9]+(?= )")))
  
  for (j in seq_along(seeds)) {
    
    range_length <- c(range_length, seeds[j])
    destination <- c(destination, 0)
    source_start <- c(source_start, 0)
    
    source_end <- source_start + range_length
    
    pos_diff = function(x) min(x[x >= 0])
    
    min_difference <- pos_diff(seeds[j] - source_start) 
    index <- which((seeds[j] - source_start) == min_difference)
    index <- index[1] # if there are more 0s
    
    # falls nicht über range abgedeckt wird
    if ((min_difference <= source_end[index]) == TRUE) {
      seeds[j] <- destination[index] + min_difference
    } else {
      seeds[j] <- seeds[j]
    }
  }
}

min(seeds) # 31599214