library(stringr)
input <- readLines("input/day04.txt")

winners <- lapply(input, function(x) {as.numeric(unlist(str_split(str_trim(str_extract_all(x, "(?<=:\\s).+(?=\\s\\|)")), "\\s+")))})
numbers <- lapply(input, function(x) {as.numeric(unlist(str_split(str_trim(str_extract_all(x, "(?<=\\|).+")), "\\s+")))})

# part 1
n_matches <- mapply(x = winners, y = numbers, function(x, y) {sum(y %in% x)})
n_matches <- n_matches[n_matches > 0]

points <- sapply(n_matches, function(n_matches) {2^(n_matches-1)})
sum(points) # 25651

# part 2
n_matches <- mapply(x = winners, y = numbers, function(x, y) {sum(y %in% x)})
card_counter <- c(rep(1, times = length(n_matches)))

for (i in 1:(length(n_matches)-1)) {
  j <- i+n_matches[i]
  if (i+n_matches[i] > length(n_matches)) {
    j <- length(n_matches)
  }
  
  if (n_matches[i] != 0) {
    card_counter[c((i+1):j)] <- card_counter[c((i+1):j)] + card_counter[i]
  }
}

sum(card_counter) # 19499881

