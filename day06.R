library(stringr)
input <- readLines("input/day06.txt")

time <- as.numeric(unlist(str_extract_all(input[1], "[0-9]+")))
best_distance <- as.numeric(unlist(str_extract_all(input[2], "[0-9]+")))


# part 1

# relationship between time_max and distance can be expressed as:
# distance = (time_max - time) * time

distance <- function(time) {
  time_max <- max(time)
  return((time_max - time)*time)
}

n_possibilites <- c()

for (i in seq_along(time)) {
  press_time <- c(1:time[i])
  n_possibilites[i] <- length(which(distance(press_time)>best_distance[i]))
}

prod(n_possibilites) # 861300


# part 2

time_max <- as.numeric(paste0(time, collapse = ""))
best_distance <- as.numeric(paste0(best_distance, collapse = ""))

# distance = (time_max - press_time) * press_time
# -> 1 * press_time^2 - time_max * press_time + best_distance = 0

# solve quadratic equation for x (= press_time) via pq formula:
# lower_limit = -(p/2)-sqrt((p/2)^2 - q)
# upper_limit = -(p/2)+sqrt((p/2)^2 - q)

p <- -1 * time_max
q <- best_distance

lower_press_time <- ceiling(-(p/2)-sqrt((p/2)^2 - q))
upper_press_time <- floor(-(p/2)+sqrt((p/2)^2 - q))
n_possibilities <- upper_press_time - lower_press_time + 1 
n_possibilities # 28101347


