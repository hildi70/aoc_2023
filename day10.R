input <- readLines("input/day10.txt")
input <- matrix(unlist(strsplit(input, "")),  ncol = nchar(input[1]), byrow = TRUE)

input <- gsub("7", "Z", input)

south_neighbors <- c("L", "J", "|")
north_neighbors <- c("Z", "F", "|")
east_neighbors <- c("Z", "J", "-")
west_neighbors <- c("L", "F", "-")

south_coordinates <- c(1, 0)
north_coordinates <- c(-1, 0)
east_coordinates <- c(0, 1)
west_coordinates <- c(0, -1)


looking_north <- function(x, temp_coordinates, i) {
  row <- temp_coordinates[i,1] + north_coordinates[1]
  col <- temp_coordinates[i,2] + north_coordinates[2]
  if ((row > 0) && (x[row, col] %in% north_neighbors)) {
    x[row, col] <- distance_counter + 1
  }
  x
}

looking_south <- function(x, temp_coordinates, i) {
  row <- temp_coordinates[i,1] + south_coordinates[1]
  col <- temp_coordinates[i,2] + south_coordinates[2]
  if ((row <= nrow(x)) && (x[row, col] %in% south_neighbors)) {
    x[row, col] <- distance_counter + 1
  }
  x
}

looking_east <- function(x, temp_coordinates, i) {
  row <- temp_coordinates[i,1] + east_coordinates[1]
  col <- temp_coordinates[i,2] + east_coordinates[2]
  if ((col <= ncol(x)) && (x[row, col] %in% east_neighbors)) {
    x[row, col] <- distance_counter + 1
  }
  x
}

looking_west <- function(x, temp_coordinates, i) {
  row <- temp_coordinates[i,1] + west_coordinates[1]
  col <- temp_coordinates[i,2] + west_coordinates[2]
  if ((col > 0) && (x[row, col] %in% west_neighbors)) {
    x[row, col] <- distance_counter + 1
  }
  x
}

distance_counter = 0
start_coordinates <- which(input == "S", arr.ind = T)
temp_coordinates <- start_coordinates

while(nrow(temp_coordinates) > 0) {
  
  for (i in 1:nrow(temp_coordinates)) {
    input <- looking_north(x = input, temp_coordinates = temp_coordinates, i = i)
    input <- looking_south(x = input, temp_coordinates = temp_coordinates, i = i)
    input <- looking_east(x = input, temp_coordinates = temp_coordinates, i = i)
    input <- looking_west(x = input, temp_coordinates = temp_coordinates, i = i)
  }
    distance_counter <- distance_counter + 1
    
    temp_coordinates <-  which(input == distance_counter, arr.ind = T)
}

needed_steps <- as.numeric(input[which(input == distance_counter-1, arr.ind = T)])
needed_steps # 6828
