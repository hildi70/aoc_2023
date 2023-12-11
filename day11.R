input <- readLines("input/day11.txt")
input <- gsub("\\.", 0, input)
input <- gsub("#", 1, input)
input <- matrix(as.numeric(unlist(strsplit(input, ""))), nrow = nchar(input[1]), byrow = T)

# part 1
added_rows <- which(rowSums(input[,]) == 0)
added_rows <- added_rows + 0:(length(added_rows)-1)
row_input <- matrix(0, nrow = nrow(input)+length(added_rows), ncol = ncol(input))
row_input[-added_rows, ] <- input

added_cols <- which(colSums(row_input[,]) == 0)
added_cols <- added_cols + 0:(length(added_cols)-1)
col_input <- matrix(0, nrow = nrow(row_input), ncol = ncol(row_input)+length(added_cols))
col_input[, -added_cols] <- row_input

input_expanded <- col_input

coordinates <- which(input_expanded == 1, arr.ind = T)
distance <- dist(coordinates, method = "manhattan")
sum(distance) # 10276166


# part 2
added_rows <- which(rowSums(input[,]) == 0)
added_rows <- added_rows+0:(length(added_rows)-1)*(1000000-1)

added_cols <- which(colSums(input[,]) == 0)
added_cols <- added_cols+0:(length(added_cols)-1)*(1000000-1)

coordinates <- which(input == 1, arr.ind = T)

for (i in seq_along(added_rows)) {
  coordinates[which(coordinates[,1] > added_rows[i]), 1] <- coordinates[which(coordinates[,1] > added_rows[i]), 1] + 1000000-1
}

for (i in seq_along(added_cols)) {
  coordinates[which(coordinates[,2] > added_cols[i]), 2] <- coordinates[which(coordinates[,2] > added_cols[i]), 2] + 1000000-1
}

distance <- dist(coordinates, method = "manhattan")
sum(distance) # 598693078798
