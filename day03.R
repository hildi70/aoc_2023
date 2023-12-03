library(stringr)

input <- readLines("input/day03.txt")
input <- gsub("\\.", "x", input)

input <- str_split_fixed(input, "", nchar(input[1]))

input <- cbind(c(rep("x", times = nrow(input))), input, c(rep("x", times = nrow(input))))
input <- rbind(c(rep("x", times = ncol(input))), input, c(rep("x", times = ncol(input))))

# part 1


numb_logical <- matrix(grepl(pattern = "[0-9]", x = input), nrow = nrow(input))
symb_logical <- matrix(grepl(pattern = "[[:punct:]]", x = input), nrow = nrow(input))

parts <- c(0)


for (col_nr in 2:(ncol(numb_logical)-1)) {
  for (row_nr in 2:(nrow(numb_logical)-1)) {
    if ((numb_logical[row_nr, col_nr] == T) && (sum(symb_logical[c((row_nr-1):(row_nr+1)), c((col_nr-1):(col_nr+1))]) > 0)) {
        index_behind <- col_nr -1
        while ((numb_logical[row_nr, index_behind] == T) && (index_behind != 1)) {index_behind <- index_behind - 1}
        index_ahead <- col_nr + 1
        while ((numb_logical[row_nr, index_ahead] == T) && (index_ahead != ncol(numb_logical))) {index_ahead <- index_ahead + 1}
        number <- as.numeric(paste0(input[row_nr, c((index_behind+1):(index_ahead-1))], collapse = ""))
        parts <- c(parts, number)
        numb_logical[row_nr, c(index_behind:index_ahead)] <- F
    }
  }
}

sum(parts) # 537732


# part 2

numb_logical <- matrix(grepl(pattern = "[0-9]", x = input), nrow = nrow(input))
star_logical <- matrix(grepl(pattern = "\\*", x = input), nrow = nrow(input))

gears <- c(0)
number <- c(0)
counter <- 0

star_coordinates <- which(star_logical==1, arr.ind=T)
i <- 1

for (i in 1:nrow(star_coordinates)) {
  if (sum(numb_logical[c((star_coordinates[i,1]-1):(star_coordinates[i,1]+1)), c((star_coordinates[i,2]-1):(star_coordinates[i,2]+1))]) > 1) {
    numb_coordinates <- which(numb_logical[c((star_coordinates[i,1]-1):(star_coordinates[i,1]+1)), c((star_coordinates[i,2]-1):(star_coordinates[i,2]+1))] == T, arr.ind = T)
    
    j <- 1
    while (j < (nrow(numb_coordinates)+1)) {
      col_nr <- star_coordinates[i, 2] + numb_coordinates[j, 2] - 2
      row_nr <- star_coordinates[i, 1] + numb_coordinates[j, 1] - 2
      
      if (numb_logical[row_nr, col_nr] == T) {
        counter <- counter + 1
        index_behind <- col_nr -1
        while ((numb_logical[row_nr, index_behind] == T) && (index_behind != 1)) {index_behind <- index_behind - 1}
        index_ahead <- col_nr + 1
        while ((numb_logical[row_nr, index_ahead] == T) && (index_ahead != ncol(numb_logical))) {index_ahead <- index_ahead + 1}
            
        number[counter] <- as.numeric(paste0(input[row_nr, c((index_behind+1):(index_ahead-1))], collapse = ""))
        numb_logical[row_nr, c(index_behind:index_ahead)] <- F
      }
      
      j <- j+1
    }
    
    if (counter == 2) {
      gear_ratio = number[1] * number[2]
      gears <- c(gears, gear_ratio)
    }
      
    counter <- 0
    number <- c(0)
  }
}
sum(gears) # 84883664
