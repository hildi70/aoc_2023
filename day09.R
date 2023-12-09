input <- readLines("input/day09.txt")
input <- lapply(input, function(x) {as.numeric(unlist(strsplit(x, "\\s")))})

# part 1
prediction <- sapply(input, function(x) {
  last_values <- x[length(x)]
  temp_input <- x
  
  while(!all(diff(temp_input) == 0)) {
    temp_input <- diff(temp_input)
    last_values <- c(last_values, temp_input[length(temp_input)])
  }
  
  sum(last_values)

})

sum(prediction) #1853145119

# part 2
prediction <- sapply(input, function(x) {
  first_values <- x[1]
  temp_input <- x
  
  while(!all(diff(temp_input) == 0)) {
    temp_input <- diff(temp_input)
    first_values <- c(first_values, temp_input[1])
  }
  
  temp_value <- first_values[length(first_values)]
  
  for (i in (length(first_values)-1):1) {
    temp_value <- first_values[i] - temp_value
  }
  
  temp_value
  
})

sum(prediction) #923