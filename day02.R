library(stringi)
input <- readLines("input/day02.txt")

input_as_numbers <- stri_replace_all_regex(input,
                                           pattern = c("Game [0-9]+: ", "red", "green", "blue"),
                                           replacement = c("", 12, 13, 14),
                                           vectorize=FALSE)

n_cubes <- stri_extract_all(input_as_numbers, regex = "[0-9]+(?=\\s)")
max_cubes <- stri_extract_all(input_as_numbers, regex = "(?<=[0-9]\\s)[0-9]+")


# part 1

counter = 0

for (i in 1:length(n_cubes)){
  n_cubes_num <- as.numeric(unlist(n_cubes[[i]]))
  max_cubes_num <- as.numeric(unlist(max_cubes[[i]]))
  
  if (all(max_cubes_num-n_cubes_num >= 0)) {
    counter <- counter + i
  }
}

counter # 2541

# part 2
# find minimum set of cubes

power = 0

for (i in 1:length(n_cubes)){
  n_cubes_num <- as.numeric(unlist(n_cubes[[i]]))
  max_cubes_num <- as.numeric(unlist(max_cubes[[i]]))
  
  red_min <- max(n_cubes_num[grep(12, max_cubes_num)])
  green_min <- max(n_cubes_num[grep(13, max_cubes_num)])
  blue_min <- max(n_cubes_num[grep(14, max_cubes_num)])
  
  power = power + red_min * green_min * blue_min
}

power # 66016
