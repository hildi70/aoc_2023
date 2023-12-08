library(stringr)
library(stringi)
library(dplyr)

input <- readLines("input/day07.txt")

extract_element <- function(list, n){
  sapply(list, `[`, n)
}

# part 1
all_cards <- str_extract_all(input, "\\w+(?=\\s)")
bid <- as.numeric(unlist(str_extract_all(input, "(?<=\\s)\\d+")))

cards_char <- c("T", "J", "Q", "K", "A")
cards_numeric <- c("10", "11", "12", "13", "14")

single_cards <- sapply(all_cards, function(x) {strsplit(x, "")})
single_cards <- lapply(single_cards, function(x) {stri_replace_all_regex(x,
                         pattern = cards_char,
                         replacement = cards_numeric,
                         vectorize=FALSE)})
single_cards <- lapply(single_cards, as.numeric)

card_counter <- sapply(single_cards, table)

cards_df <- as.data.frame(cbind(all_cards = unlist(all_cards),
                                bid = as.numeric(unlist(bid)),
                                first_card = "",
                                second_card = "",
                                third_card = "",
                                fourth_card = "",
                                fifth_card = ""),
                                set = numeric())

for (i in 1:5) {
  cards_df[,i+2] <- extract_element(single_cards, i)
}

cards_df[which(sapply(card_counter, function(x) {any(x == 5)})), "set"] <- 7                               # five of a kind
cards_df[which(sapply(card_counter, function(x) {any(x == 4)})) , "set"] <- 6                              # four of a kind
cards_df[which(sapply(card_counter, function(x) {(sum(x == 3) == 1) && (sum(x == 2) == 1)})) , "set"] <- 5 # full house
cards_df[which(sapply(card_counter, function(x) {(sum(x == 3) == 1) && (sum(x == 1) == 2)})), "set"] <- 4  # three of a kind
cards_df[which(sapply(card_counter, function(x) {(sum(x == 2) == 2) && (sum(x == 1) == 1)})) , "set"] <- 3 # two pairs
cards_df[which(sapply(card_counter, function(x) {(sum(x == 2) == 1) && (sum(x == 1) == 3)})) , "set"] <- 2 # one pair
cards_df[which(sapply(card_counter, function(x) {sum(x == 1) == 5})) , "set"] <- 1                         # high card (all distinct)

cards_df <- cards_df %>%
  group_by(set,first_card, second_card, third_card, fourth_card, fifth_card) %>%
  mutate(rank = cur_group_id(),
         winnings = rank * as.numeric(bid))

sum(cards_df$winnings) # 253910319

# part 2

single_cards <- lapply(single_cards, function(x) {stri_replace_all_regex(x,
                                                                         pattern = 11,
                                                                         replacement = 1,
                                                                         vectorize=FALSE)})
single_cards <- lapply(single_cards, as.numeric)

for (i in 1:5) {
  cards_df[,i+2] <- extract_element(single_cards, i)
}

cards_df <- cards_df %>%
  mutate(n_joker = nchar(gsub("[^J]", "", all_cards)),
         set = case_when(
           set == 1 & n_joker == 1 ~ 2,
           set == 2 & n_joker == 1 ~ 4,
           set == 2 & n_joker == 2 ~ 4,
           set == 3 & n_joker == 1 ~ 5,
           set == 3 & n_joker == 2 ~ 6,
           set == 4 & n_joker == 1 ~ 6,
           set == 4 & n_joker == 3 ~ 6,
           set == 5 & n_joker == 2 ~ 7,
           set == 5 & n_joker == 3 ~ 7,
           set == 6 & n_joker == 1 ~ 7,
           set == 6 & n_joker == 4 ~ 7,
           TRUE ~ set
         ))

cards_df <- cards_df %>%
  group_by(set,first_card, second_card, third_card, fourth_card, fifth_card) %>%
  mutate(rank = cur_group_id(),
         winnings = rank * as.numeric(bid))

sum(cards_df$winnings) #  254083736
