#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a trips and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_trips(c("5s","6h","5c","5h","7d"))
#' @export
is_trips <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)

  strength <- NA

  # Check if any ranks has appearance = 3
  max_count_equals_3 <- max(table(ranks)) == 3
  min_count_equals_1 <- min(table(ranks)) == 1

  # get strength
  if(max_count_equals_3 & min_count_equals_1){
    df <- data.frame(table(ranks))
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kickers <-  sum(ranks[ranks != as.numeric(as.character(df$ranks))[which.max(df$Freq)]])
    strength <- strength + kickers
  }

  return(list(max_count_equals_3 & min_count_equals_1, strength))
}

