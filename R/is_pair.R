#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a pair and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_pair(c("5s","6h","5c","5h","7d"))
#' @export
is_pair <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)

  strength <- NA

  # Check if the hand has a pair
  pair <- length(unique(ranks)) == 4

  # get strength
  if(pair){
    df <- data.frame(sort(table(ranks), decreasing = TRUE))
    strength <- as.numeric(as.character(df[1,"ranks"])) * 100
    kickers <- sum(as.numeric(as.character(df[2:4,"ranks"])))
    strength <- strength + kickers
  }

  return(list(pair, strength))
}

