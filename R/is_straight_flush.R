#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a straight flush and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_straight_flush(c("As","2s","3s","4s","5s"))
#' @export
is_straight_flush <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  suits <- substr(hand,2,2)

  # Check if all suits are the same
  same_suit <- length(unique(suits)) == 1

  strength <- NA

  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  if(consecutive_high == TRUE){
    strength <- max(sort(sapply(ranks, card_value_high)))
  }
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  if(consecutive_low == TRUE){
    strength <- max(sort(sapply(ranks, card_value_low)))
  }
  consecutive <- consecutive_high | consecutive_low

  if(!(same_suit & consecutive)){
    strength <- NA
  }

  return(list(same_suit & consecutive, strength))
}
