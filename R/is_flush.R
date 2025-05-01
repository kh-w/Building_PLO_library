#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a flush and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_flush(c("Ks","As","2s","3s","4s"))
#' @export
is_flush <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)

  # Extract suits
  suits <- substr(hand,2,2)

  strength <- NA

  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  non_consecutive <- !(consecutive_high | consecutive_low)

  # Check if all suits are the same
  same_suit <- length(unique(suits)) == 1

  if(same_suit & non_consecutive){
    strength <- max(sapply(ranks, card_value_high))
  }

  return(list(same_suit & non_consecutive, strength))
}
