#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a straight and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_straight(c("As","2s","3s","4s","5s"))
#' @export
is_straight <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  suits <- substr(hand,2,2)

  # Check if all suits are the same, if yes, not straight
  diff_suit <- length(unique(suits)) != 1

  strength <- NA

  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  if(consecutive_high == TRUE & diff_suit){
    strength <- max(sort(sapply(ranks, card_value_high)))
  }
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  if(consecutive_low == TRUE & diff_suit){
    strength <- max(sort(sapply(ranks, card_value_low)))
  }
  consecutive <- consecutive_high | consecutive_low

  return(list(consecutive & diff_suit, strength))
}

