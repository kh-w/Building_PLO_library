#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a high card and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_high_card(c("5s","6h","7c","8h","Td"))
#' @export
is_high_card <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  ranks_value <- sapply(ranks, card_value_high)
  suits <- substr(hand,2,2)

  length_5 <- length(unique(ranks)) == 5
  diff_suits <- length(unique(suits)) != 1

  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  non_consecutive <- !(consecutive_high | consecutive_low)

  strength <- NA

  if(length_5 & diff_suits & non_consecutive){
    strength <- sum(ranks_value)
  }

  return(list(length_5 & diff_suits & non_consecutive, strength))
}


