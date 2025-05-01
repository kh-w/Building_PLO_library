#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a two pairs and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_two_pairs(c("5s","6h","5c","5h","7d"))
#' @export
is_two_pairs <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)

  strength <- NA

  # Check if the shape of the hand is 1,2,2
  if(length(sort(table(ranks))) != 3){
    hand_shape_122 <- FALSE
  }else{
    hand_shape_122 <- all(sort(table(ranks)) == c(1,2,2))
  }

  # get strength
  if(hand_shape_122){
    df <- data.frame(sort(table(ranks), decreasing = TRUE))
    strength <- sum(as.numeric(as.character(df[1:2,"ranks"]))) * 100
    kicker <- as.numeric(as.character(df[3,"ranks"]))
    strength <- strength + kicker
  }

  return(list(hand_shape_122, strength))
}
