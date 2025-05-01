#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a quads and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_quads(c("5s","5d","5h","5c","As"))
#' @export
is_quads <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)

  # Check if any ranks has appearance = 4
  count_equals_4 <-  max(table(ranks)) == 4

  strength <- NA

  # get strength
  if(count_equals_4){
    df <- data.frame(table(ranks))
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kicker <- as.numeric(as.character(df$ranks))[which.min(df$Freq)]
    strength <- strength + kicker
  }

  return(list(count_equals_4, strength))
}
