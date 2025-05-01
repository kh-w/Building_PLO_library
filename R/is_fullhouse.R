#' Return the list: list(T/F, strength) of a 5-card hand stating whether (T/F) a 5-card hand is a full house and its strength if T
#'
#'
#'
#' @param hand A 5-card hand
#' @return list(T/F, strength)
#' @examples
#' is_fullhouse(c("5s","6h","5c","5h","6d"))
#' @export
is_fullhouse <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }

  # Extract ranks
  ranks <- substr(hand,1,1)

  strength <- NA

  # Check if any ranks has appearance = 4
  max_count_equals_3 <- max(table(ranks)) == 3
  min_count_equals_2 <- min(table(ranks)) == 2

  # get strength
  if(max_count_equals_3 & min_count_equals_2){
    df <- data.frame(table(ranks))
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kicker <- as.numeric(as.character(df$ranks))[which.min(df$Freq)]
    strength <- strength + kicker
  }

  return(list(max_count_equals_3 & min_count_equals_2, strength))
}
