#' Whether a 5-card hand is valid
#'
#'
#'
#' @param hand A hand that consists of 5 cards
#' @return True or False
#' @examples
#' hand_validation(c("2h","3c","3s","4d","Kh"))
#' @export
hand_validation <- function(hand){
  uniqueness <- length(unique(hand)) == 5
  card_mapping <- c("A" = 1, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  rank_check <- all(substr(hand,1,1) %in% names(card_mapping))
  suit_check <- all(substr(hand,2,2) %in% c("d","c","h","s"))
  return(uniqueness & rank_check & suit_check)
}
