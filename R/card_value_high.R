#' Return the value of a single card under "high" assumption, i.e. A = 14
#'
#'
#'
#' @param card The number (without suit) of a single card
#' @return a number from 2,3,....,14
#' @examples
#' card_value_high("A")
#' @export
card_value_high <- function(card) {
  card_mapping <- c("A" = 14, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  if (card %in% names(card_mapping)) {
    return(card_mapping[card])
  } else {
    return(NA)  # Return NA for unmapped cards
  }
}
