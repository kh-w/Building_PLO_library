#' Return the value of a single card under "low" assumption, i.e. A = 1
#'
#'
#'
#' @param card The number (without suit) of a single card
#' @return a number from 1,2,....,13
#' @examples
#' card_value_low("A")
#' @export
card_value_low <- function(card) {
  card_mapping <- c("A" = 1, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  if (card %in% names(card_mapping)) {
    return(card_mapping[card])
  } else {
    return(NA)  # Return NA for unmapped cards
  }
}
