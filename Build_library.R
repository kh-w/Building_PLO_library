library(usethis)
# create_package("PLO")
usethis::use_r("hand_validation")
usethis::use_r("card_value_low")
usethis::use_r("card_value_high") #
usethis::use_r("is_straight_flush")
usethis::use_r("is_straight")
usethis::use_r("is_flush")
usethis::use_r("is_quads")
usethis::use_r("is_fullhouse")
usethis::use_r("is_trips")
usethis::use_r("is_two_pairs")
usethis::use_r("is_pair")
usethis::use_r("is_high_card")

# Add your function with roxygen2 comments
# Then:
library(devtools)
document()    # Generate NAMESPACE and man/
load_all()    # Load package
# Test functions
hand_validation(c("2h","3c","3s","4d","Kh"))
card_value_low("A")
card_value_high("A")
is_straight_flush(c("As","2s","3s","4s","5s"))
is_straight_flush(c("Ts","Js","Qs","Ks","As"))
is_straight_flush(c("Ks","As","2s","3s","4s"))
is_straight_flush(c("5s","6h","7s","8c","9d"))
is_straight(c("As","2s","3s","4s","5s"))
is_straight(c("Ts","Js","Qs","Ks","As"))
is_straight(c("Ks","As","2s","3s","4s"))
is_straight(c("5s","6h","7s","8c","9d"))
is_flush(c("As","2s","3s","4s","5s"))
is_flush(c("Ts","Js","Qs","Ks","As"))
is_flush(c("Ks","As","2s","3s","4s"))
is_flush(c("5s","6h","7s","8c","9d"))
is_quads(c("As","Ad","Ah","Ac","5s"))
is_quads(c("5s","5d","5h","5c","As"))
is_quads(c("Ts","Js","Qs","Ks","As"))
is_quads(c("Ks","As","2s","3s","4s"))
is_quads(c("5s","6h","7s","8c","9d"))
is_fullhouse(c("5s","6h","5c","5h","6d"))
is_fullhouse(c("6s","5h","6c","6h","5d"))
is_trips(c("5s","6h","5c","5h","7d"))
is_trips(c("6s","5h","6c","6h","5d"))
is_two_pairs(c("5s","6h","5c","5h","7d"))
is_two_pairs(c("6s","5h","6c","6h","5d"))
is_two_pairs(c("As","5h","6c","6h","5d"))
is_pair(c("5s","6h","5c","5h","7d"))
is_pair(c("6s","5h","6c","6h","5d"))
is_pair(c("As","5h","6c","6h","5d"))
is_pair(c("As","5h","6c","6h","4d"))
is_high_card(c("5s","6h","7c","8h","Td"))
is_high_card(c("6s","5h","6c","6h","5d"))
is_high_card(c("As","5h","6c","6h","5d"))
is_high_card(c("As","5h","6c","6h","4d"))

check()       # Validate the package
build()       # Build the .tar.gz file
