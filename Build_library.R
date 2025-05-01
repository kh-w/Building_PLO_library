library(usethis)
# create_package("PLO")
usethis::use_r("hand_validation")
usethis::use_r("card_value_low")
usethis::use_r("card_value_high")
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

check()       # Validate the package
build()       # Build the .tar.gz file
