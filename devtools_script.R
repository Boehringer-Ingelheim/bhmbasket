
# library(sinew)
# makeOxygen(LoadScenario)

# setwd(path_to_package)

# https://r-pkgs.org/release.html

rm(list = ls())
devtools::document()
devtools::install()
.rs.restartR()

citation("bhmbasket")

library(bhmbasket)

# usethis::use_vignette("bhmbasket_on_HPC")

devtools::run_examples()
# devtools::build_vignettes()
devtools::check()
devtools::build() # creates tar.gz file
# available::available("bhmbasket")

## create cran comments
# usethis::use_cran_comments()
# results <- devtools::check_rhub()
# results$cran_summary()

# rhub::platforms()
results <- devtools::check_rhub(
  platforms = c("ubuntu-gcc-devel", "macos-m1-bigsur-release",
                "ubuntu-gcc-release", "windows-x86_64-devel"))
results$cran_summary()
## copy-paste results summary to cran-comments.md

## create NEWS file
# usethis::use_news_md()

## check for good practice
goodpractice::gp()

### CRAN submission check-list
devtools::install_deps()
devtools::run_examples()
devtools::spell_check()
devtools::check()
# devtools::check_rhub()
devtools::check_win_devel()
devtools::document()
## Update your NEWS file
## Update DESCRIPTION (e.g. version number)
devtools::spell_check()
## Update cran-comments.md
devtools::check()


# devtools::release()
