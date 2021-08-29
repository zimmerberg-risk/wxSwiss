# https://xiaolishen.medium.com/use-multiple-ssh-keys-for-different-github-accounts-on-the-same-computer-7d7103ca8693

# usethis::edit_r_environ()
# usethis::edit_git_config()
# usethis::edit_r_profile()
# usethis::use_readme_rmd()

usethis::create_package(path = "C:/Users/mat/Documents/R/wxSwiss")
usethis::git_sitrep()
usethis::git_vaccinate()
usethis::use_namespace(roxygen = TRUE)
usethis::use_ccby_license()

## ------------------------------------- Upon build --------------------------------------
# usethis::use_data_table()
# usethis::use_pipe()

devtools::load_all()
usethis:::use_data(
  lib.mod,
  lib.para,
  lib.units,
  lib.paper,
  lib.theme,
  lib.ffmpeg,
  lib.cs,
  lib.logo,
  perils.logo,
  internal=FALSE, overwrite=TRUE
)

devtools::document()
devtools::check()
devtools::build_readme()

p <- devtools::build()
detach("package:wxChart", unload = TRUE)
devtools::install_local(p, force = TRUE, upgrade = "never")
library(wxChart)

v <- usethis::use_version(which = "patch") #major, minor, patch
# library(wxChart)

# git checkout master
# git tag 0.0.2
# git push --tags

# git remote add origin git@github.com:m-saenger/wxChart.git
devtools::install_github("m-saenger/wxChart")


