
expression({
  usethis::create_package(path = "C:/Users/mat/Documents/R/wxSwiss")
  usethis::git_sitrep()
  usethis::git_vaccinate()
  usethis::use_namespace(roxygen = TRUE)
  usethis::use_ccby_license()
})

# usethis::use_readme_rmd()

## ------------------------------------- Upon build --------------------------------------
# usethis::use_data_table()
# usethis::use_pipe()

devtools::load_all()
usethis:::use_data(
  lib.crs,
  smn.para,
  internal=FALSE, overwrite=TRUE
)

devtools::document()
devtools::check()
devtools::build_readme()

p <- devtools::build()
detach("package:wxSwiss", unload = TRUE)
devtools::install_local(p, force = TRUE, upgrade = "never")
library(wxSwiss)

# git add
# git tag 0.0.1
# git push --tags 0.0.1

# git remote add origin git@github.com:m-saenger/wxSwiss.git
devtools::install_github("m-saenger/wxSwiss")


