## ------------------------------------- Def --------------------------------------

pkg.name <- "wxSwiss"
github <- "m-saenger"
version <- "0.2.1"

## ------------------------------------- Build --------------------------------------

devtools::load_all()

# Register data sets

usethis:::use_data(
  lib.crs,
  smn.para,
  internal=FALSE, overwrite=TRUE
)

# Document and check
devtools::document()
devtools::check()
devtools::build_readme()
devtools::build_site()
devtools::build_vignettes()

# Build and install local
p <- devtools::build()
detach(sprintf("package:%s", pkg.name), unload = TRUE)
devtools::install_local(p, force = TRUE, upgrade = "never", build_manual = T, build_vignettes = T)

## ------------------------------------- Install from Github --------------------------------------
expression({

  # Install tagged version
  devtools::install_github(sprintf("%s/%s@%s", github, pkg.name, version))
  # Install latest
  devtools::install_github(sprintf("%s/%s", github, pkg.name))


})

