.onAttach <- function(libname, pkgname) {
  madrat::madratAttach(pkgname)
}

.onDetach <- function(libpath) {
  madrat::madratDetach(libpath)
}

# redirect standard messaging functions to vcat
cat     <- function(...) madrat::vcat(1, ...)
message <- function(...) madrat::vcat(1, ...)
warning <- function(...) madrat::vcat(0, ...)
stop    <- function(...) madrat::vcat(-1, ...)
