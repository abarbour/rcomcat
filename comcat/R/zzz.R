# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  pack <- "comcat"
  packageStartupMessage(sprintf("Loaded %s (%s) -- tools for searching the ANSS ComCat earthquake catalog",
                                pack,
                                utils::packageVersion(pack)))
}
