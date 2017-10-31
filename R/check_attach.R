check_attach <- function(x, nenv) {
  fn <- gsub("^(.*?)::", "", x)
  pkg <- gsub("::(.*?)$", "", x)
  if (!pkg %in% loadedNamespaces()) {
    attachNamespace(pkg)
    if (!exists("toUnload", envir = nenv)) {
      nenv$toUnload <- c()
    }
    nenv$toUnload <- unique(c(nenv$toUnload, sprintf("package:%s", pkg)))
  }
}
