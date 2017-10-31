get_oxy <- function(this_oxy) {
  this_oxy <- gsub("\\n$", "", strsplit(this_oxy, "#' @")[[1]])
  this_oxy <- this_oxy[nchar(this_oxy) > 0]
  this_oxy_vals <- gsub("^(.*?)\\s+", "", this_oxy)

  names(this_oxy_vals) <- regmatches(this_oxy, regexpr("(?:\\S+\\s+){0}(\\S+)", this_oxy))

  long_names <- which(names(this_oxy_vals) %in% c("importFrom", "param"))
  if (length(long_names) > 0) {
    names(this_oxy_vals)[long_names] <- regmatches(this_oxy[long_names], regexpr("(?:\\S+\\s+){1}(\\S+)", this_oxy[long_names]))
    this_oxy_vals[long_names] <- sapply(
      strsplit(this_oxy[long_names], "\\s+"),
      function(x) paste(utils::tail(x, -2), collapse = " "),
      USE.NAMES = FALSE
    )
  }

  if ("export" %in% names(this_oxy_vals)) this_oxy_vals["export"] <- ""

  this_oxy_vals
}
