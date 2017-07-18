#' @title set/change the default `add_fields` used by makeOxygen
#' @description set default values for `add_field` which are retained among different
#' calls to `makeOxygen`, by creating and setting a "sinew_defaultaddfields" entry in `options`
#' If called without arguments (the default), `add_fields` is set/reset to the default value
#' (corresponding to "details", "examples", "seealso", "rdname", "export")).
#' If an `add_field` argument is passed, the default values are changed (see examples)
#' @param add_fields PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # set/reset add_fields to default
#'  setDefaultAddfields()
#'  makeOxygen(stats::lm)
#'
#'  # change default `add_fields`: After this, all calls to `makeOxygen`
#'  # without an `add_fields` argument will use this setting
#'
#'  setDefaultAddfields(c("details", "rdname", "export", "author"))
#'  makeOxygen(stats::lm)
#'  }
#' }
#' @rdname setDefaultAddfields
#' @export

setDefaultAddfields <- function(add_fields = NULL) {

  if (is.null(add_fields)) {

    # If `add_fields` is not passed, set add_fields to default. This allows to automatically set
    # the defaults at first execution of `makeOxygen` using
    # `if (is.null(options("sinew_defaultaddfields"))) setDefaultAddfields()`
    # and/or to reset add_fields to default at anytime using setDefaultAddfields()

    message('sinew --> Setting default `add_fields` to: "details", "examples", "seealso", "rdname", "export")')

    options(sinew_defaultaddfields = c("details", "examples", "seealso", "rdname", "export"))
  } else {
    # If `add_fields` passed, check if it is "valid" and set the default add_fields accordingly

    # Abort on wrong "add_field" type
    if (!is.character(add_fields)) {
      stop("sinew --> `add_fields` must be a character array. Aborting.")
    }

    # set a list of allowed fields
    header_add=c(
      author            ="AUTHOR [AUTHOR_2]",
      backref           ="src/filename.cpp",
      concept           ="CONCEPT_TERM_1 [CONCEPT_TERM_2]",
      describeIn        ="FUNCTION_NAME DESCRIPTION",
      details           ="DETAILS",
      #evalRd           ="",
      example           ="path_to_file/relative/to/packge/root",
      examples          ="\n#' \\dontrun{\n#' if(interactive()){\n#'  #EXAMPLE1\n#'  }\n#' }",
      export            ="",
      #exportClass      ="",
      #exportMethod     ="",
      family            ="FAMILY_TITLE",
      field             ="FIELD_IN_S4_RefClass DESCRIPTION",
      format            ="DATA_STRUCTURE",
      importClassesFrom ="PKG CLASS_a [CLASS_b]",
      importMethodsFrom ="PKG METHOD_a [METHOD_b]",
      include           ="FILENAME.R [FILENAME_b.R]",
      inherit           ="[PKG::]SOURCE_FUNCTION [FIELD_a FIELD_b]",
      inheritDotParams  ="[PKG::]SOURCE_FUNCTION",
      inheritSection    ="[PKG::]SOURCE_FUNCTION [SECTION_a SECTION_b]",
      keywords          ="KEYWORD_TERM",
      name              ="NAME",
      #note             ="",
      #noRd             ="",
      #rawRd            ="",
      #rawNamespace     ="",
      rdname            ="FUNCTION_NAME",
      references        ="BIB_CITATION",
      section           ="SECTION_NAME",
      source            ="\\url{http://somewhere.important.com/}",
      slot              ="SLOTNAME DESCRIPTION",
      template          ="FILENAME",
      templateVar       ="NAME VALUE",
      useDynLib         ="PKG [ROUTINE_a ROUTINE_b]"
    )

    # Check if all values passed in `add_fields` are valid. If yes, set
    # options("sinew_defaultaddfields"), otherwise abort without changing the defaults
    if (all(add_fields %in% c(names(header_add), "seealso")) ) {
      message("sinew --> Setting default `add_fields` to: ",
              paste(add_fields, collapse = ", "))
      options(sinew_defaultaddfields = add_fields)
    } else {
      stop("Invalid values found in `add_fields`. See ?makeOxygen for a list of allowed fields.
             Aborting")
    }
  }
}
