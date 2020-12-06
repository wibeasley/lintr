get_function_calls <- function(x) {
  as.character(xml2::xml_find_all(x, "//SYMBOL_FUNCTION_CALL/text()"))
}

get_import_exprs <- function(x) {
  xml2::xml_find_all(x, "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]]")
}

get_package_imports <- function(filename) {
  pkg <- find_package(path = filename)
  if (is.null(pkg) || !file.exists(file.path(pkg, "NAMESPACE"))) return(character())

  ns <- readLines(file.path(pkg, "NAMESPACE"))

  # Extract package names from import(pkg) and importFrom(pkg,fun)
  # From https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages, Section 1.1.1:
  # The mandatory ‘Package’ field gives the name of the package.
  # This should contain only (ASCII) letters, numbers and dot, have at least two characters and
  # start with a letter and not end in a dot.
  pkgname_regex <- rex::rex(
    letter, any_of(alnum, "."), alnum
  )
  imports <- rex::re_matches(
    ns,
    rex::rex(
      start, "import", maybe("From"), "(",
      capture(name = "package", pkgname_regex)
    )
  )[, "package"]

  unique(imports[!is.na(imports)])
}

#' @describeIn linters checks that libraries imported are actually used.
#' @export
unused_import_linter <- function(except_packages = "tidyverse") {
  function(source_file) {
    if (is.null(source_file$full_xml_parsed_content)) return(list())

    import_exprs <- get_import_exprs(source_file$full_xml_parsed_content)
    if (length(import_exprs) == 0) {
      return()
    }
    pkg_exprs <- xml2::xml_find_first(import_exprs, "./expr[STR_CONST|SYMBOL]")
    import_pkgs <- strip_names(as.character(xml2::xml_find_first(pkg_exprs, "./*/node()")))

    function_calls <- get_function_calls(source_file$full_xml_parsed_content)

    lapply(
      seq_along(import_pkgs),
      function(i) {
        pkg <- import_pkgs[[i]]

        if (pkg %in% except_packages) {
          return()
        }

        package_exports <- getNamespaceExports(pkg)
        if (!any(package_exports %in% function_calls)) {
          import_expr <- xml2::as_list(import_exprs[[i]])
          pkg_expr <- xml2::as_list(pkg_exprs[[i]])
          line_num <- import_expr@line1
          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = pkg_expr@col1,
            type = "warning",
            message = paste0("package ", pkg, " is never used."),
            line = source_file[["file_lines"]][[as.numeric(line_num)]],
            ranges = list(as.numeric(c(import_expr@col1, import_expr@col2))),
            linter = "unused_import_linter"
          )
        }
      }
    )
  }
}
