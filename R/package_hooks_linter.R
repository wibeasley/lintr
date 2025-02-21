#' @describeIn linters Check various common "gotchas" in [.onLoad()], [.onAttach()], [.Last.lib()], and [.onDetach()]
#'    namespace hooks that will cause `R CMD check` issues.
#' @export
package_hooks_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    bad_msg_calls <- c("cat", "message", "print", "writeLines")
    bad_calls <- list(
      ".onLoad" = c(bad_msg_calls, "packageStartupMessage"),
      ".onAttach" = c(bad_msg_calls, "library.dynam")
    )

    # lints here will hit the function <expr>,
    #   this path returns to the corresponding namespace hook's name
    get_hook <- function(xml) {
      ns_calls <- xp_text_in_table(c(".onLoad", ".onAttach", ".onDetach", ".Last.lib"))
      xml2::xml_text(xml2::xml_find_all(xml, sprintf("./ancestor::expr/expr/SYMBOL[%s]", ns_calls)))
    }

    # (1) improper messaging calls shouldn't be used inside .onLoad()/.onAttach()
    bad_msg_calls <- c("cat", "message", "print", "writeLines")
    bad_calls <- list(
      ".onLoad" = c(bad_msg_calls, "packageStartupMessage"),
      ".onAttach" = c(bad_msg_calls, "library.dynam")
    )

    bad_msg_call_xpath_fmt <- "
      //expr[SYMBOL[text() = '%s']]
      /following-sibling::expr[FUNCTION]
      //SYMBOL_FUNCTION_CALL[%s]
    "

    # inherits: xml, source_file, bad_msg_call_xpath_fmt
    bad_msg_call_lints <- function(hook) {
      xpath <- sprintf(bad_msg_call_xpath_fmt, hook, xp_text_in_table(bad_calls[[hook]]))
      bad_expr <- xml2::xml_find_all(xml, xpath)
      lapply(bad_expr, make_bad_call_lint, source_file, hook)
    }

    onload_bad_msg_call_lints <- bad_msg_call_lints(".onLoad")
    onattach_bad_msg_call_lints <- bad_msg_call_lints(".onAttach")

    # (2) .onLoad() and .onAttach() should take two arguments, with names matching ^lib and ^pkg
    load_arg_name_xpath <- "
    //expr[SYMBOL[text() = '.onAttach' or text() = '.onLoad']]
    /following-sibling::expr[
      FUNCTION
      and (
        count(SYMBOL_FORMALS) != 2
        or SYMBOL_FORMALS[
          (position() = 1 and not(starts-with(text(), 'lib')))
          or (position() = 2 and not(starts-with(text(), 'pkg')))
        ]
      )
    ]
    "

    load_arg_name_expr <- xml2::xml_find_all(xml, load_arg_name_xpath)

    load_arg_name_lints <- lapply(
      load_arg_name_expr,
      function(expr) {
        message <- sprintf(
          "%s() should take two arguments, with the first starting with 'lib' and the second starting with 'pkg'.",
          get_hook(expr)
        )
        xml_nodes_to_lint(expr, source_file, message, type = "warning")
      }
    )

    # (3) .onLoad() and .onAttach() shouldn't call require(), library(), or installed.packages()
    # NB: base only checks the SYMBOL_FUNCTION_CALL version, not SYMBOL.
    library_require_xpath <- "
    //expr[SYMBOL[text() = '.onAttach' or text() = '.onLoad']]
    /following-sibling::expr//*[
      (self::SYMBOL or self::SYMBOL_FUNCTION_CALL)
      and (text() = 'require' or text() = 'library' or text() = 'installed.packages')
    ]
    "

    library_require_expr <- xml2::xml_find_all(xml, library_require_xpath)

    library_require_lints <- lapply(
      library_require_expr,
      function(expr) {
        bad_call <- xml2::xml_text(expr)
        hook <- get_hook(expr)
        if (bad_call == "installed.packages") {
          message <- sprintf("Don't slow down package load by running installed.packages() in %s().", hook)
        } else {
          message <- sprintf("Don't alter the search() path in %s() by calling %s().", hook, bad_call)
        }
        xml_nodes_to_lint(expr, source_file, message, type = "warning")
      }
    )

    # (4) .Last.lib() and .onDetach() shouldn't call library.dynam.unload()
    bad_unload_call_xpath <- "
      //expr[SYMBOL[text() = '.Last.lib' or text() = '.onDetach']]
      /following-sibling::expr[FUNCTION]
      //SYMBOL_FUNCTION_CALL[text() = 'library.dynam.unload']
    "

    bad_unload_call_expr <- xml2::xml_find_all(xml, bad_unload_call_xpath)

    bad_unload_call_lints <- lapply(
      bad_unload_call_expr,
      function(expr) {
        message <- sprintf("Use library.dynam.unload() calls in .onUnload(), not %s().", get_hook(expr))
        xml_nodes_to_lint(expr, source_file, message, type = "warning")
      }
    )

    # (5) .Last.lib() and .onDetach() should take one arguments with name matching ^lib
    unload_arg_name_xpath <- "
    //expr[SYMBOL[text() = '.onDetach' or text() = '.Last.lib']]
    /following-sibling::expr[
      FUNCTION
      and (
        count(SYMBOL_FORMALS) != 1
        or SYMBOL_FORMALS[not(starts-with(text(), 'lib'))]
      )
    ]
    "

    unload_arg_name_expr <- xml2::xml_find_all(xml, unload_arg_name_xpath)

    unload_arg_name_lints <- lapply(
      unload_arg_name_expr,
      function(expr) {
        message <- sprintf(
          "%s() should take one argument starting with 'lib'.",
          get_hook(expr)
        )
        xml_nodes_to_lint(expr, source_file, message, type = "warning")
      }
    )

    return(c(
      onload_bad_msg_call_lints,
      onattach_bad_msg_call_lints,
      load_arg_name_lints,
      library_require_lints,
      bad_unload_call_lints,
      unload_arg_name_lints
    ))
  })
}

make_bad_call_lint <- function(expr, source_file, hook) {
  call_name <- xml2::xml_text(expr)
  message <- switch(
    call_name,
    cat = ,
    message = ,
    print = ,
    writeLines = sprintf("Don't use %s() in %s().", call_name, hook),
    packageStartupMessage = "Put packageStartupMessage() calls in .onAttach(), not .onLoad().",
    library.dynam = "Put library.dynam() calls in .onLoad, not .onAttach()."
  )
  xml_nodes_to_lint(expr, source_file, message, type = "warning")
}
