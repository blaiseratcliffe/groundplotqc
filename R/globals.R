# Column names that data.table code uses without quotes, declared so R CMD
# check doesn't report them as undefined globals (plan 17.8). None yet (D11.8):
# each name joins with the code that uses it.

#' @importFrom utils globalVariables
NULL

utils::globalVariables(character())
