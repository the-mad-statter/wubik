globalVariables(
  c(
    "dbutils.fs.ls",
    "dbutils.fs.rm",
    "dbutils.fs.mkdirs",
    "dbutils.fs.cp",
    "dbutils.fs.put",
    "dbutils.secrets.list",
    "displayHTML"
  )
)

#' Pipe operator
#'
#' See `dplyr::[\%>\%][dplyr::reexports]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' .data pronoun
#'
#' See `rlang::[.data][rlang::.data]` for details.
#'
#' @name .data
#' @rdname dot-data
#' @keywords internal
#' @export
#' @importFrom rlang .data
#' @inherit rlang::.data description
NULL

#' .env pronoun
#'
#' See `rlang::[.env][rlang::.env]` for details.
#'
#' @name .env
#' @rdname dot-env
#' @keywords internal
#' @export
#' @importFrom rlang .env
#' @inherit rlang::.env description
NULL

#' := pronoun
#'
#' See `rlang::[:=][rlang:::=]` for details.
#'
#' @name :=
#' @rdname dyn-dots
#' @keywords internal
#' @export
#' @importFrom rlang :=
#' @inherit rlang:::= description
NULL

#' SQL Query
#'
#' See `SparkR::[sql][SparkR::sql]` for details.
#'
#' @name sql
#' @rdname sql
#' @keywords internal
#' @export
#' @importFrom SparkR sql
#' @inherit SparkR::sql description
NULL

#' collect: Collects all the elements of a SparkDataFrame and coerces...
#'
#' See `SparkR::[collect][SparkR::collect]` for details.
#'
#' @name collect
#' @rdname collect
#' @keywords internal
#' @export
#' @importFrom SparkR collect
#' @inherit SparkR::collect description
NULL

#' write_csv: Save a data frame to a csv file.
#'
#' See `readr::[write_csv][readr::write_csv]` for details.
#'
#' @name write_csv
#' @rdname write_csv
#' @keywords internal
#' @export
#' @importFrom readr write_csv
#' @inherit readr::write_csv description
NULL
