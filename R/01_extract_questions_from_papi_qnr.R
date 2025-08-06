#' Extract questions text from a target section of an Excel questionnaire
#'
#' @param path Character. Path to the Excel questionnaire.
#' @param sheet Character. Name of the target sheet in Excel.
#' @param skip Numeric. Number of rows to skip before question numbers are
#' encountered.
#' @param var_prefix Character. Prefix for transforming FILTER and question
#' numbers into variable names (e.g. `FILTER` -> `s03_FILTER2`,
#' `2` -> `s03q02`)
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select starts_with matches rename_with case_when filter
#' everything
#' @importFrom stringr str_pad str_extract
#' @importFrom tidyr pivot_longer
extract_questions <- function(
  path,
  sheet,
  skip,
  var_prefix
) {

  qnr_as_df <- readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = skip,
    col_names = TRUE,
    col_types = "text"
  )

  vars_and_question_text_df <- qnr_as_df |>
    dplyr::select(
      -dplyr::starts_with("..."),
      - dplyr::matches("INSTRUCTION")
    ) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ dplyr::case_when(
        # filter variables
        grepl(x = .x, pattern = "FILTRE") ~
          paste0(var_prefix, "_", .x),
        # all questions
        grepl(x = .x, pattern = "^[0-9]+") ~
          paste0(
            var_prefix,
            stringr::str_pad(
              string = .x,
              width = 2, side = "left", pad = "0"
            )
          ),
        # anything else
        .default = .x
      )
    ) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "question"
    ) |>
    # pre-pend variable number to question text
    dplyr::mutate(
      question_number = dplyr::if_else(
        condition = grepl(
          x = variable,
          pattern = "s[0-9]+"
        ),
        true = stringr::str_extract(
          string = variable,
          pattern = "(?<=q)([0-9]+)"
        ) |>
          as.numeric() |>
          as.character(),
        false = NA_character_
      ),
      question = dplyr::if_else(
        condition = !is.na(question_number),
        true = paste0(question_number, ". ", question),
        false = question
      )
    ) |>
    dplyr::select(-question_number)

}
