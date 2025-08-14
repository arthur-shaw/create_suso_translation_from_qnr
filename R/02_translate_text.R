#' Translate Survey Solutions question text
#'
#' @description
#' Send the source text to OpenAI for translation.
#' 
#' Note: need developer account, credits, and an API key from OpenAI.
#' 
#' Note also: the OpenAI API key must be available as an environment variable
#' in the R session where this function is run.
#'
#' @param from_lang Character. Name of source language passed to the prompt.
#' @param to_lang Character. Name of target language passed to the prompt.
#' @param text Character. Text to be translated.
#' @param verbose Boolean. If `TRUE`, print original text and translation to
#' the console. Otherwise, do not echo these details to the console.
#'
#' @return Character. Translation of `text`.
#'
#' @importFrom ellmer chat_openai
#' @importFrom glue glue
translate_suso_qnr_text <- function(
  from_lang = "English",
  to_lang,
  text,
  verbose = FALSE
) {

  chat <- ellmer::chat_openai(
    system_prompt = glue::glue(
      "You are an expert survey translator.",
      "You will translate text from {from_lang} to {to_lang}",
      "However, do not translate words inside HTML tags (e.g. `font` in `<font>`)",
      "or text substitution markers (i.e., `some_text` between `%` like `%some_text%`).",
      "For `<font>` tags, be sure to place the same syntactic elements found in English",
      "inside of the font tag in the French translation.",
      "For sentence elements surrounded by text substitution markers (`%`),",
      "treat these as either nouns or a person's name.",
      "For all translations, please respect the case/capitalization style of the",
      "original text (e.g., ALL CAPS, lower case, Captial Case, etc.)",
      "In addition, respect grammatical/functional form of the source text",
      "(e.g., imperative, interogative, statement, sentence fragement, etc.)",
      "Return only the translated text.",
      .sep = " "
    ),
    echo = verbose
  ) |>
  # prevent `ellmer` from echoing to the console the model used
	suppressMessages() |>
	suppressWarnings()

  if (verbose == TRUE) {
    cat("Original text:\n")
    cat(text)
    cat("\n")
    cat("Translation:\n")
  }
  chat$chat(text)

}
