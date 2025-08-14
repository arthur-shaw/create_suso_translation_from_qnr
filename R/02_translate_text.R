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

  # ============================================================================
  # provide context for each potential component of the prompt
  # ============================================================================

  core_context <- glue::glue(
    "You are an expert survey translator.",
    "You will translate text from {from_lang} to {to_lang}.",
    "For all translations, please respect the case/capitalization style of the",
    "original text (e.g., ALL CAPS, lower case, Capital Case, etc.)",
    "In addition, respect grammatical/functional form of the source text",
    "(e.g., imperative, interogative, statement, sentence fragement, etc.)",
    "Return only the translated text.",
    .sep = " "
  )
  html_context <- paste(
    "However, do not translate words inside HTML tags (e.g. `font` in `<font>`)",
    "or text substitution markers (i.e., `some_text` between `%` like `%some_text%`).",
    "For `<font>` tags, be sure to place the same syntactic elements found in English",
    "inside of the font tag in the French translation."
  )
  text_substition_context <- paste(
    "For sentence elements surrounded by text substitution markers (`%`),",
    "treat these as either nouns or a person's name."
  )
  household_context <- "Translate `household` as `ménage`"
  childcare_context <- "Translate `childcare` as `garde d'enfant`"
  interviewer_context <- "Translate `interviewer` as `enquêteur`"

  # ============================================================================
  # check for content
  # ============================================================================

  has_html <- grepl(x = text, pattern = "<.+>")
  has_text_substitution = grepl(x = text, pattern = "%.+%")
  has_household <- grepl(x = text, pattern = "household", ignore.case = TRUE)
  has_childcare <- grepl(x = text, pattern = "childcare", ignore.case = TRUE)
  has_interviewer <- grepl(x = text, pattern = "interviewer", ignore.case = TRUE)
  
  # ============================================================================
  # construct system prompt based on content of text to be translated
  # ============================================================================

  custom_sys_prompt <- glue::glue(
    core_context,
    "{if (has_html) html_context else ''}",
    "{if (has_text_substitution) text_substition_context else ''}",
    "{if (has_household) household_context else ''}",
    "{if (has_childcare) childcare_context else ''}",
    "if (has_interviewer) interviewer_context else ''",
    .sep = " "
  )

  chat <- ellmer::chat_openai(
    system_prompt = custom_sys_prompt,
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
