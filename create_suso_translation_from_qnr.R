# ==============================================================================
# projet setup
# ==============================================================================

# ------------------------------------------------------------------------------
# load computational environment
# ------------------------------------------------------------------------------

renv::restore(prompt = FALSE)

# ------------------------------------------------------------------------------
# set paths
# ------------------------------------------------------------------------------

proj_dir <- here::here()
excel_dir <- here::here("data", "00_excel_qnr")
excel_file <- excel_dir |>
	fs::dir_ls(regexp = "\\.xlsx")

# ------------------------------------------------------------------------------
# load functions
# ------------------------------------------------------------------------------

script_paths <- proj_dir |>
  fs::path("R") |>
  fs::dir_ls(regexp = "\\.R")

purrr::walk(
  .x = script_paths,
  .f = ~ source(.x)
)

# ==============================================================================
# [01] extract questions from Excel questionnaire
# ==============================================================================

# provide specifications as a data frame
# - `sheet`. Name of the target sheet in Excel
# - `var_prefix`. Prefix of variables in the target section
# - `skip`. Number of rows to skip before question numbers are encountered.
sheet_specs <- tibble::tribble(
  ~ sheet, ~ var_prefix, ~ skip,
  "1. Membres du ménage", "01", 3,
  "2. Éducation", "02", 2,
  "3. Emploi", "03", 3,
  "4a. Filtre de garde d'enfants", "04a", 4,
  "4b. Modalités de garde d'enfant", "04b", 4,
  "4c. Satisfaction à l'égard de l", "04c", 3,
  "4d. Préférences en matière de g", "04d", 3,
  "4e. DCE", "04e", 3,
  "5a. Paiement pour la garde d'en", "5a", 3,
  "5a. Paiement pour la garde d'en", "5b", 4,
  "6a. Utilisation du temps", "6a", 5,
  "6b. Tâches dans le ménage", "6b", 3,
  "7a. Normes sociales - vignette", "7a", 5,
  "7b. Normes sociales", "07b", 4,
  "8a. Garde d'enfants 7-14 ans Fi", "08a", 5,
  "8b. École 7-14", "08b", 4,
  "8c. Modes de garde d'enfants 7-", "08c", 5,
  "8d. Préférences en matière de g", "08d", 4,
  "8e. Paiement pour la garde d'en", "08e", 3,
  "9. Tâches dans les ménages 7 à ", "09", 3,
  "10a. Logement, énergie et eau, ", "10a", 2,
  "10b. Biens durables", "10b", 2,
  "10c. Tâches à domicile (Autres ", "10c", 3,
  "11. Préférences du père", "11", 3,
  "12. L'utilisation du temps pour", "12", 5,
  "13a. Normes sociales - vignette", "13a", 4,
  "13b. Normes sociales pour les p", "13b", 4,
  "Module d'éducation des filles", "ac", 3,
  "Vignette 1", "CA", 2,
  "Vignette 2", "CB", 2,
  "Vignette 3", "CC", 2,
)

# create a data frame with variables names and question text
var_question_text_mapping <- sheet_specs |>
  # apply the question extraction function to each section in the specification
  # returns a list of data frames
  purrr::pmap(
    .f = ~ extract_questions(
      path = excel_file,
      sheet = ..1,
      skip = ..3,
      var_prefix = paste0("s", ..2, "q")
    )
  ) |>
  # "convert" the list of dfs into a single df by row-binding
	purrr::list_rbind() |>
	# change translated variable names back to English
  dplyr::mutate(
    variable = dplyr::if_else(
      condition = grepl(x = variable, pattern = "FILTRE"),
      true = sub(
        x = variable,
        pattern = "FILTRE",
        replacement = "FILTER",
        fixed = TRUE
      ),
      false = variable
    )
  )

# save to disk
saveRDS(
  object = var_question_text_mapping,
  file = fs::path(
    proj_dir, "data", "01_extracted_qnr_data",
    "var_question_text_mapping.rds"
  )
)

# ==============================================================================
# [02] populate SuSo translation with content extracted from questionnaire
# ==============================================================================


# ------------------------------------------------------------------------------
# ingest SuSo translation file
# ------------------------------------------------------------------------------

suso_translation_template_path <- fs::path(
  proj_dir, "inst",
  "{New translation}Childcare Situation Assessment - Demand Side Survey.xlsx"
)

suso_translation_template <- suso_translation_template_path |>
	readxl::read_excel()

# ------------------------------------------------------------------------------
# merge extracted translations into SuSo translation file
# ------------------------------------------------------------------------------

suso_template_w_fr <- suso_translation_template |>
	dplyr::left_join(
    var_question_text_mapping,
    by = c("Variable" = "variable")
  ) |>
	dplyr::mutate(
    Translation = dplyr::if_else(
      condition = !is.na(Variable) & Type == "Title",
      true = question,
      false = Translation
    )
  ) |>
	dplyr::select(-question)

# ------------------------------------------------------------------------------
# identify widow questions and orphan translations
# ------------------------------------------------------------------------------

widow_questions <- suso_template_w_fr <- suso_translation_template |>
	dplyr::anti_join(
    var_question_text_mapping,
    by = c("Variable" = "variable")
  ) |>
  dplyr::filter(!is.na(Variable) & Type == "Title")

orphan_translations <- var_question_text_mapping |>
  dplyr::anti_join(
    suso_template_w_fr,
    by = c( "variable" = "Variable")
  )

# ------------------------------------------------------------------------------
# save to disk
# ------------------------------------------------------------------------------

writexl::write_xlsx(
  x = suso_template_w_fr,
  path = fs::path(
    proj_dir, "data", "02_suso_translation",
    "{français}Childcare Situation Assessment - Demand Side Survey.xlsx"
  )
)

writexl::write_xlsx(
  x = widow_questions,
  path = fs::path(
    proj_dir, "data", "02_suso_translation",
    "widow_questions.xlsx"
  )
)

writexl::write_xlsx(
  x = orphan_translations,
  path = fs::path(
    proj_dir, "data", "02_suso_translation",
    "orphan_translations.xlsx"
  )
)
