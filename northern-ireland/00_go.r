stop("Don't actually source this script, that would be bad for business!")

# Extract ======================================================================

source("scripts/01a_get_cohort.r")
source("scripts/01b_get_death.r")
source("scripts/01c_get_vacc.r")
source("scripts/01d_get_hosp.r")

# Clean strict =================================================================

source("scripts/02a_clean_cohort.r")

# Analyse strict ===============================================================

source("scripts/03a_select_sample.r")
source("scripts/03b_make_survival_dataset.r")
source("scripts/03c_analysis_main.r")
source("scripts/03d_select_sample_dataflow.r")

# Clean broad  =================================================================

source("scripts/04a_clean_cohort_broad.r")

# Analyse broad ================================================================

source("scripts/04b_select_sample_broad.r")
source("scripts/04c_make_survival_dataset_broad.r")
source("scripts/04d_analysis_main_broad.r")

# Report =======================================================================

render(
    input = "scripts/99_report_analysis_strict.rmd",
    output_format = html_document(toc = TRUE),
    output_file = "../output/99_report_analysis_strict.html",
    quiet = TRUE
)

render(
  input = "scripts/99_report_analysis_broad.rmd",
  output_format = html_document(toc = TRUE),
  output_file = "../output/99_report_analysis_broad.html",
  quiet = TRUE
)

beep()
