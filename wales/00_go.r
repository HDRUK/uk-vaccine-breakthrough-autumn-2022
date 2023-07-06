stop("Don't actually source this script, that would be bad for business!")

# ToDo =========================================================================

# Analysis:
# [ ] Update QCOVID measures to be May 2022

# Extract ======================================================================

source("01a_get_cohort.r")
source("01b_get_residence.r")
source("01c_get_death.r")
source("01d_get_vacc.r")
source("01e_get_hosp.r")

# Clean ========================================================================

source("02a_clean_cohort.r")
source("02b_impute_bmi.r")

# Explore ======================================================================

source("03a_explore_vacc.r")
source("03b_explore_hosp_death.r")

# Analyse ======================================================================

source("04a_select_sample.r")
source("04b_make_survival_dataset.r")
source("04c_analysis_main.r")
source("04d_analysis_qcovid.r")

# Report =======================================================================

render(
    input = "99_report_analysis.rmd",
    output_format = html_document(toc = TRUE),
    output_file = "request-out/99_report_analysis.html",
    quiet = TRUE
)
beep()
