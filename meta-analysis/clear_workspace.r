
# remove everything from the user environment ----------------------------------
rm(list = ls())
gc()

# load packages ----------------------------------------------------------------

pkg <- c(
  "assertr",
  "broom",
  "dplyr",
  "forcats",
  "ggplot2",
  "ggstance",
  "janitor",
  "knitr",
  "kableExtra",
  "metafor",
  "openxlsx",
  "qs",
  "patchwork",
  "readr",
  "readxl",
  "rmarkdown",
  "rvest",
  "scales",
  "stringr",
  "tidyr"
)

for (p in pkg) {
  suppressMessages(
    suppressPackageStartupMessages(
      library(p, character.only = TRUE)
    )
  )
}

rm(pkg, p)

# set options ------------------------------------------------------------------

options(
  readr.show_col_types   = FALSE,
  dplyr.summarise.inform = FALSE,
  knitr.kable.NA         = ''
)

# dimensions -------------------------------------------------------------------

plot_dim <- list(
  a4_width   = 5.2,
  a4_height  = 6.75,
  ppt_width  = 29.21 / 2.54,
  ppt_height = 16.14 / 2.54
)

plot_dpi <- 600

# common look ups --------------------------------------------------------------

lkp_country <- c(
  "meta"      = "#ad002a",
  "england"   = "#925e9f",
  "scotland"  = "#00468b",
  "wales"     = "#fc710a",
  "ni"        = "#42b540",
  "ref"       = "#000000"
)

# custom functions -------------------------------------------------------------

write_pretty_xlsx <- function(df, file) {
  
  # final xvar names
  lkp_pretty_xvar <- c(
    "total"                                    = "total",
    "Autumn 2022 vaccination"                  = "vacc_name",
    "Time post Autumn 2022 vaccination"        = "vacc_week",
    "Number of previous COVID-19 vaccinations" = "prv_dose_num",
    "Time since previous COVID-19 vaccination" = "prv_dose_diff",
    "Sex"                                      = "sex",
    "Age"                                      = "age",
    "Ethnicity*"                               = "ethnicity",
    "BMI*"                                     = "bmi",
    "Number of QCovid risk groups*"            = "qcovid_n",
    "Number of BNF risk groups§"               = "bnf_n",
    "Household size"                           = "household_n",
    "Socioeconomic deprivation quintile"       = "area_deprivation",
    "Rural/urban area classification"          = "urban_rural",
    "Theraputices before booster"              = "thera_before",
    "Theraputices after booster"               = "thera_after",
    "Type of treatment"                        = "thera_type"
  )
  
  # insert an xvar header row every time we change xvar
  which_row <- c(1, which(diff(as.numeric(factor(df$xvar))) != 0) + 1)
  
  which_xvar <- df$xvar[which_row]
  which_xvar <- factor(which_xvar, lkp_pretty_xvar, names(lkp_pretty_xvar))
  which_xvar <- as.character(which_xvar)
  
  for (i in length(which_row):1) {
    df <- df %>% add_row(xlbl = which_xvar[i], .before = which_row[i])
  }
  
  # start a workbook
  wb <- createWorkbook()
  addWorksheet(wb, sheet = "counts")
  writeData(wb, sheet = "counts", df)
  
  # make xvar bold and merge the row
  xvar_style <- createStyle(textDecoration = "bold")
  
  xvar_header_row <- which(df$xlbl %in% which_xvar) + 1
  
  addStyle(
    wb = wb,
    sheet = "counts",
    style = xvar_style,
    rows = xvar_header_row,
    cols = which(names(df) == "xlbl"),
    gridExpand = TRUE
  )
  
  which_cols <- c(
    "xlbl",
    # desc columns
    "ni_np",
    "ni_er",
    "scotland_np",
    "scotland_er",
    "wales_np",
    "wales_er",
    "pool_np",
    "pool_er",
    # coef columns
    "ni_hr_ci",
    "scotland_hr_ci",
    "wales_hr_ci",
    "meta_hr_ci"
  )
  
  which_cols <- which(names(df) %in% which_cols)
  
  for (xhr in xvar_header_row) {
    mergeCells(wb, sheet = "counts", cols = which_cols, rows = xhr)
  }
  
  # auto col width
  setColWidths(wb, sheet = "counts", cols = 1:ncol(df), widths = "auto")
  
  # save
  saveWorkbook(wb, file, overwrite = TRUE)
  
  # you deserve nothing
  invisible()
}