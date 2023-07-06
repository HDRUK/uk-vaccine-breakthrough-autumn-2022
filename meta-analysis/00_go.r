
setwd("~/Projects/DCP08 - NCSi2 vacc breakthrough/meta-analysis/")

source("clear_workspace.r")

source("01_strict_main_pool_desc.r")

source("02_strict_main_meta_coef.r")

source("03_strict_comorbidity_pool_desc.r")

source("04_strict_comorbidity_meta_coef.r")

source("05_strict_scotland_theraputics.r")

source("06_broad_main_pool_desc.r")

source("07_broad_main_meta_coef.r")

render("99_report.rmd", clean = TRUE)
