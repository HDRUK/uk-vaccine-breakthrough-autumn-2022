cat("clear workspace\n")

# clear workspace ==============================================================

rm(list = ls())
gc()
memory.limit(64000)

# options ======================================================================

options(
    dplyr.summarise.inform = FALSE,
    readr.show_col_types = FALSE,
    lubridate.week.start = 1 # Monday
)


# load packages ================================================================

pkgs <- c(
    # load first so they dont take over other packages
    "purrr",
    # alphabetical
    "assertr",
    "beepr",
    "broom",
    "dplyr",
    "dtplyr",
    "forcats",
    "ggplot2",
    "ggstance",
    "knitr",
    "kableExtra",
    "mice",
    "janitor",
    "lubridate",
    "patchwork",
    "qs",
    "rlang",
    "rmarkdown",
    #"sailr",
    "scales",
    "stringr",
    "readr",
    "survival",
    "tibble",
    "tidyr",
    "finalfit",
    "tidyverse"
)

for (pkg in pkgs) {
    suppressWarnings(
        suppressPackageStartupMessages(
            library(pkg, character.only = TRUE)
        )
    )
}

# custom functions =============================================================

quotemeta <- function(string) {
    str_replace_all(string, "(\\W)", "\\\\\\1")
}

suppress_n <- function(x) {
    y <- if_else(1 <= x & x <= 9, NA_integer_, as.integer(x))
    y <- janitor::round_half_up(y, -1)
    return(y)
}

kable_pretty <- function(
    kable_input,
    bootstrap_options = "striped",
    full_width = FALSE,
    position = "center",
    ...
) {
    kable_styling(
        kable_input = kable_input,
        bootstrap_options = bootstrap_options,
        full_width = full_width,
        position = position,
        ...
    )
}

# plot dimensions ==============================================================

p_width  <- 5.20
p_height <- 8.75

# useful study dates ===========================================================

useful_date <- list(
    vacc_start          = ymd("2020-12-08"),
    vacc_booster_start  = ymd("2021-09-20"),
    vacc_autumn22_start = ymd("2022-09-01"),
    study_start         = ymd("2022-09-01"),
    study_end           = ymd("2022-12-31")
)

# tidy up ======================================================================

rm(pkg, pkgs)
