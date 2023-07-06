cat("clear workspace\n")

# try to close any open connection =============================================

tryCatch(
    expr = {db2_close(con)},
    error = function(e) {invisible()}
)

tryCatch(
    expr = {db2_close(conn)},
    error = function(e) {invisible()}
)

# clear workspace ==============================================================

rm(list = ls())
gc()

# determine where we are =======================================================

sys_node <- unlist(Sys.info()["nodename"])

if (grepl("^jupyter-", sys_node)) {
    sys_env <- "jupyter"
} else if (grepl("^SAIL-", sys_node)) {
    sys_env <- "sail_desktop"
} else {
    sys_env <- "unknown"
}

# options ======================================================================

options(
    dplyr.summarise.inform = FALSE,
    readr.show_col_types = FALSE,
    lubridate.week.start = 1 # Monday
)

# cran mirror ==================================================================

# if we're on jupyterhub then update cran mirror
if (sys_env == "jupyter") {
    #r <- getOption("repos")
    r <- c("Nexus" = "http://192.168.7.56:8081/repository/r-proxy/")
    options(repos = r)
    rm(r)
}

# portable library =============================================================

# if we are using a portable version of R, use only the packages within its
# own library i.e. ignore user library
if (grepl(x = R.home(), pattern = "R-Portable")) {
    .libPaths(paste0(R.home(), "/library"))
}

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
    "sailr",
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

if (sys_env == "jupyter") {
    save_dir <- function(...) {
        str_c("~/dcp08-vaccine-breakthrough-winter22/", ...)
    }
} else if (sys_env == "sail_desktop") {
    save_dir <- function(...) {
        str_c("S:/1151 - Wales Multi-morbidity cohort (0911) - Census Data/Stu Bedston/dcp08-vaccine-breakthrough-winter22/", ...)
    }
}

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
    study_end           = ymd("2022-11-27")
)

# tidy up ======================================================================

rm(pkg, pkgs, sys_env, sys_node)
