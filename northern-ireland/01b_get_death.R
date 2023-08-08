source("scripts/r_clear_workspace.r")

# Only run this if there is a raw data refresh

# load data ===================================================================
cat("get cohort\n")

dt_deaths_broad <- readRDS("../input_2023_03_21/deaths.rds") # this is the broader definition
dt_deaths_strict <- readRDS("../input_2023_03_21/deaths_strict.rds") # this is the strict definition 

# Create a single death data file =============================================

d_death = dt_deaths_broad %>%
  left_join(dt_deaths_strict, by="study_id") %>% collect() %>%
  select(study_id, date_of_death_rounded.x, covid_death.x, covid_death.y) %>%
  mutate(
    death_c19_underlying_flg = as.numeric(covid_death.y == "TRUE"),
    death_c19_underlying_flg = replace_na(death_c19_underlying_flg, 0),
    death_c19_secondary_flg  = as.numeric(covid_death.x == "TRUE")
  ) %>%
  select(
    study_id,
    death_date = date_of_death_rounded.x,
    death_c19_underlying_flg,
    death_c19_secondary_flg
  )

# d_death %>% group_by(death_c19_underlying_flg) %>% count()
# d_death %>% group_by(death_c19_secondary_flg) %>% count()

# is study_id unique?
if (n_distinct(d_death$study_id) != nrow(d_death))
  stop("study_id not unique")

# save ========================================================================
cat("save\n")

qsave(d_death, file = "input/d_death.qs")
