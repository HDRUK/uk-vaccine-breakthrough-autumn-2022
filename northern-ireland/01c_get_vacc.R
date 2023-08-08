source("scripts/r_clear_workspace.r")

# Only run this if there is a raw data refresh

# load data ===================================================================
cat("get cohort\n")

dt_covid_vaccines_long <- readRDS("../input_2023_01_20/clean_2023-02-09/covid_vaccines_long_clean.rds")

d_vacc = dt_covid_vaccines_long %>%
  select(study_id, vacc_dose_num = dose_number_all, dose_number, campaign, vacc_name = vaccine_name,
         vacc_date = date_vaccination, time_from_last_dose) %>% collect()

d_vacc %>% group_by(vacc_name) %>% tally()

#Check vacc names after study start date

# x = d_vacc %>%
#   filter(vacc_date >= useful_date$study_start) %>%
#   group_by(vacc_name) %>% tally()

lkp_vacc_name <- c(
  "AZ" = "AZ",
  "PB" = "BNT162b2",
  "PB" = "Bivalent",
  "MD" = "Moderna",
  "Novavax" = "Novavax",
  "Novavax" = "Nuvaxovid"
)

d_vacc = d_vacc %>%
  mutate(
    vacc_name  = factor(vacc_name, lkp_vacc_name, names(lkp_vacc_name))
  )

# # Looking at the use of campaign and dose_number
# test = d_vacc %>%
#   filter(campaign=="Autumn2022Booster") %>%
#   group_by(vacc_dose_num, dose_number) %>% tally()
# 
# # Create a time since last dose 
# Removed this as it's in the select sample code now
# d_vacc = d_vacc %>%
#   mutate(t_vacc = as.numeric(time_from_last_dose, units="weeks")) %>%
#   mutate(
#     # time since last dose
#     dose_diff = ifelse(is.na(t_vacc), NA,
#                          ifelse(t_vacc >= 24,"24+","<24")))
# 
# #check that it all worked OK
# # d_vacc %>% filter(vacc_date >= useful_date$study_start) %>% group_by(vacc_dose_num, dose_diff) %>% tally()

# save =========================================================================
cat("save\n")

qsave(d_vacc, file = "input/d_vacc.qs")
