source("scripts/r_clear_workspace.r")

# Only run this if there is a raw data refresh

# load data ===================================================================
cat("get cohort\n")

dt_demographics <- readRDS("../input_2023_01_20/clean_2023-02-09/demographics.rds")
dt_epd <- readRDS("../input_2023_01_20/clean_2023-02-09/epd_breakthrough2.rds")

## Replace the HH with 54432 people with NA
is.na(dt_demographics$household_size) = dt_demographics$household_size=="54432" 
# x = dt_demographics %>% group_by(household_size) %>% tally()

# clean epd chapters ===========================================================

d_epd = dt_epd %>%
  select(study_id, BNF_chapters) %>% collect()

d_epd = d_epd %>%
  mutate(BNF_group = cut(BNF_chapters,
                         breaks = c(0,1,2,3,4,5,6, Inf),
                         labels = c("0","01","02","03","04","05", "6+"),
                         right = FALSE
  )
  ) %>% select(study_id, BNF_group)


d_cohort = dt_demographics %>%
  left_join(d_epd, by="study_id") %>% collect()


# save =========================================================================
cat("save\n")

qsave(d_cohort, file = "input/d_cohort.qs")




