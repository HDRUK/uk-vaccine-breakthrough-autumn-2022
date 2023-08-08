source("scripts/r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort  <- qread(("input/d_cohort_clean_broad.qs"))

#Sense check
d_cohort %>% filter(vacc_autm22_date>=useful_date$study_start) %>% group_by(vacc_autm22_name) %>% tally()

d_cohort %>% filter(vacc_autm22_date>=useful_date$study_start, vacc_autm22_name=="PB") %>%
  tally(!is.na(hosp_c19_date))


# select sample ================================================================
cat("select sample\n")

d_cohort <-
    d_cohort %>%
    mutate(
        # any outcome between vaccination and day 14?
        recent_c19_outcome = (vacc_autm22_date <= prior_c19_hosp_date & prior_c19_hosp_date < vacc_autm22_day14_date) |
            (vacc_autm22_date <= death_c19_date & death_c19_date < vacc_autm22_day14_date),
        recent_c19_outcome = replace_na(recent_c19_outcome, FALSE)
    ) %>%
    mutate(date_diff =
        # creating date_diff between doses 
        case_when(
            !is.na(vacc6_date) & !is.na(vacc5_date) ~ interval(vacc5_date, vacc6_date) / weeks(),
            !is.na(vacc5_date) & !is.na(vacc4_date) ~ interval(vacc4_date, vacc5_date) / weeks(),
            is.na(vacc5_date) & !is.na(vacc4_date) ~ interval(vacc3_date, vacc4_date) / weeks()
        )
    ) %>%
    mutate(
        # create categories for analysis
        dose_diff = ifelse(is.na(date_diff), NA,
                           ifelse(date_diff >= 24,"24+","<24"))) %>%
    mutate(
        is_c20                = !is.na(study_id),
        has_dob_sex           = !is.na(dob) & !is.na(sex),
        is_age_gte18          = age >= 18,
        has_lsoa              = not_na(health_board) & not_na(uprn) & not_na(hh_n_cat) & hh_n_cat!="unknown",
        has_autm_vacc         = !is.na(vacc_autm22_date) & vacc_autm22_name %in% c("PB", "MD"),
        has_follow_up         = vacc_autm22_day14_date < useful_date$study_end,
        is_alive              = (is.na(death_other_date) | death_other_date > vacc_autm22_day14_date) &
            (is.na(death_c19_date)   | death_c19_date   > vacc_autm22_day14_date),
        no_recent_c19_outcome = !recent_c19_outcome
    )

t_sample_selection <- tribble(
    ~step, ~criteria, ~n,
        1, "is in covid-19 cohort 2020",
            d_cohort %>% filter(is_c20) %>% nrow(),
        2, "has dob and sex recorded",
            d_cohort %>% filter(is_c20, has_dob_sex) %>% nrow(),
        3, "is aged 18yo or more at study start (1st sep 22)",
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18) %>% nrow(),
        4, "has lsoa (health board, uprn, hh number",
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18, has_lsoa) %>% nrow(),
        5, "has autumn covid vacc",
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18, has_lsoa, has_autm_vacc) %>% nrow(), #removed has_lsoa, has_sail_gp, has_good_vacc_record,
        6, "is autumn covid vacc 14 days before study end date",
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18, has_lsoa, has_autm_vacc, has_follow_up) %>% nrow(),
       7, "is alive at autumn covid vaccination", #and welsh resident
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18, has_lsoa, has_autm_vacc, has_follow_up, is_alive) %>% nrow(), #, is_welsh_resident
       8, "no covid19 hosp/death between vaccination and day14",
            d_cohort %>% filter(is_c20, has_dob_sex, is_age_gte18, has_lsoa, has_autm_vacc, has_follow_up, is_alive, no_recent_c19_outcome) %>% nrow()
    ) %>%
    mutate(
        n_diff = n - lag(n),
        p_diff = n / first(n) * 100,
        p_diff = round(p_diff, 1)
    )

print(as.data.frame(t_sample_selection))

d_sample <-
    d_cohort %>%
    filter(
        is_c20,
        has_dob_sex,
        is_age_gte18,
        has_lsoa,
        has_autm_vacc,
        has_follow_up,
        is_alive,
        no_recent_c19_outcome
    )


# save =========================================================================
cat("save\n")

qsave(
    t_sample_selection,
    file = "results/t_sample_selection_broad.qs"
)

qsave(
    d_sample,
    file = ("input/d_sample_broad.qs")
)

beep()
