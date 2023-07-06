source("r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort  <- qread(save_dir("d_cohort_clean.qs"))
d_bmi_imp <- qread(save_dir("d_bmi_imp.qs"))


# join bmi imp =================================================================
cat("join bmi imp\n")

d_cohort <-
    d_cohort %>%
    left_join(d_bmi_imp, by = "alf_e") %>%
    mutate(
        bmi_cat = fct_explicit_na(bmi_cat, "unknown")
    )


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
    mutate(
        is_c20                = !is.na(alf_e),
        has_wob_sex           = !is.na(wob) & !is.na(sex),
        is_age_gte18          = age >= 18,
        has_lsoa              = not_na(ralf_e) & not_na(lsoa2011_cd),
        has_sail_gp           = gp_end_date > useful_date$vacc_start & qc_flg == 1,
        has_good_vacc_record  = vacc_all_valid_flg == 1 | is.na(vacc_all_valid_flg),
        has_autm_vacc         = !is.na(vacc_autm22_date) & vacc_autm22_name %in% c("PB", "MD"),
        has_follow_up         = vacc_autm22_day14_date < useful_date$study_end,
        is_alive              = (is.na(death_other_date) | death_other_date > vacc_autm22_day14_date) &
                                (is.na(death_c19_date)   | death_c19_date   > vacc_autm22_day14_date),
        is_welsh_resident     = wds_start_date <= ymd("2020-01-01") &
                               (is.na(move_out_date) | move_out_date > vacc_autm22_day14_date),
        no_recent_c19_outcome = !recent_c19_outcome
    )

t_sample_selection <- tribble(
    ~step, ~criteria, ~n,
        1, "is in covid-19 cohort 2020",
            d_cohort %>% filter(is_c20) %>% nrow(),
        2, "has wob and sex recorded",
            d_cohort %>% filter(is_c20, has_wob_sex) %>% nrow(),
        3, "is aged 18yo or more at study start (1st sep 22)",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18) %>% nrow(),
        4, "has lsoa",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa) %>% nrow(),
        5, "is registered with sail gp at 8 dec 2020",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp) %>% nrow(),
        6, "has good vacc records",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp, has_good_vacc_record) %>% nrow(),
        7, "has autumn covid vacc",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp, has_good_vacc_record, has_autm_vacc) %>% nrow(),
        9, "is autumn covid vacc 14 days before study end date",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp, has_good_vacc_record, has_autm_vacc, has_follow_up) %>% nrow(),
       10, "is alive and welsh resident at autumn covid vaccination",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp, has_good_vacc_record, has_autm_vacc, has_follow_up, is_alive, is_welsh_resident) %>% nrow(),
       11, "no covid19 hosp/death between vaccination and day14",
            d_cohort %>% filter(is_c20, has_wob_sex, is_age_gte18, has_lsoa, has_sail_gp, has_good_vacc_record, has_autm_vacc, has_follow_up, is_alive, is_welsh_resident, no_recent_c19_outcome) %>% nrow()
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
        has_wob_sex,
        is_age_gte18,
        has_lsoa,
        has_sail_gp,
        has_good_vacc_record,
        has_autm_vacc,
        has_follow_up,
        is_alive,
        is_welsh_resident,
        no_recent_c19_outcome
    )


# save =========================================================================
cat("save\n")

qsave(
    t_sample_selection,
    file = "results/t_sample_selection.qs"
)

qsave(
    d_sample,
    file = save_dir("d_sample.qs")
)

beep()
