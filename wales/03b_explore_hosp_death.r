source("r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort <-
	qread(save_dir("d_cohort_clean.qs")) %>%
	filter(
		# aged 18 or older
		age >= 18,
		# has good vacc records
		(is.na(vacc_all_valid_flg) | vacc_all_valid_flg == 1),
		# is alive at autumn vaccination
		(is.na(death_other_date) | death_other_date > useful_date$vacc_autumn22_start),
		(is.na(death_c19_date)   | death_c19_date   > useful_date$vacc_autumn22_start),
		# is welsh resident at autumn vaccination
		wds_start_date <= useful_date$vacc_autumn22_start,
		(is.na(move_out_date) | move_out_date > useful_date$vacc_autumn22_start)
	) %>%
	# simplify age
	mutate(
		vacc_autm22_flg = as.numeric(!is.na(vacc_autm22_date)),
		vacc_autm22_day14_date = vacc_autm22_date + ddays(14),
		age_2cat = fct_collapse(age_3cat, "18_64" = c("18_49", "50_64"))
	) %>%
	select(
		alf_e,
		age_2cat,
		vacc_autm22_flg,
		vacc_autm22_day14_date,
		death_c19_date
	)

d_hosp <-
	qread(save_dir("d_hosp.qs")) %>%
	filter(useful_date$study_start <= admis_dt & admis_dt <= useful_date$study_end) %>%
	select(
		alf_e,
		admis_dt,
		admis_mthd_cat,
		disch_dt,
		ends_with("_flg"),
		admis_covid19_cat
	) %>%
	mutate(
		admis_covid19_cat = fct_explicit_na(admis_covid19_cat, "Other")
	)

# weekly hosp/death dose counts by age and vacc status =========================
cat("weekly hosp/death dose counts by age and vacc status\n")

d_c19_death <-
	d_cohort %>%
	filter(!is.na(death_c19_date)) %>%
	select(
		alf_e,
		age_2cat,
		vacc_autm22_flg,
		vacc_autm22_day14_date,
		event_date = death_c19_date
	) %>%
	mutate(event_cat = "c19_death")

d_c19_hosp <-
	d_cohort %>%
	inner_join(d_hosp, by = "alf_e") %>%
	filter(admis_covid19_cause_flg == 1) %>%
	select(
		alf_e,
		age_2cat,
		vacc_autm22_day14_date,
		event_date = admis_dt
	) %>%
	mutate(event_cat = "c19_hosp")

d_c19_hosp_death_week <-
	bind_rows(d_c19_death, d_c19_hosp) %>%
	# study window
	filter(event_date >= useful_date$study_start) %>%
	# vaccinated at event
	mutate(
		vacc_autm22_flg = as.numeric(vacc_autm22_day14_date <= event_date),
		vacc_autm22_flg = replace_na(vacc_autm22_flg, 0),
		vacc_autm22_cat = factor(vacc_autm22_flg, 0:1, c("no autm 22 vacc", "yes autm 22 vacc"))
	) %>%
	# weekly counts
	mutate(event_week = floor_date(event_date, "week"))

p_c19_hosp_death_week <-
	d_c19_hosp_death_week %>%
	count(age_2cat, vacc_autm22_cat, event_week) %>%
	# suppression
	mutate(n = if_else(1 <= n & n <= 9, as.integer(0), as.integer(n))) %>%
	mutate(n = round_half_up(n, -1)) %>%
	# plot
	ggplot(aes(x = event_week, y = n)) +
	facet_grid(vacc_autm22_cat ~ age_2cat) +
	geom_col() +
	scale_x_date(
		date_labels = "%b\n%Y",
		date_breaks = "1 month"
	) +
	scale_y_continuous(
		breaks = pretty_breaks()
	) +
	theme(
		panel.grid.minor.x = element_blank()
	)

print(p_c19_hosp_death_week)

# total counts =================================================================
cat("total counts\n")

t_c19_hosp_death_n <-
	d_c19_hosp_death_week %>%
	tabyl(age_2cat, vacc_autm22_cat, event_cat)

print(t_c19_hosp_death_n)

# save =========================================================================
cat("save\n")

qsavem(
	p_c19_hosp_death_week,
	t_c19_hosp_death_n,
	file = "results/explore_c19_hosp_death.qsm"
)

beep()
