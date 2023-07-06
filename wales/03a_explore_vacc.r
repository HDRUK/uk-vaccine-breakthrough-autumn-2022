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
		# is alive at vacc start
		(is.na(death_other_date) | death_other_date > useful_date$vacc_start),
		(is.na(death_c19_date)   | death_c19_date   > useful_date$vacc_start),
		# is welsh resident at vacc start
        wds_start_date <= ymd("2020-01-01"),
		(is.na(move_out_date) | move_out_date > useful_date$vacc_start)
	)


# weekly vaccine dose counts by age ============================================
cat("weekly vaccine dose counts by age\n")

x_weeks <- seq(
	from = floor_date(useful_date$vacc_start, "week"),
	to   = floor_date(useful_date$study_end, "week"),
	by   = "1 week"
)

d_dummy_weeks <- expand_grid(
	age_3cat  = sort(unique(d_cohort$age_3cat)),
	dose_num  = c("dose1", "dose2", "dose3", "dose4", "dose5"),
	dose_week = x_weeks
)

d_vacc_week <-
	d_cohort %>%
	select(
		age_3cat,
		dose1 = vacc1_date,
		dose2 = vacc2_date,
		dose3 = vacc3_date,
		dose4 = vacc4_date,
		dose5 = vacc5_date,
	) %>%
	# remove events after study window
	mutate(across(
		.cols = where(is.Date),
		.fns  = ~ if_else(.x <= useful_date$study_end, .x, NA_Date_)
	)) %>%
	pivot_longer(
		cols           = matches("dose"),
		names_to       = "dose_num",
		values_to      = "dose_date",
		values_drop_na = TRUE
	) %>%
	mutate(dose_week = floor_date(dose_date, "week")) %>%
	filter(dose_week <= useful_date$study_end) %>%
	count(age_3cat, dose_num, dose_week) %>%
	full_join(d_dummy_weeks, by = c("age_3cat", "dose_num", "dose_week")) %>%
	mutate(n = replace_na(n, as.integer(0))) %>%
	arrange(age_3cat, dose_num, dose_week)

p_vacc_week <-
	d_vacc_week %>%
	# suppression
	mutate(n = if_else(1 <= n & n <= 9, as.integer(10), as.integer(n))) %>%
	mutate(n = round_half_up(n, -1)) %>%
	# plot
	ggplot(aes(x = dose_week, y = n, colour = dose_num)) +
	facet_wrap(~ age_3cat, ncol = 1, scales = "free_y") +
	geom_line() +
	scale_x_date(
		date_labels = "%b\n%Y",
		date_breaks = "2 months"
	) +
	scale_y_continuous(
		labels = comma
	) +
	scale_colour_brewer(
		palette = "Set1"
	) +
	theme(
		legend.position = "top",
		axis.title.x = element_blank()
	)

print(p_vacc_week)


# zoom in on dose 4 and 5 ======================================================
cat("zoom in on dose 4 and 5\n")

d_vacc45_day <-
	d_cohort %>%
	select(
		age_3cat,
		dose4 = vacc4_date,
		dose5 = vacc5_date,
	) %>%
	pivot_longer(
		cols           = matches("dose"),
		names_to       = "dose_num",
		values_to      = "dose_date",
		values_drop_na = TRUE
	) %>%
	filter(
		dose_date >= ymd("2022-08-29"),
		dose_date <= useful_date$study_end
	) %>%
	count(dose_date, age_3cat, dose_num)

p_vacc45_day <-
	d_vacc45_day %>%
	# suppression
	mutate(n = if_else(1 <= n & n <= 9, as.integer(10), as.integer(n))) %>%
	mutate(n = round_half_up(n, -1)) %>%
	# plot
	ggplot(aes(x = dose_date, y = n, fill = dose_num)) +
	facet_wrap(~ age_3cat, ncol = 1) +
	geom_col() +
	scale_x_date(
		date_labels = "%d %b\n%Y",
		date_breaks = "week"
	) +
	scale_y_continuous(
		name = "Daily counts",
		labels = comma
	) +
	scale_fill_brewer(
		palette = "Set1"
	) +
	theme(
		legend.position = "top",
		axis.title.x = element_blank()
	)

print(p_vacc45_day)

# cumulative incidence of uptake ===============================================
cat("cumulative incidence of uptake\n")

lkp_state <- c(
	"(censored)",
	"unvac",
	"dose1",
	"dose2",
	"dose3",
	"dose4",
	"dose5",
	"death",
	"move_out",
	"study_end"
)

# make multi-state date set for dose
d_mstate_vacc <-
	d_cohort %>%
	mutate(
		death     = pmin(death_other_date, death_c19_date, na.rm = TRUE),
		study_end = useful_date$study_end
	) %>%
	select(
		alf_e,
		age_3cat,
		dose1    = vacc1_date,
		dose2    = vacc2_date,
		dose3    = vacc3_date,
		dose4    = vacc4_date,
		dose5    = vacc5_date,
		move_out = move_out_date,
		death,
		study_end
	) %>%
	# remove events after someone has either moved out, died, or reached the
	# end of the study window
	mutate(
		end_follow_up = pmin(move_out, death, study_end, na.rm = TRUE),
		end_follow_up = as.Date(end_follow_up),
		end_follow_up_cat = case_when(
			end_follow_up == death     ~ "death",
			end_follow_up == move_out  ~ "(censored)",
			end_follow_up == study_end ~ "(censored)"
		)
	) %>%
	select(
		-death,
		-move_out,
		-study_end
	) %>%
	mutate(across(
		.cols = starts_with("dose"),
		.fns  = ~ if_else(.x >= end_follow_up, NA_Date_, .x)
	)) %>%
	# make event-list
	pivot_longer(
		cols           = c(-alf_e, -age_3cat, -end_follow_up_cat),
		names_to       = "event_name",
		values_to      = "event_date",
		values_drop_na = TRUE
	) %>%
	# add row number for alf
	lazy_dt() %>%
	arrange(alf_e, event_date) %>%
	group_by(alf_e) %>%
	mutate(alf_seq = row_number()) %>%
	as_tibble() %>%
	# define survival columns
	mutate(
		event_name = if_else(event_name == "end_follow_up", end_follow_up_cat, event_name)
	) %>%
	mutate(
		tstart     = if_else(alf_seq == 1, useful_date$vacc_start - ddays(1), lag(event_date)),
		tstart     = interval(useful_date$vacc_start - ddays(1), tstart) / ddays(),
		state_from = if_else(alf_seq == 1, "unvac", lag(event_name)),
		tstop      = event_date,
		tstop      = interval(as_datetime(useful_date$vacc_start) - ddays(1), tstop) / ddays(),
		state_to   = event_name
	) %>%
	select(
		-end_follow_up_cat,
		-event_name,
		-event_date
	) %>%
	# any same day events just add half a day
	mutate(
		tstop = if_else(tstart == tstop, tstop + 0.5, tstop)
	) %>%
	# finalise categories
	mutate(
		state_from = factor(state_from, lkp_state) %>% fct_drop(),
		state_to   = factor(state_to,   lkp_state) %>% fct_drop()
	)

# checking =====================================================================
cat("checking: ")

d_mstate_vacc %>%
	verify(tstart >= 0) %>%
	verify(tstart < tstop) %>%
	verify(alf_seq == 1 | tstart == lag(tstop))

cat("passed\n")

# explore ======================================================================
cat("explore\n")

t_vacc_state_n <- d_mstate_vacc %>% tabyl(state_from, state_to)

print(t_vacc_state_n)


# survfit ======================================================================
cat("survfit\n")

mstate_vacc <- survfit(
	formula = Surv(tstart, tstop, state_to) ~ age_3cat,
	data = d_mstate_vacc,
	id = alf_e
)

p_vacc_state <-
	mstate_vacc %>%
	tidy() %>%
	# only keep what we're gonna plot
	select(
		time,
		estimate,
		state,
		strata
	) %>%
	# add dummy rows so all panels are "square"
	bind_rows(
		tribble(
			~time, ~estimate, ~state,          ~strata,
				0,         1, "(s0)", "age_3cat=18_49",
				0,         1, "(s0)", "age_3cat=50_64"
		)
	) %>%
	mutate(
		time = useful_date$vacc_start + ddays(time),
		strata = strata %>%
			str_replace("age_3cat=", "") %>%
			factor(),
		state = state %>%
			factor() %>%
			fct_recode("unvacc" = "(s0)") %>%
			fct_relevel("death")
	) %>%
	ggplot(aes(
		x = time,
		y = estimate,
		fill = state
	)) +
	facet_wrap(~ strata, ncol = 1) +
	geom_area() +
	# add marker for study start and end dates
	geom_vline(
		xintercept = useful_date$study_start,
		linetype = 2
	) +
	scale_x_date(
		date_labels = "%b\n%Y",
		date_breaks = "2 months"
	) +
	scale_fill_brewer(
		palette = "Set1",
		guide = guide_legend(nrow = 2, byrow = TRUE)
	) +
	theme(
		legend.position = "top",
		axis.title.x = element_blank()
	)

print(p_vacc_state)

# save =========================================================================
cat("save\n")

qsavem(
	p_vacc_week,
	p_vacc45_day,
	p_vacc_state,
	t_vacc_state_n,
	file = "results/explore_vacc.qsm"
)

beep()
