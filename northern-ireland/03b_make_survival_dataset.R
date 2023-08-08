source("scripts/r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_sample <- qread("input/d_sample.qs")

x = d_sample %>% select(study_id, vacc4_date, vacc5_date,vacc6_date, dose_diff, date_diff) 

# count people who had a 4th dose during study period with no fifth, 4th and 5th, or 5th with 4th preceding
# study
x %>% filter(vacc4_date >= useful_date$study_start & is.na(vacc5_date))%>% count()
x %>% filter(vacc4_date >= useful_date$study_start & !is.na(vacc5_date))%>% count()
x %>% filter(vacc5_date >= useful_date$study_start & vacc4_date<useful_date$study_start) %>% count()  
x %>% filter(vacc6_date >= useful_date$study_start & !is.na(vacc5_date))%>% count()

# Add in a dose number variable 24 April 2023
d_sample = d_sample %>%
  mutate(dose_num =
           # creating date_diff between doses 
           case_when(
             !is.na(vacc6_date) ~ 6,
             is.na(vacc6_date) & !is.na(vacc5_date) ~ 5 ,
             is.na(vacc5_date) & !is.na(vacc4_date) ~ 4 ,
             is.na(vacc4_date) & !is.na(vacc3_date) ~ 3
           )
  ) 

class(d_sample$dose_num)

d_sample$dose_num = as.factor(d_sample$dose_num)

# %>% group_by(dose_num) %>% tally()

# only covariates fixed at baseline
x_covar <- c(
	"vacc_autm22_name",
	"sex",
	"age_3cat",
	"age_4y_cat",
	"BNF_group",
	"hh_n_cat",
	"nimdm",
	"urban_rural",
	"health_board",
	"dose_diff", #Added this following analyst meeting
	"dose_num" #added 24 April
)


# make survival dataset ========================================================
cat("make survival dataset\n")

# ignore events after study ends
d_sample <-
	d_sample %>%
	mutate(across(
		.cols = where(is.Date),
		.fns = ~ if_else(.x > useful_date$study_end, NA_Date_, .x)
	))

# start follow up 14 days after vaccination:
# 	* fourth or fifth dose after 1st Sept 2022

# stop follow up at outcome:
#	* c19 hosp
#	* c19 death

# stop follow up at censoring:
#   * other death
#   * move out of Wales - not applied
#   * end of study window

d_surv <-
	d_sample %>%
	# define when follow-up starts as well as subsequent vacc time cutpoints
	mutate(
		followup_start_date = vacc_autm22_date + ddays(14),
        dose_day28_date     = vacc_autm22_date + ddays(28),
        dose_day42_date     = vacc_autm22_date + ddays(42),
        dose_day56_date     = vacc_autm22_date + ddays(56),
		    dose_day70_date     = vacc_autm22_date + ddays(70),
	    	dose_day84_date     = vacc_autm22_date + ddays(84)
	) %>%
	# define when follow-up stops and why
	mutate(
		followup_end_date = pmin(
			# outcome
			death_c19_date,
			hosp_c19_date,
			# censor
			death_other_date,
			useful_date$study_end,
			na.rm = TRUE
		),
		event_cat = case_when(
			# outcome
			followup_end_date == death_c19_date        ~ "death_c19",
			followup_end_date == hosp_c19_date         ~ "hosp_c19",
			# censor
			followup_end_date == death_other_date      ~ "death_other",
			followup_end_date == useful_date$study_end ~ "study_end",
		)
	) %>%
	# prepare dates used for time-varying measures
	mutate(across(
		.cols = c("hosp_other_date", "dose_day28_date", "dose_day42_date", "dose_day56_date", "dose_day70_date",
		          "dose_day84_date"), # "antiviral_dose_date"
		.fns = ~ if_else(.x >= followup_end_date, NA_Date_, .x)
	)) %>%
	# keep only cols used to define start-stop dataset
	select(
		study_id,
		followup_start_date,
		followup_end_date,
		event_cat,
		hosp_other_at_start_flg,
		hosp_other_date,
		dose_day28_date,
		dose_day42_date,
		dose_day56_date,
		dose_day70_date,
		dose_day84_date
		# antiviral_dose_date
	) %>%
	# reshape the data frame so we can begin creating the start-stop dataset
	pivot_longer(
		cols           = c(hosp_other_date, dose_day28_date, dose_day42_date, dose_day56_date, 
		                   dose_day70_date,dose_day84_date,followup_end_date), # "antiviral_dose_date"
		names_to       = "tstop_reason",
		values_to      = "tstop_date",
		values_drop_na = TRUE
	) %>%
	# add person-row number (it is useful later)
	lazy_dt() %>%
	arrange(study_id, tstop_date) %>%
	group_by(study_id) %>%
	mutate(alf_row = row_number()) %>%
	ungroup() %>%
	as_tibble() %>%
	# tidy up tstop reason and create event flag
	mutate(
		event_cat    = if_else(tstop_reason != "followup_end_date", "(censored)", event_cat),
		tstop_reason = if_else(tstop_reason == "followup_end_date", event_cat, tstop_reason),
		tstop_reason = str_replace(tstop_reason, "_date", ""),
		event_flg    = as.numeric(event_cat %in% c("death_c19", "hosp_c19"))
	) %>%
	select(-event_cat) %>%
	# prepare time-varying measures
	mutate(
		hosp_other_cat = case_when(
			alf_row == 1 ~ hosp_other_at_start_flg,
			lag(tstop_reason) == "hosp_other" ~ 1
		),
		hosp_other_cat = factor(hosp_other_cat, 0:1, c("no", "yes")),
		dose_cat = case_when(
			alf_row == 1 ~ "day14",
			str_detect(lag(tstop_reason), "dose_day") ~ lag(tstop_reason)
		),
		dose_cat = str_replace(dose_cat, "dose_", ""),
		dose_cat = factor(dose_cat)
	) %>%
	# prepare time-varying antiviral measure
	# mutate()
	# complete time-varying measures
	fill(
		hosp_other_cat,
		dose_cat,
		.direction = "down"
	) %>%
	# create tstart and tstop
	mutate(
		followup_tstart = interval(useful_date$study_start, followup_start_date) / ddays(),
		tstop           = interval(useful_date$study_start, tstop_date) / ddays(),
		tstop           = if_else(tstop == followup_tstart, followup_tstart + 0.5, tstop),
		tstart          = if_else(alf_row == 1, followup_tstart, lag(tstop))
	) %>%
	select(
		study_id,
		alf_row,
		tstart,
		tstop,
		tstop_reason,
		event_flg,
		hosp_other_cat,
		dose_cat
		# antiviral_dose_cat
	)

# <!> new rows get added whenever there is a change in any of the time varying
# measures, if both happen on the same day, then we just want one new row
d_surv <-
	d_surv %>%
	filter(!( # NOT the following
		tstart > 0 &
		tstart == tstop &
		lead(tstart) < lead(tstop) &
		str_detect(tstop_reason, "(hosp_other|dose_day|antiviral)") &
		event_flg == 0
	))

# add in covariates
d_covar <-
	d_sample %>%
	select(any_of(c("study_id", x_covar))) %>%
	mutate(across(where(is.factor), fct_drop))

d_surv <-
	d_surv %>%
	left_join(d_covar, by = "study_id") %>%
	mutate(across(where(is.factor), fct_drop))

# sanity checks ================================================================
cat("sanity checks\n")

d_surv %>%
	verify(tstart >= 0) %>%
	verify(tstart < tstop) %>%
	# event flag is only 1 when covid death or hosp admis
	verify((event_flg == 1 &   tstop_reason %in% c("death_c19", "hosp_c19"))  | event_flg == 0) %>%
	# event flag is only 0 when not covid death or hosp admis
	verify((event_flg == 0 & !(tstop_reason %in% c("death_c19", "hosp_c19"))) | event_flg == 1) %>%
	assert(not_na, sex) %>%
	assert(not_na, age_4y_cat)

# check number of events
n_event_surv   <- d_surv %>% filter(event_flg == 1) %>% count() %>% pull(n)
n_event_sample <- d_sample %>% filter(!is.na(death_c19_date) | !is.na(hosp_c19_date)) %>% count() %>% pull(n)

if (n_event_surv != n_event_sample)
	stop("Number of events has changed between d_sample and d_surv")

# check for continuous follow-up time
d_check_dur <-
	d_surv %>%
	mutate(row_dur = tstop - tstart) %>%
	group_by(study_id) %>%
	summarise(
		tstart = min(tstart),
		tstop  = max(tstop),
		total_row_dur = sum(row_dur)
	) %>%
	ungroup() %>%
	mutate(total_dur = tstop - tstart)

if (any(d_check_dur$total_row_dur != d_check_dur$total_dur))
	stop("Gaps in follow-up found inside d_surv")

# save =========================================================================
cat("save\n")

qsave(
    d_surv,
    file = ("input/d_surv.qs")
)


beep()
