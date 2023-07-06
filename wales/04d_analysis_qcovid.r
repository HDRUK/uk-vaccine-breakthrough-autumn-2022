source("r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_surv <- qread(save_dir("d_surv.qs")) %>%
	mutate(across(
		.cols = starts_with("qc_"),
		.fns = factor
	))

# set list of qcovid indicators
x_qcovid <- c(
	"qc_82",
	"qc_leukolaba",
	"qc_prednisolone",
	"qc_af",
	"qc_ccf",
	"qc_asthma",
	"qc_bloodcancer",
	"qc_cerebralpalsy",
	"qc_chd",
	"qc_cirrhosis",
	"qc_congenheart",
	"qc_copd",
	"qc_dementia",
	"qc_epilepsy",
	"qc_fracture4",
	"qc_neurorare",
	"qc_parkinsons",
	"qc_pulmhyper",
	"qc_pulmrare",
	"qc_pvd",
	"qc_ra_sle",
	"qc_respcancer",
	"qc_semi",
	"qc_sicklecelldisease",
	"qc_stroke",
	"qc_diabetes_cat",		# has levels 0, 1, 2
	"qc_vte",
	"qc_chemo_cat",
	"qc_home_cat",
	"qc_learn_cat",			# has levels 0, 1, 2
	"qc_marrow6",
	"qc_radio6",
	"qc_solidtransplant",
	"qc_renal_cat"			# has levels 1 to 6
)

# set list of background covariates to control for
# note: no bmi, no qcovid count summary, no health board (strata)
x_covar <- c(
	"dose_cat",
	"hosp_other_cat",
	"sex",
	"age_3cat",
	"hh_n_cat",
	"wimd2019",
	"urban_rural"
)


# set reference categories =====================================================
cat("set reference categories\n")

d_surv <-
	d_surv %>%
	mutate(
		vacc_autm22_name = fct_relevel(vacc_autm22_name, "MD"),
		dose_cat         = fct_relevel(dose_cat,         "day14"),
		hosp_other_cat   = fct_relevel(hosp_other_cat,   "no"),
		sex              = fct_relevel(sex,              "female"),
		age_3cat         = fct_relevel(age_3cat,         "18_49"),
		bmi_cat          = fct_relevel(bmi_cat,          "25.0-29.9"),
		hh_n_cat         = fct_relevel(hh_n_cat,         "02"),
		wimd2019         = fct_relevel(wimd2019,         "5_least"),
		urban_rural      = fct_relevel(urban_rural,      "urban"),
		health_board     = fct_relevel(health_board,     "Aneurin Bevan University Health Board")
	)

# collapse ckd stage 5 =========================================================
cat("collapse ckd stage 5\n")

d_surv <-
	d_surv %>%
	mutate(
		qc_renal_cat = fct_collapse(qc_renal_cat, "4" = c("4", "5", "6"))
	)

# describe =====================================================================
cat("describe\n")

describe <- function(data, variable) {
	data %>%
	group_by_at(vars(one_of(variable))) %>%
	summarise(
		total_n = n_distinct(alf_e),
		events = sum(event_flg),
		pyears = sum(tstop - tstart) / 365.25
	) %>%
	ungroup() %>%
	mutate(
		xvar = variable,
		total_p = total_n / sum(total_n) * 100,
		rate = events / pyears * 1000
	) %>%
	select(
		xvar,
		xlbl = one_of(variable),
		total_n,
		total_p,
		pyears,
		events,
		rate
	)
}

# add dummy column "total" to allow easy total to be added to the table
d_surv <- d_surv %>% mutate(total = factor("total"))

hosp_death_qcovid_npr <-
	d_surv %>%
	lapply(c("total", x_qcovid), describe, data = .) %>%
	bind_rows()

# check Viewer in RStudio
hosp_death_qcovid_npr %>%
kable(
	digits = 1,
	format.args = list(big.mark = ",")
) %>%
kable_styling(
	bootstrap_options = "striped",
	full_width = FALSE
) %>%
print()


# fit unadjusted models ========================================================
cat("fit unadjusted models\n")

fit_coxph_udj <- function(x) {
	# make formula
	frml <- str_c("Surv(tstart, tstop, event_flg) ~ ", x)
	frml <- str_c(frml, " + strata(health_board) + strata(vacc_autm22_name)")
	frml <- as.formula(frml)

	# fit
	cph <- coxph(
		data     = d_surv,
		id       = alf_e,
		formula  = frml,
		iter.max = 100
	)

	return(cph)
}

extract_coef <- function(cph) {
	expr_term <- str_c(x_qcovid, collapse = "|")
	expr_term <- str_c("(", expr_term, ")(.*)")

	coef <-
		tidy(cph, conf.int = TRUE) %>%
		mutate(
			xvar = str_replace(term, expr_term, "\\1"),
			xlbl = str_replace(term, expr_term, "\\2"),
			.after = term
		) %>%
		select(-term)

	return(coef)
}

get_hr_udj <- function(x) {
	cat("\t", x, "\n")
	cph <- fit_coxph_udj(x)
	coef <- extract_coef(cph)
	coef <- coef %>% filter(xvar == x)
	return(coef)
}

hosp_death_qcovid_coef_udj <- lapply(x_qcovid, get_hr_udj) %>% bind_rows()


# model adjusted ===============================================================
cat("model adjusted\n")

fit_coxph_adj <- function(x) {
	# make background-adjusted forumula
	frml <- str_c(c(x, x_covar), collapse = " + ")
	frml <- str_c("Surv(tstart, tstop, event_flg) ~", frml)
	frml <- str_c(frml, " + strata(health_board) + strata(vacc_autm22_name)")
	frml <- as.formula(frml)

	# fit
	cph <- coxph(
		data     = d_surv,
		id       = alf_e,
		formula  = frml,
		iter.max = 100
	)

	return(cph)
}

get_hr_adj <- function(x) {
	cat("\t", x, "\n")
	cph <- fit_coxph_adj(x)
	coef <- extract_coef(cph)
	coef <- coef %>% filter(xvar == x)
	return(coef)
}

hosp_death_qcovid_coef_adj <-
	lapply(x_qcovid, get_hr_adj) %>%
	bind_rows()


# plot coef ====================================================================
cat("plot coef\n")

lkp_qcovid <- tribble(
	~xvar,                   ~xlbl, ~pretty_name,
	"qc_82",                   "1", "Immunosuppressants",
	"qc_leukolaba",            "1", "Anti-leukotriene or LABA",
	"qc_prednisolone",         "1", "Oral steroids",
	"qc_af",                   "1", "Atrial fibrillation",
	"qc_ccf",                  "1", "Heart failure",
	"qc_asthma",               "1", "Asthma",
	"qc_bloodcancer",          "1", "Blood or bone marrow cancer",
	"qc_cerebralpalsy",        "1", "Cerebral palsy",
	"qc_chd",                  "1", "Coronary heart disease",
	"qc_cirrhosis",            "1", "Liver cirrhosis",
	"qc_congenheart",          "1", "Congenital heart disease",
	"qc_copd",                 "1", "COPD",
	"qc_dementia",             "1", "Dementia",
	"qc_epilepsy",             "1", "Epilepsy",
	"qc_fracture4",            "1", "Prior fracture",
	"qc_neurorare",            "1", "Motor neurone disease +3",
	"qc_parkinsons",           "1", "Parkinson's disease",
	"qc_pulmhyper",            "1", "Pulmonary hypertension +1",
	"qc_pulmrare",             "1", "Cystic fibrosis +2",
	"qc_pvd",                  "1", "Peripheral vascular disease",
	"qc_ra_sle",               "1", "Rheumatoid arthritis or SLE",
	"qc_respcancer",           "1", "Lung or oral cancer",
	"qc_semi",                 "1", "Severe mental illness",
	"qc_sicklecelldisease",    "1", "Sickle cell disease +1",
	"qc_stroke",               "1", "Stroke or TIA",
	"qc_diabetes_cat",         "1", "Diabetes Type 1",
	"qc_diabetes_cat",         "2", "Diabetes Type 2",
	"qc_vte",                  "1", "Thrombosis or pulmonary embolus",
	"qc_chemo_cat",            "2", "Chemotherapy",
	"qc_home_cat",             "1", "Living in a care home",
	"qc_home_cat",             "2", "Homeless",
	"qc_learn_cat",            "1", "Learning disability",
	"qc_learn_cat",            "2", "Down's syndrome",
	"qc_marrow6",              "1", "Bone marrow or stem cell transplant",
	"qc_radio6",               "1", "Radiotherapy",
	"qc_solidtransplant",      "1", "Solid organ transplant",
	"qc_renal_cat",            "2", "CKD Stage 3",
	"qc_renal_cat",            "3", "CKD Stage 4",
	"qc_renal_cat",            "4", "CKD Stage 5",
	"qc_renal_cat",            "5", "CKD Stage 5 & dialysis",
	"qc_renal_cat",            "6", "CKD Stage 5 & transplant"
)

# find covars with totals Ns less than 10
small_qcovid <-
	hosp_death_qcovid_npr %>%
	filter(total_n < 10) %>%
	pull(xvar)

# plot unadjusted and adjusted coefs
d_hosp_death_qcovid_coef <-
	bind_rows(
		hosp_death_qcovid_coef_udj %>% mutate(est_type = "udj"),
		hosp_death_qcovid_coef_adj %>% mutate(est_type = "adj")
	) %>%
	# order the pretty labels
	left_join(lkp_qcovid, by = c("xvar", "xlbl")) %>%
	arrange(estimate, desc(est_type)) %>%
	mutate(pretty_name = fct_inorder(pretty_name)) %>%
	# suppress
	mutate(across(where(is.numeric), ~ if_else(xvar %in% small_qcovid, NA_real_, .x)))

p_hosp_death_qcovid_coef <-
	d_hosp_death_qcovid_coef %>%
	# plot
	ggplot(aes(
		x      = exp(estimate),
		xmin   = exp(conf.low),
		xmax   = exp(conf.high),
		y      = pretty_name,
		colour = est_type
	)) +
	geom_vline(xintercept = 1) +
	geom_pointrange(position = position_dodge(0.5)) +
	scale_x_continuous(
		breaks = pretty_breaks()
	) +
	scale_colour_manual(
		values = c(
			"ref" = "#444444",
			"udj" = "#d95f02",
			"adj" = "#1b9e77"
		)
	) +
	coord_cartesian(xlim = c(0, 20)) +
	theme(
		legend.position   = "top",
		axis.title.y      = element_blank(),
		strip.placement   = "outside",
		strip.text.y.left = element_text(angle = 0)
	)

print(p_hosp_death_qcovid_coef)


# save =========================================================================
cat("save\n")

qsavem(
	x_qcovid,
	x_covar,
	# descriptive tables
	hosp_death_qcovid_npr,
	# model coefs
	hosp_death_qcovid_coef_udj,
	hosp_death_qcovid_coef_adj,
	# plot
	d_hosp_death_qcovid_coef,
	p_hosp_death_qcovid_coef,
	file = "results/analyse_hosp_death_qcovid.qsm"
)

beep()
