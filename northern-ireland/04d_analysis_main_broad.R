source("scripts/r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_surv <- qread("input/d_surv_broad.qs")

# set list of covariates
x_covar <- c(
	"vacc_autm22_name",
	"dose_cat",
	"dose_diff",
	"dose_num",
	#"hosp_other_cat",
	"sex",
	#"age_3cat",
	"age_4y_cat",
	"BNF_group",
	"hh_n_cat",
	"nimdm",
	"urban_rural",
	"health_board"
)

# set reference categories =====================================================
cat("set reference categories\n")

d_surv <-
	d_surv %>%
	mutate(
		vacc_autm22_name = fct_relevel(vacc_autm22_name, "MD"),
		dose_cat         = fct_relevel(dose_cat,         "day14"),
		dose_diff         = fct_relevel(dose_diff,         "24+"),
		#hosp_other_cat   = fct_relevel(hosp_other_cat,   "no"),
		sex              = fct_relevel(sex,              "female"),
		age_4y_cat         = fct_relevel(age_4y_cat,         "18_49"),
		BNF_group     = fct_relevel(BNF_group,     "0"),
		hh_n_cat         = fct_relevel(hh_n_cat,         "02"),
		nimdm         = fct_relevel(nimdm,         "5_least"),
		urban_rural      = fct_relevel(urban_rural,      "urban"),
		health_board     = fct_relevel(health_board,     "N09000003")
	)

## Fix levels here for charts below
levels(d_surv$nimdm)
d_surv$nimdm = fct_relevel(d_surv$nimdm, c("5_least", "4", "3", "2", "1_most"))

# describe =====================================================================
cat("describe\n")

describe <- function(data, variable) {
	data %>%
	group_by_at(vars(one_of(variable))) %>%
	summarise(
		total_n = n_distinct(study_id),
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

hosp_death_overall_npr <-
	d_surv %>%
	lapply(c("total", x_covar), describe, data = .) %>%
	bind_rows()

hosp_death_md_npr <-
	d_surv %>%
	filter(vacc_autm22_name == "MD") %>%
	lapply(c("total", x_covar), describe, data = .) %>%
	bind_rows()

hosp_death_pb_npr <-
	d_surv %>%
	filter(vacc_autm22_name == "PB") %>%
	lapply(c("total", x_covar), describe, data = .) %>%
	bind_rows()

# check Viewer in RStudio
hosp_death_overall_npr %>%
kable(
	digits = 1,
	format.args = list(big.mark = ",")
) %>%
kable_styling(
	bootstrap_options = "striped",
	full_width = FALSE
) %>%
print()


# update covar list ============================================================
cat("update covar list\n")

# always exclude health board from list of covariates as its used to stratify
x_covar <- x_covar[x_covar != "health_board"]
x_covar <- x_covar[x_covar != "vacc_autm22_name"]

x_covar <- x_covar[x_covar != "dose_num"] #remove dose num 24 April

# fit unadjusted models ========================================================
cat("fit unadjusted models\n")

fit_coxph_udj <- function(vacc_name, x) {
	# subset and drop unused levels
	d_surv <- d_surv %>%
		filter(vacc_autm22_name %in% vacc_name) %>%
		mutate(across(where(is.factor), fct_drop))

	# make formula
	frml <- str_c("Surv(tstart, tstop, event_flg) ~ ", x)
	frml <- str_c(frml, " + strata(health_board) + strata(vacc_autm22_name)")
	frml <- as.formula(frml)

	# fit
	cph <- coxph(
		data     = d_surv,
		id       = study_id,
		formula  = frml,
		iter.max = 100
	)

	return(cph)
}

extract_coef <- function(cph) {
	expr_term <- str_c(x_covar, collapse = "|")
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

get_hr_udj <- function(vacc_name, x) {
	cph <- fit_coxph_udj(vacc_name, x)
	coef <- extract_coef(cph)
	coef <- coef %>% filter(xvar == x)
	return(coef)
}

cat("\toverall\n")
hosp_death_overall_coef_udj <-
	lapply(x_covar, get_hr_udj, vacc_name = c("PB", "MD")) %>%
	bind_rows()

cat("\tmd\n")
hosp_death_md_coef_udj <-
	lapply(x_covar, get_hr_udj, vacc_name = "MD") %>%
	bind_rows()

cat("\tpb\n")
hosp_death_pb_coef_udj <-
	lapply(x_covar, get_hr_udj, vacc_name = "PB") %>%
	bind_rows()


# fit adjusted models ==========================================================
cat("fit adjusted models\n")

fit_coxph <- function(vacc_name) {
	d_surv <- d_surv %>% filter(vacc_autm22_name %in% vacc_name)

	# make fully-adjusted forumula
	frml <- str_c(x_covar, collapse = " + ")
	frml <- str_c("Surv(tstart, tstop, event_flg) ~ ", frml)
	frml <- str_c(frml, " + strata(health_board) + strata(vacc_autm22_name)")
	frml <- as.formula(frml)

	# fit
	cph <- coxph(
		data     = d_surv,
		id       = study_id,
		formula  = frml,
		iter.max = 100
	)

	return(cph)
}

cat("\toverall\n")
hosp_death_overall_cph_adj  <- fit_coxph(c("PB", "MD"))
hosp_death_overall_coef_adj <- extract_coef(hosp_death_overall_cph_adj)
hosp_death_overall_vcov_adj <- vcov(hosp_death_overall_cph_adj)

cat("\tmd\n")
hosp_death_md_cph_adj  <- fit_coxph("MD")
hosp_death_md_coef_adj <- extract_coef(hosp_death_md_cph_adj)
hosp_death_md_vcov_adj <- vcov(hosp_death_md_cph_adj)

cat("\tpb\n")
hosp_death_pb_cph_adj  <- fit_coxph("PB")
hosp_death_pb_coef_adj <- extract_coef(hosp_death_pb_cph_adj)
hosp_death_pb_vcov_adj <- vcov(hosp_death_pb_cph_adj)


summary(hosp_death_overall_cph_adj)

# plot coef ====================================================================
cat("plot coef\n")

# make a mini data frame of values for the reference categories
# of each covariate
d_covar_ref <-
	tribble(
		~xvar,              ~xlbl,
		"dose_cat",          levels(d_surv$dose_cat)[1],
		"dose_diff",          levels(d_surv$dose_diff)[1],
		#"hosp_other_cat",    levels(d_surv$hosp_other_cat)[1],
		"sex",               levels(d_surv$sex)[1],
		"age_4y_cat",          levels(d_surv$age_4y_cat)[1],
		"BNF_group",          levels(d_surv$BNF_group)[1],
		"hh_n_cat",          levels(d_surv$hh_n_cat)[1],
		"nimdm",          levels(d_surv$nimdm)[1],
		"urban_rural",       levels(d_surv$urban_rural)[1]
	) %>%
	mutate(
		estimate  = 0,
		std.error = 0,
		conf.low  = 0,
		conf.high = 0,
		est_type  = "ref"
	) %>%
	# replicate reference categories for the three analyses
	full_join(data.frame(vacc_group = c("overall", "md", "pb")), by = character())

p_hosp_death_coef <-
	bind_rows(
		d_covar_ref,
		hosp_death_overall_coef_udj %>% mutate(vacc_group = "overall", est_type = "udj"),
		hosp_death_overall_coef_adj %>% mutate(vacc_group = "overall", est_type = "adj"),
		hosp_death_md_coef_udj      %>% mutate(vacc_group = "md",      est_type = "udj"),
		hosp_death_md_coef_adj      %>% mutate(vacc_group = "md",      est_type = "adj"),
		hosp_death_pb_coef_udj      %>% mutate(vacc_group = "pb",      est_type = "udj"),
		hosp_death_pb_coef_adj      %>% mutate(vacc_group = "pb",      est_type = "adj")
	) %>%
	# order covariates and labels
	mutate(
		vacc_group = factor(vacc_group, c("overall", "md", "pb")),
		xvar       = factor(xvar, x_covar),
		xlbl       = fct_inorder(xlbl),
		# # manually fix the order for some the levels
		xlbl = fct_relevel(xlbl, c(
		# household size
		  #"00",
		  "0",
			"01",
			"02",
			"03",
			"04",
			"05",
			"6+"
		# # nimdm
		# 	"5_least",
		# 	"4",
		# 	"3",
		# 	"2",
		# 	"1_most"
		))
	) %>%
	# plot it
	ggplot(aes(
		x      = exp(estimate),
		xmin   = exp(conf.low),
		xmax   = exp(conf.high),
		y      = xlbl,
		colour = est_type
	)) +
	facet_grid(xvar ~ vacc_group, space = "free", scales = "free", switch = "y") +
	geom_vline(xintercept = 1) +
	geom_pointrange(position = position_dodge(0.5)) +
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

print(p_hosp_death_coef)


# save =========================================================================
cat("save\n")

qsavem(
	x_covar,
	# descriptive tables
	hosp_death_overall_npr,
	hosp_death_md_npr,
	hosp_death_pb_npr,
	# model coefs
	hosp_death_overall_coef_udj,
	hosp_death_overall_coef_adj,
	hosp_death_md_coef_udj,
	hosp_death_md_coef_adj,
	hosp_death_pb_coef_udj,
	hosp_death_pb_coef_adj,
	# model var-covar matrices
	hosp_death_overall_vcov_adj,
	hosp_death_md_vcov_adj,
	hosp_death_pb_vcov_adj,
	# plot
	p_hosp_death_coef,
	file = "results/analyse_hosp_death_main_broad.qsm"
)


beep()
