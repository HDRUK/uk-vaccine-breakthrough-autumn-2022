source("r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort <- qread(save_dir("d_cohort_clean.qs"))


# impute bmi ===================================================================
cat("impute bmi\n")

d_bmi <-
    d_cohort %>%
    filter(
        age >= 18,
        gp_end_date > useful_date$vacc_start,
        qc_flg == 1,
        not_na(lsoa2011_cd)
    ) %>%
    mutate(bmi_log = log(bmi)) %>%
    mutate(
        vacc1_flg = as.numeric(!is.na(vacc1_date)),
        vacc2_flg = as.numeric(!is.na(vacc2_date)),
        vacc3_flg = as.numeric(!is.na(vacc3_date)),
        vacc4_flg = as.numeric(!is.na(vacc4_date)),
        vacc5_flg = as.numeric(!is.na(vacc5_date)),
        c19_hosp_death_flg = as.numeric(!is.na(hosp_c19_date) | !is.na(death_c19_date))
    )

# create predictor matrix
pred.mat <- matrix(
    data = 0,
    nrow = ncol(d_bmi),
    ncol = ncol(d_bmi),
    dimnames = list(
        names(d_bmi),
        names(d_bmi)
    )
)

# use the following measures to impute bmi
bmi_xvar <- c(
    # demographics
    "age_5y_cat",
    "sex",
    "ethn_cat",
    # residence
    "hh_n_cat",
    "wimd2019",
    "urban_rural",
    "health_board",
    # vaccination
    "vacc1_flg",
    "vacc2_flg",
    "vacc3_flg",
    "vacc4_flg",
    "vacc5_flg",
    # c19 hosp/death
    "prior_c19_hosp_flg",
    "c19_hosp_death_flg",
    # qcovid
    "qc_asthma",
    "qc_semi",
    "qc_diabetes_cat",
    # health care utilisation
    "hcu_hosp_spell_cat",
    "hcu_gp_att_cat",
    "hcu_gp_prescription_cat"
)

# check all xvars are present
if (!all(bmi_xvar %in% colnames(pred.mat)))
    stop("value in bmi_xvar not in pred.mat")

pred.mat[
    rownames(pred.mat) == "bmi_log",
    colnames(pred.mat) %in% bmi_xvar
] <- 1


# method, choose from: sample, norm, norm.boot, rf
mice.method <- rep("", ncol(d_bmi))
mice.method[rownames(pred.mat) == "bmi_log"] <- "norm"

# impute
imp_bmi <- mice(
    data            = d_bmi,
    m               = 1,
    predictorMatrix = pred.mat,
    method          = mice.method,
    printFlag       = TRUE,
    maxit           = 5
)

# plot summary =================================================================
cat("plot summary\n")

p_bmi_imp <-
    imp_bmi %>%
    complete(
        action = "long",
        include = TRUE
    ) %>%
    select(
        alf_e,
        age_3cat,
        sex,
        imp = .imp,
        bmi_log
    ) %>%
    mutate(
        imp = factor(imp, 0:1, c("observed", "cmp1"))
    ) %>%
    filter(imp != "observed" | not_na(bmi_log)) %>%
    ggplot(aes(x = exp(bmi_log), group = imp, colour = imp)) +
    facet_grid(age_3cat ~ sex) +
    geom_density() +
    labs(x = "bmi") +
    xlim(10, 52)

print(p_bmi_imp)

ggsave(
    p_bmi_imp,
    file = "results/p_impute_bmi_density.png",
    width = p_width * 2,
    height = p_height
)


# categorise ===================================================================
cat("categorise\n")

d_bmi_imp <-
    imp_bmi %>%
    complete(
        action = "broad",
        include = TRUE
    ) %>%
    select(
        alf_e = alf_e.0,
        bmi_imp = bmi_log.1
    ) %>%
    mutate(
        bmi_imp = exp(bmi_imp)
    ) %>%
    mutate(
        bmi_cat = case_when(
                              bmi_imp < 18.5 ~ "<18.5",     # underweight
            18.5 <= bmi_imp & bmi_imp < 25.0 ~ "18.5-24.9", # healthly weight
            25.0 <= bmi_imp & bmi_imp < 30.0 ~ "25.0-29.9", # overweight
            30.0 <= bmi_imp & bmi_imp < 35.0 ~ "30.0-34.9", # obese - part 1
            35.0 <= bmi_imp & bmi_imp < 40.0 ~ "35.0-39.9", # obese - part 2
            40.0 <= bmi_imp                  ~ "40.0+"
        ) %>%
        factor() %>%
        fct_relevel("<18.5")
    )

# save =========================================================================
cat("save\n")

qsave(
    d_bmi_imp,
    file = save_dir("d_bmi_imp.qs")
)

beep()
