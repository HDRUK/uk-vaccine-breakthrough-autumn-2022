source("clear_workspace.r")

# lookup for tidy names --------------------------------------------------------

lkp_strata <- c(
  "Overall"    = "overall",
  "Comirnaty"  = "pb",
  "Spikevax*"  = "md"
)

lkp_xvar_xlbl <- read_xlsx(path = "lkp_main_xvar_xlbl.xlsx")

lkp_xvar <-
  lkp_xvar_xlbl %>% 
  filter(!is.na(clean_xvar)) %>% 
  pull(clean_xvar) %>% 
  unique()

lkp_xlbl <-
  lkp_xvar_xlbl %>% 
  select(clean_xlbl) %>% 
  drop_na() %>% 
  distinct() %>% 
  arrange(
    clean_xlbl != 0,
    clean_xlbl != 1,
    clean_xlbl != 2,
    clean_xlbl != 3,
    clean_xlbl != 4,
    clean_xlbl != 5,
    clean_xlbl != "5th (Least)",
    clean_xlbl != "4th",
    clean_xlbl != "3rd",
    clean_xlbl != "2nd",
    clean_xlbl != "1st (Most)"
  ) %>% 
  pull(clean_xlbl)


# load England -----------------------------------------------------------------
cat("load England\n")

l_england <- list()
l_england[["overall"]] <- read_csv("england_results/model_results_strict.csv",   name_repair = make_clean_names)
l_england[["md"]]      <- read_csv("england_results/model_results_strict_m.csv", name_repair = make_clean_names)
l_england[["pb"]]      <- read_csv("england_results/model_results_strict_p.csv", name_repair = make_clean_names)

expr_eng_x <-
  lkp_xvar_xlbl %>%
  pull(england_xvar) %>%
  na.omit() %>%
  unique() %>%
  str_c(collapse = "|") %>%
  str_c("(", ., ")(.*)")

d_england <-
  bind_rows(l_england, .id = "strata") %>%
  rename(
    estimate = coef,
    std.error = se_coef
  ) %>%
  mutate(
    country = factor("england"),
    strata = factor(strata, lkp_strata)
  ) %>%
  # clean variable and level names
  mutate(
    xvar = str_replace(x, expr_eng_x, "\\1"),
    xlbl = str_replace(x, expr_eng_x, "\\2")
  ) %>%
  left_join(lkp_xvar_xlbl, join_by(xvar == england_xvar, xlbl == england_xlbl)) %>%
  # exclude bmi missing
  filter(!(
    xvar == "bmi_cat" & xlbl == "missing"
  )) %>%
  # exclude vacc type
  filter(!(
    xvar == "vacc_type"
  )) %>%
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_england %>% assert(not_na, xvar, estimate)


# load Scotland ----------------------------------------------------------------
cat("load Scotland\n")

d_scotland <-
  read_csv(file = "scotland_results/strict_hosp_death_main_adj_coef.csv") %>% 
  rename(strata = vacc_group) %>% 
  filter(est_type == "adj") %>%
  mutate(
    country = factor("scotland"),
    strata = str_to_lower(strata),
    strata = if_else(strata == "mo", "md", strata, strata),
    strata = factor(strata, lkp_strata)
  ) %>% 
  # clean variable and level names
  left_join(lkp_xvar_xlbl, join_by(xvar == scotland_xvar, xlbl == scotland_xlbl)) %>% 
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_scotland %>% assert(not_na, xvar, estimate)


# load Wales -------------------------------------------------------------------
cat("load Wales\n")

l_wales <- list()
l_wales[["overall"]] <- read_csv("wales_results/strict_hosp_death_overall_adj_coef.csv")
l_wales[["md"]]      <- read_csv("wales_results/strict_hosp_death_md_adj_coef.csv")
l_wales[["pb"]]      <- read_csv("wales_results/strict_hosp_death_pb_adj_coef.csv")

d_wales <-
  bind_rows(l_wales, .id = "strata") %>% 
  mutate(
    country = factor("wales"),
    strata = factor(strata, lkp_strata)
  ) %>% 
  left_join(lkp_xvar_xlbl, join_by(xvar == wales_xvar, xlbl == wales_xlbl)) %>% 
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_wales %>% assert(not_na, xvar, estimate)


# load Northern Ireland --------------------------------------------------------
cat("load Northern Ireland\n")

l_ni <- list()
l_ni[["overall"]] <- read_csv("northern_ireland_results/strict_hosp_death_overall_adj_coef.csv")
l_ni[["pb"]]      <- read_csv("northern_ireland_results/strict_hosp_death_pb_adj_coef.csv")
  
d_ni <-
  bind_rows(l_ni, .id = "strata") %>% 
  mutate(
    country = "ni",
    strata = factor(strata, lkp_strata)
  ) %>% 
  # clean variable and level names
  left_join(lkp_xvar_xlbl, join_by(xvar == ni_xvar, xlbl == ni_xlbl)) %>% 
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_ni %>% assert(not_na, xvar, estimate)


# stack coefs from each nation -------------------------------------------------
cat("stack coefs from each nation\n")

d_nation <- 
  bind_rows(
    d_england,
    d_scotland,
    d_wales,
    d_ni
  ) %>% 
  mutate(
    xvar = factor(xvar, lkp_xvar),
    xlbl = factor(xlbl, lkp_xlbl)
  ) %>% 
  arrange(
    country,
    strata,
    xvar,
    xlbl
  ) %>% 
  mutate(
    xparam = str_c(xvar, ":", xlbl),
    xparam = fct_inorder(xparam),
    .after = xlbl
  )

# View(d_nation)

# fixed effects meta analysis --------------------------------------------------
cat("fixed effects meta analysis\n")

meta_analyse <- function(df, strata, xvar, xlbl) {
  # df = d_nation; strata = "overall"; xvar = "sex"; xlbl = "Male"
  df_subset <- df[df$strata == strata & df$xvar == xvar & df$xlbl == xlbl, ]

  fit_ma <- rma(
    data   = df_subset,
    yi     = estimate,
    sei    = std.error,
    method = "FE" # fixed effect
  )
  
  # extract meta coef
  d_coef <-
    coef(summary(fit_ma)) %>% 
    as_tibble() %>% 
    select(
      estimate,
      std.error = se
    )
  
  # extract test results
  d_test <-
    data.frame(
      I2  = fit_ma$I2,
      H2  = fit_ma$H2,
      Q   = fit_ma$QE,
      Q.p = fit_ma$QEp
    )
  
  # combine and finalise output
  d_result <-
    bind_cols(d_coef, d_test) %>% 
    mutate(
      country = "meta",
      strata = strata,
      xvar = xvar,
      xlbl = xlbl,
      .before = estimate
    )
  
  # goodbye
  return(d_result)
}

l_ma_coef <- list()
d_x <- d_nation %>% select(strata, xvar, xlbl) %>% distinct()

for (i in 1:nrow(d_x)) {
  l_ma_coef[[i]] <- meta_analyse(
    df     = d_nation,
    strata = d_x$strata[i],
    xvar   = d_x$xvar[i],
    xlbl   = d_x$xlbl[i]
  )
}

d_ma_coef <- bind_rows(l_ma_coef)

# stack meta estimates with nation estimates
d_all <-
  bind_rows(
    d_nation %>% select(-xparam),
    d_ma_coef
  )

qsave(
  d_all,
  file = "meta_analysis_results/strict_main_d_coef_all.qs"
)

# meta heterogeneity stats -----------------------------------------------------

d_ma_het_test <-
  d_all %>%
  filter(country == "meta") %>% 
  mutate(
    Q = format(round(Q, 2), nsmall = 2),
    Q.p = format(round(Q.p, 4), nsmall = 4),
    Q.p = if_else(Q.p == "0.0000", "<0.0001", Q.p),
    Q_test = str_glue("{Q} (p={Q.p})")
  ) %>% 
  select(
    strata,
    xvar,
    xlbl,
    Q_test
  )


# derive footnote symbols ------------------------------------------------------

lkp_xvar_footnote <-
  d_all %>%
  filter(!is.na(xvar)) %>% 
  group_by(xvar) %>% 
  summarise(
    england  = any(country == "england"),
    ni       = any(country == "ni"),
    scotland = any(country == "scotland"),
    wales    = any(country == "wales")
  ) %>% 
  mutate(
    footnote = case_when(
       england & !ni &  scotland &  wales ~ "*",
       england & !ni & !scotland & !wales ~ "†",
      !england &  ni &  scotland &  wales ~ "‡",
      !england & !ni &  scotland &  wales ~ "§",
      !england &  ni & !scotland & !wales ~ "‖",
       england & !ni & !scotland &  wales ~ "¶"
    )
  ) %>% 
  select(
    xvar,
    footnote
  )

lkp_xvar_footnote

# human friendly all coef table ------------------------------------------------
cat("human friendly all coef table\n")

lkp_pretty_xvar <- c(
  "Time post Autumn 2022 vaccination"        = "vacc_week",
  "Time since previous COVID-19 vaccination" = "prv_dose_diff",
  "Sex"                                      = "sex",
  "Age"                                      = "age",
  "Ethnicity"                                = "ethnicity",
  "BMI"                                      = "bmi",
  "Number of QCovid risk groups"             = "qcovid_n",
  "Number of BNF risk groups"                = "bnf_n",
  "Household size"                           = "household_n",
  "Socioeconomic deprivation quintile"       = "area_deprivation",
  "Rural/urban area classification"          = "urban_rural"
)

d_ref <-
  lkp_xvar_xlbl %>% 
  filter(is_reference == 1) %>% 
  cross_join(data.frame(country = c("england", "ni", "scotland", "wales", "meta"))) %>% 
  mutate(
    xvar = fct_inorder(clean_xvar),
    xlbl = fct_inorder(clean_xlbl),
    estimate = 0,
    std.error = NA
  ) %>% 
  select(
    country,
    xvar,
    xlbl,
    estimate,
    std.error
  ) %>% 
  # replicate for all countries
  cross_join(data.frame(strata = factor(lkp_strata))) %>% 
  # no BNF for england, scotland and wales
  filter(!(xvar == "bnf_n" & country %in% c("england", "scotland", "wales"))) %>% 
  # no NI refs for MD models
  filter(!(country == "ni" & strata == "md")) %>% 
  # no NI ref for BMI or QCovid
  filter(!(country == "ni" & xvar %in% c("qcovid_n", "bmi")))

d_main_all_coef <-
  bind_rows(d_all, d_ref) %>% 
  mutate(
    # friendly values
    hr  = exp(estimate),
    lci = exp(estimate - 1.96 * std.error),
    uci = exp(estimate + 1.96 * std.error),
    # formatting
    hr  = format(round(hr,  2), nsmall = 2, trim = TRUE),
    lci = format(round(lci, 2), nsmall = 2, trim = TRUE),
    uci = format(round(uci, 2), nsmall = 2, trim = TRUE),
    # gluing
    hr_ci = str_glue("{hr} ({lci}, {uci})"),
    hr_ci = if_else(hr_ci == "1.00 (NA, NA)", "1.00", hr_ci),
  ) %>% 
  select(
    country,
    strata,
    xvar,
    xlbl,
    hr_ci
  ) %>% 
  pivot_wider(
    values_from = hr_ci,
    names_from = country,
    names_glue = "{country}_{.value}"
  ) %>% 
  arrange(
    strata,
    xvar,
    xlbl
  ) %>% 
  mutate(
    xlbl = as.character(xlbl)
  )

d_main_all_coef <- d_main_all_coef %>% left_join(d_ma_het_test, join_by(strata, xvar, xlbl))

write_pretty_xlsx(
  df = d_main_all_coef,
  file = "meta_analysis_results/strict_main_t_coef_all.xlsx"
)


# roll my own forest plot ------------------------------------------------------
cat("roll my own forest plot\n")

lkp_pretty_xvar <- tribble(
    ~xvar,              ~xvar_pretty,
    "vacc_week",        "Time post\nAutumn 2022\nvaccination",
    "prv_dose_diff",    "Previous COVID-19\nvaccination",
    "sex",              "Sex",
    "age",              "Age",
    "ethnicity",        "Ethnicity",
    "bmi",              "BMI",
    "qcovid_n",         "Number of QCovid\nrisk groups",
    "bnf_n",            "Number of BNF\nrisk groups",
    "household_n",      "Household size",
    "area_deprivation", "Socioeconomic\ndeprivation\nquintile",
    "urban_rural",      "Rural/urban area\nclassification"
  ) %>% 
  left_join(lkp_xvar_footnote, join_by(xvar)) %>% 
  mutate(
    xvar_pretty = if_else(is.na(footnote), xvar_pretty, str_c(xvar_pretty, " ", footnote))
  )

d_ref <-
  lkp_xvar_xlbl %>% 
  filter(is_reference == 1) %>% 
  mutate(
    country   = factor("ref"),
    xvar      = fct_inorder(clean_xvar),
    xlbl      = fct_inorder(clean_xlbl),
    estimate  = 0,
    std.error = 0
  ) %>% 
  select(
    country,
    xvar,
    xlbl,
    estimate,
    std.error
  ) %>% 
  cross_join(data.frame(strata = factor(lkp_strata))) %>% 
  # no BNF ref for MD models
  filter(!(
    xvar == "bnf_n" & strata == "md"
  ))

d_all_ref <-
  bind_rows(d_all, d_ref) %>% 
  mutate(
    strata   = factor(strata, lkp_strata, names(lkp_strata)),
    country  = factor(country, names(lkp_country)),
    xvar     = factor(xvar, lkp_pretty_xvar$xvar, lkp_pretty_xvar$xvar_pretty),
    hr       = exp(estimate),
    hr.lower = exp(estimate - 1.96 * std.error),
    hr.upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  mutate(
    xlbl = fct_relevel(xlbl, "≥24 weeks")
  )


p_main_all_coef <-
  d_all_ref %>% 
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl,
    colour = fct_rev(country)
  )) +
  facet_grid(
    rows   = vars(xvar),
    cols   = vars(strata),
    space  = "free_y",
    scales = "free_y",
    switch = "y"
  ) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 1
  ) +
  geom_pointrange(
    position = position_dodge(0.65)
  ) +
  scale_x_continuous(
    name   = "Hazards ratio",
    breaks = pretty_breaks()
  ) +
  scale_colour_manual(
    values = lkp_country,
    guide  = guide_legend(reverse = TRUE)
  ) +
  coord_cartesian(
    xlim = c(0, 15)
  ) +
  theme_grey(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background.y = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle= 0, face = "bold")
  )

print(p_main_all_coef)

qsave(
  p_main_all_coef,
  file = "meta_analysis_results/strict_main_p_coef_all.qs"
)




# plot meta-coef only ----------------------------------------------------------
cat("plot meta-coef only\n")

d_main_meta_coef <-
  bind_rows(d_all, d_ref) %>% 
  filter(country %in% c("meta", "ref")) %>% 
  mutate(
    strata   = factor(strata, lkp_strata, names(lkp_strata)),
    country  = factor(country, names(lkp_country)),
    xvar     = factor(xvar, lkp_pretty_xvar$xvar, lkp_pretty_xvar$xvar_pretty),
    hr       = exp(estimate),
    hr.lower = exp(estimate - 1.96 * std.error),
    hr.upper = exp(estimate + 1.96 * std.error)
  ) %>%
  mutate(
    xlbl = fct_relevel(xlbl, "≥24 weeks")
  )

p_main_meta_coef <-
  d_main_meta_coef %>% 
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl,
    colour = fct_rev(country)
  )) +
  facet_grid(
    rows   = vars(xvar),
    cols   = vars(strata),
    space  = "free_y",
    scales = "free_y",
    switch = "y"
  ) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 2,
    linewidth  = (0.5 * 10) / 22
  ) +
  geom_point(size = 1) +
  geom_linerangeh(size = 0.5) +
  scale_x_continuous(
    name   = "Adjusted hazard ratio (95% CI)",
    trans  = log_trans(),
    breaks = c(1/2^(0:5), 2^(0:5)),
    labels = c(1/2^(0:5), 2^(0:5))
  ) +
  scale_colour_manual(
    values = lkp_country,
    labels = c("Ref.", "Meta-estimate")
  ) +
  scale_shape_manual(
    values = c(
      "meta" = 16, # circle
      "ref"  = 15  # square
    )
  ) +
  guides(
    colour = guide_legend(override.aes = list(linetype = c(0, 1))),
    shape  = guide_legend(override.aes = list(linetype = c(0, 1)))
  ) +
  coord_cartesian(
    xlim = c(0.5, 10.3)
  ) +
  theme_bw(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    axis.text.y        = element_text(colour = "black"),
    legend.background  = element_rect(colour = "black", linewidth = (0.5 * 10 / 22)),
    legend.key.height  = unit(0.3, "cm"),
    legend.position    = "bottom",
    legend.spacing.y   = unit(0, "cm"),
    legend.title       = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background    = element_rect(fill='transparent'),
    strip.placement    = "outside",
    strip.background.x = element_rect(fill = NA, colour = NA),
    strip.background.y = element_blank(),
    strip.text.x       = element_text(face = "bold"),
    strip.text.y.left  = element_text(angle = 0, face = "bold")
  )

print(p_main_meta_coef)

qsave(
  p_main_meta_coef,
  file = "meta_analysis_results/strict_main_p_coef_meta.qs"
)

ggsave(
  p_main_meta_coef,
  filename = "meta_analysis_results/strict_main_p_coef_meta.png",
  width    = plot_dim$a4_width,
  height   = plot_dim$a4_height,
  dpi      = plot_dpi
)


# done! ------------------------------------------------------------------------
cat("done!\n")


# powerpoint -------------------------------------------------------------------
cat("powerpoint\n")

xvar_slide1 <- levels(d_all_ref$xvar)[1:5]
xvar_slide2 <- levels(d_all_ref$xvar)[6:10]

p_slide1 <-
  d_all_ref %>% 
  filter(xvar %in% xvar_slide1) %>% 
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl,
    colour = fct_rev(country)
  )) +
  facet_grid(
    rows   = vars(xvar),
    cols   = vars(strata),
    space  = "free_y",
    scales = "free_y",
    switch = "y"
  ) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 1
  ) +
  geom_pointrange(
    size = 0.3,
    position = position_dodge(0.65)
  ) +
  scale_x_continuous(
    name   = "Hazards ratio",
    breaks = pretty_breaks()
  ) +
  scale_colour_manual(
    values = lkp_country,
    guide  = guide_legend(reverse = TRUE, nrow = 1)
  ) +
  coord_cartesian(
    xlim = c(0, 15)
  ) +
  theme_grey(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background.y = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle = 0, face = "bold")
  )

p_slide2 <-
  d_all_ref %>% 
  filter(xvar %in% xvar_slide2) %>% 
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl,
    colour = fct_rev(country)
  )) +
  facet_grid(
    rows   = vars(xvar),
    cols   = vars(strata),
    space  = "free_y",
    scales = "free_y",
    switch = "y"
  ) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 1
  ) +
  geom_pointrange(
    size = 0.3,
    position = position_dodge(0.65)
  ) +
  scale_x_continuous(
    name   = "Hazards ratio",
    breaks = pretty_breaks()
  ) +
  scale_colour_manual(
    values = lkp_country,
    guide  = guide_legend(reverse = TRUE, nrow = 1)
  ) +
  coord_cartesian(
    xlim = c(0, 15)
  ) +
  theme_grey(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background.y = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle = 0, face = "bold")
  )

max_dims <- get_max_dim(p_slide1, p_slide2)
p_slide1 <- set_dim(p_slide1, max_dims)
p_slide2 <- set_dim(p_slide2, max_dims)

print(p_slide1)
print(p_slide2)

ggsave(
  p_slide1,
  file = "meta_analysis_results/strict_main_p_slide1.png",
  width = plot_dim$ppt_width,
  height = plot_dim$ppt_height
)

ggsave(
  p_slide2,
  file = "meta_analysis_results/strict_main_p_slide2.png",
  width = plot_dim$ppt_width,
  height = plot_dim$ppt_height
)
