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
l_england[["overall"]] <- read_csv("england_results/model_results_broad.csv",   name_repair = make_clean_names)
l_england[["md"]]      <- read_csv("england_results/model_results_broad_m.csv", name_repair = make_clean_names)
l_england[["pb"]]      <- read_csv("england_results/model_results_broad_p.csv", name_repair = make_clean_names)

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

d_scotland <-
  read_csv(file = "scotland_results/broad_hosp_death_main_adj_coef.csv") %>% 
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

l_wales <- list()
l_wales[["overall"]] <- read_csv("wales_results/broad_hosp_death_overall_adj_coef.csv")
l_wales[["md"]]      <- read_csv("wales_results/broad_hosp_death_md_adj_coef.csv")
l_wales[["pb"]]      <- read_csv("wales_results/broad_hosp_death_pb_adj_coef.csv")

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

l_ni <- list()
l_ni[["overall"]] <- read_csv("northern_ireland_results/broad_hosp_death_overall_adj_coef.csv")
l_ni[["pb"]]      <- read_csv("northern_ireland_results/broad_hosp_death_pb_adj_coef.csv")

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


# fixed effects meta analysis --------------------------------------------------

extract_ma_coef <- function(x) {
  nm <- deparse(substitute(x))
  nm <- str_replace(nm, "ma_fit_", "")
  
  d <-
    coef(summary(x)) %>% 
    as_tibble(rownames = "coef") %>% 
    mutate(coef = str_replace(coef, "xparam", "")) %>%
    separate(
      col  = coef,
      into = c("xvar", "xlbl"),
      sep  = ":"
    ) %>% 
    mutate(
      country = "meta",
      country = factor(country),
      strata  = factor(nm),
      xvar    = factor(xvar, lkp_xvar),
      xlbl    = factor(xlbl, lkp_xlbl)
    ) %>% 
    select(
      country,
      strata,
      xvar,
      xlbl,
      estimate,
      std.error = se
    )
  
  return(d)
}

ma_fit_overall <- rma(
  data   = d_nation,
  subset = (strata == "overall"),
  yi     = estimate,
  sei    = std.error,
  mods   = ~ -1 + xparam,
  method = "FE" # fixed effect
)

ma_fit_md <- rma(
  data   = d_nation,
  subset = (strata == "md"),
  yi     = estimate,
  sei    = std.error,
  mods   = ~ -1 + xparam,
  method = "FE" # fixed effect
)

ma_fit_pb <- rma(
  data   = d_nation,
  subset = (strata == "pb"),
  yi     = estimate,
  sei    = std.error,
  mods   = ~ -1 + xparam,
  method = "FE" # fixed effect
)

# stack meta estimates with nation estimates
d_all <-
  bind_rows(
    d_nation,
    extract_ma_coef(ma_fit_overall),
    extract_ma_coef(ma_fit_md),
    extract_ma_coef(ma_fit_pb)
  )

qsave(
  d_all,
  file = "meta_analysis_results/broad_main_d_coef_all.qs"
)


# roll my own forest plot ------------------------------------------------------

lkp_pretty_xvar <- c(
  "Time post\nAutumn 2022\nvaccination"   = "vacc_week",
  "Previous COVID-19\nvaccination"        = "prv_dose_diff",
  "Sex"                                   = "sex",
  "Age"                                   = "age",
  "Ethnicity*"                            = "ethnicity",
  "BMI*"                                  = "bmi",
  "Number of QCovid\nrisk groups*"        = "qcovid_n",
  "Number of BNF\nrisk groups†"           = "bnf_n",
  "Household size"                        = "household_n",
  "Socioeconomic\ndeprivation\nquintile"  = "area_deprivation",
  "Rural/urban area\nclassification"      = "urban_rural"
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
  # no BNF ref for PB and MD models
  filter(!(
    xvar == "bnf_n" & strata %in% c("pb", "md")
  ))

p_main_all_coef <-
  bind_rows(d_all, d_ref) %>% 
  mutate(
    strata   = factor(strata, lkp_strata, names(lkp_strata)),
    country  = factor(country, names(lkp_country)),
    xvar     = factor(xvar, lkp_pretty_xvar, names(lkp_pretty_xvar)),
    hr       = exp(estimate),
    hr.lower = exp(estimate - 1.96 * std.error),
    hr.upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  mutate(
    xlbl = fct_relevel(xlbl, "≥24 weeks")
  ) %>% 
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
  file = "meta_analysis_results/broad_main_p_coef_all.qs"
)


# plot meta-coef only ----------------------------------------------------------

d_all_strict <-
  qread(file = "meta_analysis_results/strict_main_d_coef_all.qs") %>% 
  filter(country == "meta") %>% 
  mutate(country = "meta_strict")

d_main_meta_coef <-
  bind_rows(d_all, d_ref, d_all_strict) %>% 
  filter(country %in% c("meta", "ref", "meta_strict")) %>% 
  mutate(
    strata   = factor(strata, lkp_strata, names(lkp_strata)),
    country  = factor(country, c("ref", "meta", "meta_strict")),
    xvar     = factor(xvar, lkp_pretty_xvar, names(lkp_pretty_xvar)),
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
  geom_point(size = 1, position = position_dodge(0.5)) +
  geom_linerangeh(size = 0.5, position = position_dodge(0.5)) +
  scale_x_continuous(
    name   = "Adjusted hazard ratio (95% CI)",
    trans  = log_trans(),
    breaks = c(1/2^(0:5), 2^(0:5)),
    labels = c(1/2^(0:5), 2^(0:5))
  ) +
  scale_colour_manual(
    values = c(
      "meta_strict" = "#ad002a",
      "meta"        = "#00468b",
      "ref"         = "#000000"
    ),
    labels = c("Meta, Strict", "Meta, Broad", "Ref")
  ) +
  scale_shape_manual(
    values = c(16, 16, 15)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linetype = c(1, 1, 0))),
    shape  = guide_legend(override.aes = list(linetype = c(1, 1, 0)))
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
  file = "meta_analysis_results/broad_main_p_coef_meta.qs"
)

ggsave(
  p_main_meta_coef,
  filename = "meta_analysis_results/broad_main_p_coef_meta.png",
  width    = plot_dim$a4_width,
  height   = plot_dim$a4_height,
  dpi      = plot_dpi
)


# human friendly all coef table ------------------------------------------------

lkp_pretty_xvar <- c(
  "Time post Autumn 2022 vaccination"        = "vacc_week",
  "Time since previous COVID-19 vaccination" = "prv_dose_diff",
  "Sex"                                      = "sex",
  "Age"                                      = "age",
  "Ethnicity*"                               = "ethnicity",
  "BMI*"                                     = "bmi",
  "Number of QCovid risk groups*"            = "qcovid_n",
  "Number of BNF risk groups†"               = "bnf_n",
  "Household size"                           = "household_n",
  "Socioeconomic deprivation quintile"       = "area_deprivation",
  "Rural/urban area classification"          = "urban_rural"
)

d_ref <-
  lkp_xvar_xlbl %>% 
  filter(is_reference == 1) %>% 
  cross_join(data.frame(country = c("ni", "scotland", "wales", "meta"))) %>% 
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
  # no BNF for scotland and wales
  filter(!(xvar == "bnf_n" & country %in% c("scotland", "wales"))) %>% 
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
    hr_ci = if_else(hr_ci == "1.00 (NA, NA)", "1.00", hr_ci)
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

write_pretty_xlsx(
  df = d_main_all_coef,
  file = "meta_analysis_results/broad_main_t_coef_all.xlsx"
)


# done! ------------------------------------------------------------------------
cat("done!\n")