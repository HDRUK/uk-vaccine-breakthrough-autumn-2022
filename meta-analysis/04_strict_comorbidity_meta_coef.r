source("clear_workspace.r")


# lookup for tidy names --------------------------------------------------------

lkp_xvar_xlbl <-
  read_xlsx(path = "lkp_comorbidity_xvar_xlbl.xlsx") %>% 
  mutate(across(everything(), as.character))

lkp_xvar <-
  lkp_xvar_xlbl %>% 
  pull(clean_xvar) %>% 
  unique()

lkp_xlbl_pretty <-
  lkp_xvar_xlbl %>% 
  select(
    xvar        = clean_xvar,
    xlbl        = clean_xlbl,
    xlbl_pretty = pretty_xlbl
  ) %>% 
  drop_na()

# load England -----------------------------------------------------------------

d_england <-
  read_csv(
    file = "england_results/model_results_conds_strict.csv",
    name_repair = make_clean_names
  ) %>% 
  mutate(
    country = factor("england"),
    xvar = as.character(x),
    xlbl = "1"
  ) %>% 
  filter(!(
    xvar %in% c("cond17", "cond113", "cond117")
  )) %>% 
  # clean variable and level names
  left_join(lkp_xvar_xlbl, join_by(xvar == england_xvar, xlbl == england_xlbl)) %>% 
  # final select
  select(
    country,
    xlbl = clean_xlbl,
    xvar = clean_xvar,
    estimate = coef,
    std.error = se_coef
  )

d_england %>% assert(not_na, xvar, xlbl, estimate)


# load Scotland ----------------------------------------------------------------

d_scotland <-
  read_csv(file = "scotland_results/strict_hosp_death_qcovid_adj_coef.csv") %>% 
  filter(!(
    xvar %in% c("smoking_status", "Q_DIAG_HIV_AIDS", "blood_pressure", "qc_home_cat")
  )) %>% 
  mutate(
    country = factor("scotland"),
    xlbl = as.character(xlbl)
  ) %>% 
  # clean variable and level names
  left_join(lkp_xvar_xlbl, join_by(xvar == scotland_xvar, xlbl == scotland_xlbl)) %>% 
  # final select
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_scotland %>% assert(not_na, xvar, xlbl, estimate)


# load Wales -------------------------------------------------------------------

d_wales <-
  read_csv(file = "wales_results/strict_hosp_death_qcovid_adj_coef.csv") %>%
  filter(!is.na(estimate)) %>% 
  mutate(
    country = factor("wales"),
    xlbl = as.character(xlbl)
  ) %>%
  left_join(lkp_xvar_xlbl, join_by(xvar == wales_xvar, xlbl == wales_xlbl)) %>% 
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    estimate,
    std.error
  )

d_wales %>% assert(not_na, xvar, xlbl, estimate)


# combine coefs from each nation -----------------------------------------------

d_nation <- 
  bind_rows(
    d_england,
    d_scotland,
    d_wales
  ) %>% 
  mutate(
    xvar = factor(xvar, lkp_xvar),
    xlbl = factor(xlbl)
  ) %>% 
  arrange(
    country,
    xvar,
    xlbl
  ) %>% 
  mutate(
    xparam = str_c(xvar, ":", xlbl),
    xparam = fct_inorder(xparam),
    .after = xlbl
  )


# fixed effects meta analysis --------------------------------------------------

meta_analyse <- function(df, xparam) {
  df_subset <- df[df$xparam == xparam, ]
  
  d_xvar_xlbl <- df_subset %>% 
    select(xparam, xvar, xlbl) %>% 
    distinct()
  
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
      xparam = xparam,
      .before = estimate
    ) %>% 
    left_join(d_xvar_xlbl, join_by(xparam))
  
  # goodbye
  return(d_result)
}

l_ma_coef <- list()
v_xparam <- d_nation %>% pull(xparam) %>% unique() %>% na.omit()

for (x in v_xparam) {
  l_ma_coef[[x]] <- meta_analyse(df = d_nation, xparam = x)
}

d_ma_coef <- bind_rows(l_ma_coef)

d_all <-
  bind_rows(
    d_nation,
    d_ma_coef
  ) %>% 
  select(-xparam)

# meta heterogeneity stats -----------------------------------------------------

t_ma_het_test <-
  d_all %>%
  filter(country == "meta") %>% 
  mutate(
    Q = format(round(Q, 2), nsmall = 2),
    Q.p = format(round(Q.p, 4), nsmall = 4),
    Q.p = if_else(Q.p == "0.0000", "<0.0001", Q.p),
    Q_test = str_glue("{Q} (p={Q.p})")
  ) %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  select(
    xlbl = xlbl_pretty,
    Q_test
  )


# table of nation and meta coefs -----------------------------------------------

t_qcovid_all_coef <-
  d_all %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  mutate(
    country = factor(country, names(lkp_country)),
    # friendly values
    hr  = exp(estimate),
    lci = exp(estimate - 1.96 * std.error),
    uci = exp(estimate + 1.96 * std.error)
  ) %>% 
  # order by meta-estimate
  arrange(country, !is.na(hr), desc(hr)) %>% 
  # pretty coef
  mutate(
    # formatting
    hr  = format(round(hr,  2), nsmall = 2, trim = TRUE),
    lci = format(round(lci, 2), nsmall = 2, trim = TRUE),
    uci = format(round(uci, 2), nsmall = 2, trim = TRUE),
    # gluing
    hr_ci = str_glue("{hr} ({lci}, {uci})")
  ) %>% 
  # keep only what we need
  select(
    country,
    xlbl = xlbl_pretty,
    hr_ci
  ) %>% 
  # make wide
  pivot_wider(
    id_cols = xlbl,
    names_from = country,
    values_from = hr_ci
  ) %>% 
  select(
    xlbl,
    england,
    scotland,
    wales,
    meta
  )

t_qcovid_all_coef <-
  t_qcovid_all_coef %>%
  left_join(t_ma_het_test, join_by(xlbl))

write.xlsx(
  x = t_qcovid_all_coef,
  file = "meta_analysis_results/strict_qcovid_t_coef_all.xlsx"
)


# derive footnote symbols ------------------------------------------------------

lkp_xvar_footnote <-
  d_all %>%
  filter(!is.na(xvar)) %>% 
  group_by(xvar) %>% 
  summarise(
    england  = any(country == "england"),
    scotland = any(country == "scotland"),
    wales    = any(country == "wales")
  ) %>% 
  mutate(
    footnote = case_when(
       england & !scotland & !wales ~ "†",
      !england &  scotland &  wales ~ "§",
       england & !scotland &  wales ~ "¶",
      TRUE ~ ""
    )
  ) %>% 
  select(
    xvar,
    footnote
  )

# roll my own forest plot ------------------------------------------------------

p_qcovid_all_coef <-
  d_all %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  left_join(lkp_xvar_footnote, join_by(xvar)) %>% 
  mutate(
    country = factor(country, names(lkp_country)),
    hr = exp(estimate),
    hr.lower = exp(estimate - 1.96 * std.error),
    hr.upper = exp(estimate + 1.96 * std.error),
    xlbl_pretty = str_c(xlbl_pretty, " ", footnote)
  ) %>% 
  # order by meta-estimate
  arrange(country, !is.na(hr), hr) %>% 
  mutate(xlbl_pretty = fct_inorder(xlbl_pretty)) %>% 
  # begin the magic
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl_pretty,
    colour = fct_rev(country)
  )) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 1
  ) +
  geom_pointrangeh(
    position = position_dodge(0.75)
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
    xlim = c(0, 35)
  ) +
  theme_grey(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    legend.title       = element_blank(),
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    strip.background.y = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle= 0, face = "bold")
  )

print(p_qcovid_all_coef)

qsave(
  p_qcovid_all_coef,
  file = "meta_analysis_results/strict_qcovid_p_coef_all.qs"
)

# load prevalence --------------------------------------------------------------

d_all_desc <-
  qread("meta_analysis_results/strict_qcovid_d_desc_all.qs") %>% 
  filter(xlbl != 0) %>% 
  select(xvar, xlbl, pool_p)

# meta-coefs only --------------------------------------------------------------

d_qcovid_meta_coef <-
  d_all %>% 
  filter(country == "meta") %>% 
  left_join(d_all_desc, join_by(xvar, xlbl)) %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  left_join(lkp_xvar_footnote, join_by(xvar)) %>% 
  mutate(
    country = factor(country, names(lkp_country)),
    hr = exp(estimate),
    hr.lower = exp(estimate - 1.96 * std.error),
    hr.upper = exp(estimate + 1.96 * std.error),
    xlbl_pretty = str_c(xlbl_pretty, " ", footnote)
  ) %>% 
  # order by meta-estimate
  arrange(country, !is.na(hr), hr) %>% 
  mutate(xlbl_pretty = fct_inorder(xlbl_pretty))

d_label <-
  d_qcovid_meta_coef %>%
  filter(hr > 12) %>% 
  mutate(
    text_label = str_glue(
      "{hr} [{lci}, {uci}]",
      hr = comma(hr, accuracy = 0.1),
      lci = comma(hr.lower, accuracy = 0.1),
      uci = comma(hr.upper, accuracy = 0.1)
    )
  ) %>% 
  mutate(
    hr = pmin(12, hr.lower)
  ) %>% 
  select(
    country,
    xlbl_pretty,
    hr,
    text_label
  )

p_qcovid_meta_coef <-
  d_qcovid_meta_coef %>% 
  filter(hr > 0.0001) %>% 
  mutate(
    xlbl_pretty = fct_recode(xlbl_pretty,
      "Sickle cell or severe combined\nimmunodeficiency" = "Sickle cell or severe combined immunodeficiency"
    )
  ) %>% 
  ggplot(aes(
    x      = hr,
    xmin   = hr.lower,
    xmax   = hr.upper,
    y      = xlbl_pretty,
    colour = fct_rev(country)
  )) +
  geom_vline(
    xintercept = 1,
    colour     = "#000000",
    linetype   = 2,
    linewidth  = (0.5 * 10) / 22
  ) +
  geom_point(aes(size = pool_p)) +
  geom_linerangeh(size = 0.5) +
  geom_text(
    data    = d_label %>% filter(xlbl_pretty %in% d_qcovid_meta_coef$xlbl_pretty),
    mapping = aes(label = text_label, xmin = NULL, xmax = NULL),
    hjust   = 1,
    nudge_x = -0.1,
    size    = 10 * 5/14 * 0.75
  ) +
  scale_x_continuous(
    name   = "Adjusted hazard ratio (95% CI)",
    trans  = log_trans(),
    breaks = c(1/2^(0:5), 2^(0:5)),
    labels = c(1/2^(0:5), 2^(0:5))
  ) +
  scale_colour_manual(
    values = lkp_country
  ) +
  scale_size_area(
    name   = "Prevalence",
    breaks = c(1, 2, 5, 10),
    labels = label_percent(scale = 1),
    max_size = 5
  ) +
  guides(
    colour = "none",
    size   = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(colour = lkp_country["meta"]))
  ) +
  coord_cartesian(
    xlim = c(0.98, 12)
  ) +
  theme_bw(
    base_size = 10
  ) +
  theme(
    axis.title.y       = element_blank(),
    legend.background  = element_rect(colour = "black", linewidth = (0.5 * 10 / 22)),
    #legend.key.height  = unit(0.3, "cm"),
    legend.position    = "bottom",
    legend.spacing.x   = unit(0, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background.y = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle= 0, face = "bold")
  )

print(p_qcovid_meta_coef)

qsave(
  p_qcovid_meta_coef,
  file = "meta_analysis_results/strict_qcovid_p_coef_meta.qs"
)

ggsave(
  plot     = p_qcovid_meta_coef,
  filename = "meta_analysis_results/strict_qcovid_p_coef_meta.png",
  width    = plot_dim$a4_width,
  height   = plot_dim$a4_height,
  dpi      = plot_dpi
)


# done -------------------------------------------------------------------------
cat("done\n")
