# JGI spikeins

library(tidyverse)

jgi_mixes = readxl::read_excel("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/JGI_mixes.xlsx") |>
  pivot_longer(cols = ends_with("stoichiometry"),
               names_to = "sequins_mix",
               values_to = "AMT") |>
  drop_na(AMT) |>
  mutate(sequins_mix = str_remove(sequins_mix, "_stoichiometry"),
         sequins_mix = str_remove(sequins_mix, "MIX_")) |>
  mutate(RATIO = AMT / sum(AMT), .by = sequins_mix) |>
  rename(feature_id = ID)

# which mixes?

sources = read_lines("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/Metagenome_SIP_Proposal_508046.txt",
                     skip = 27) |>
  enframe() |>
  filter(str_detect(value, "^\\d+\\. ")) |>
  separate(value, sep = "\t", into = c("Source_sample","Source_sample_ID","Sample_group","Group ID","Isotope_label","SIP_combined_assembly_AP_ID")) |>
  separate(Source_sample, sep = " ", into = c("SOURCE", "SOURCE_ID")) |>
  mutate(SOURCE = str_remove(SOURCE, "\\.")) |>
  select(SOURCE, source_mat_id = SOURCE_ID, isotope = Isotope_label)


samples = read_lines("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/Metagenome_SIP_Proposal_508046.txt",
           skip = 27) |>
  enframe() |>
  filter(str_detect(value, "^\\d+\\.\\d+")) |>
  separate(value, sep = "\t", into = c("Fraction","Fraction_eluted_volume (uL)","Fraction_density (g/mL)","Eluted_DNA_concentration (ng/uL)","Run_date","Library_name","Fastq_name","Sequencing_project_ID","Sequins_added (pg)","Mix_type","Raw_reads_count","Filtered_reads_count")) |>
  separate(Fraction, sep = " ", into = c("Fraction", "SAMPLE")) |>
  separate(Fraction, sep = "\\.", into = c("SOURCE", "Fraction")) |>
  select(SAMPLE,
         SOURCE,
         gradient_pos = Fraction,
         gradient_pos_density = `Fraction_density (g/mL)`,
         eluted_volume_ul = `Fraction_eluted_volume (uL)`,
         eluted_conc_ng_ul = `Eluted_DNA_concentration (ng/uL)`,
         sequins_pg = `Sequins_added (pg)`,
         sequins_mix = Mix_type) |>
  left_join(sources, by = "SOURCE") |>
  select(-SOURCE)


# add coverage data

read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  select(feature_id = Feature, AD_T1_12C_R1_SIPMG_10) |>
  left_join(jgi_mixes, by = "feature_id") |>
  filter(sequins_mix == "A") |>
  # don't have pg amount added yet
  ggplot(aes(x = AD_T1_12C_R1_SIPMG_10, y = RATIO)) +
    geom_point() +
    # scale_x_log10() +
    # scale_y_log10() +
    geom_smooth(method = "lm") +
    ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        parse = TRUE)

# all
# this isn't quite right as the mixes aren't matched up correctly yet
# https://stackoverflow.com/questions/64505302/how-to-extract-the-slope-coefficient-from-tidy-data-frame-made-with-broom-packag
df = read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  pivot_longer(cols = c(everything(), -Feature),
               names_to = "SAMPLE",
               values_to = "COV") |>
  select(feature_id = Feature, everything()) |>
  left_join(jgi_mixes |> filter(sequins_mix == "A"), by = "feature_id") |>
  drop_na(GENOME_ID) |>
  select(feature_id, SAMPLE, RATIO, COV) |>
  filter(COV > 0) |>
  #mutate(RATIO = log10(RATIO), COV = log10(COV)) |>
  #group_by(SAMPLE) |>
  nest(data = c(-SAMPLE))

corn_county <- function(df) {
  lm(RATIO ~ COV, data = df)
}

corn_models <- df %>%
  mutate(model = map(data, corn_county)) %>%
  mutate(tidy = map(model, broom::tidy),
         glance = map(model, broom::glance),
         augment = map(model, broom::augment),
         rsq = glance %>% map_dbl('r.squared'),
         intercept = tidy %>% map_dbl(function(x) x$estimate[1]),
         slope = tidy %>% map_dbl(function(x) x$estimate[2])) |>
  ungroup()

corn_models


mags = read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  pivot_longer(cols = c(everything(), -Feature),
               names_to = "SAMPLE",
               values_to = "COV") |>
  filter(!Feature %in% jgi_mixes$feature_id) |>
  mutate(COV = COV)

pg_add = 100

mags = mags |>
  nest(new_data = c(-SAMPLE))




d = corn_models %>%
  select(SAMPLE, data, model) %>%
  left_join(mags, by = "SAMPLE") |>
  mutate(.pred_RATIO = map2(model, new_data, predict),
         new_data = new_data) |>
  unnest(c(.pred_RATIO, new_data))

broom::tidy(d$model[[1]])

d |> summarize(S = sum(.pred_RATIO), .by = SAMPLE)
d |> unnest(data, names_repair = "unique") |> summarize(S = sum(.pred_RATIO), .by = SAMPLE)

0.177*0.0000640 + 0.00465




##

m = read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  pivot_longer(cols = c(everything(), -Feature),
               names_to = "SAMPLE",
               values_to = "COV") |>
  select(feature_id = Feature, everything()) |>
  left_join(jgi_mixes |> filter(sequins_mix == "A"), by = "feature_id") |>
  drop_na(GENOME_ID) |>
  select(feature_id, SAMPLE, RATIO, COV) %>%
  group_by(SAMPLE) |>
  summarise(model = list(lm(COV ~ RATIO)),
            coef = list(coef(model[[1]])),
            Rsqrd = summary(model[[1]])$r.sq)%>%
  unnest_wider(coef, names_repair = 'unique')


J = read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  pivot_longer(cols = c(everything(), -Feature),
               names_to = "SAMPLE",
               values_to = "COV") |>
  filter(!Feature %in% jgi_mixes$feature_id) |>
  left_join(m, by = "SAMPLE") |>
  mutate(fitted = map(model, broom::augment))

# J$fitted


J %>%
  unnest(fitted, names_repair = "unique")



J


#
library(gapminder)
gapminder_lm <- gapminder %>%
  nest(data = c(year, lifeExp, pop, gdpPercap)) %>%
  mutate(model = map(data, ~lm(lifeExp ~ pop, .)),
         fitted = map(model, broom::augment)) %>%
  unnest(fitted)

gapminder_lm %>%
  filter(country == "Egypt") %>%
  ggplot(aes(lifeExp, .fitted)) +
  geom_point() +
  labs(title = "Egypt")






gapminder_lm <- gapminder %>%
  nest(data = c(year, lifeExp, pop, gdpPercap)) %>%
  mutate(model = map(data, ~lm(lifeExp ~ pop, .)),
         fitted = map(model, broom::augment)) %>%
  unnest(fitted)

gapminder_lm %>%
  filter(country == "Egypt") %>%
  ggplot(aes(lifeExp, .fitted)) +
  geom_point() +
  labs(title = "Egypt")




predict(J$model[[1]], J)

head(J$COV)




# tidymodels

jgi_mixes_wide = readxl::read_excel("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/JGI_mixes.xlsx") |>
  select(feature_id = ID, A = MIX_A_stoichiometry, B = MIX_B_stoichiometry, C = MIX_C_stoichiometry) |>
  mutate(A = A / sum(A, na.rm = T),
         B = B / sum(B, na.rm = T),
         C = C / sum(C, na.rm = T))


as_a_function = function(df) {

  m = df |>
    filter(feature_id %in% jgi_mixes$feature_id) |>
    left_join(jgi_mixes_wide, by = "feature_id") |>
    select(-C, -B) |>
    filter(COV > 0) |>
    rename("RATIO" = "A") |>
    select(-feature_id)

  lm_model = parsnip::linear_reg(mode = "regression") |> parsnip::set_engine("lm")
  lm_form_fit = lm_model |> parsnip::fit(RATIO ~ COV, data = m)
  lm_form_fit_xy = lm_model |> parsnip::fit_xy(x = m |> select(COV),
                                               y = m |> pull(RATIO))

  pg_added = 100

  p = df #|>
    #rename(feature_id = Feature) |>
    # filter(!feature_id %in% jgi_mixes$feature_id)


  bind_cols(p,
            predict(lm_form_fit_xy, new_data = p),
            predict(lm_form_fit_xy, new_data = p, type = "pred_int"))
}

df = read_csv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_spike_ins/508046.bins_coverage_metadata.csv") |>
  pivot_longer(cols = c(everything(), -Feature),
               names_to = "SAMPLE",
               values_to = "COV")  |>
  rename("feature_id" = "Feature") |>
  #mutate(COV = log10(COV)) |>
  group_by(SAMPLE) |>
  nest()




j = df %>%
  mutate(predictions = purrr::map(data, purrr::possibly(as_a_function, otherwise = NA)))

j |>
  unnest(predictions) |>
  filter(SAMPLE == "AD_T1_12C_R1_SIPMG_10") |>
  ungroup() |>
  summarize(M = mean(.pred),
            SD = sd(.pred),
            ) # .pred should be ~0.00362 - pretty close!

A = jgi_mixes |>
  filter(sequins_mix == "A") |>
  select(feature_id, RATIO)

j |>
  unnest(predictions) |>
  left_join(A, by = "feature_id") |>
  drop_na(RATIO) |>
  ggplot(aes(x = .pred, y = RATIO)) +
    geom_hex() +
  geom_abline(slope = 1)


j |>
  unnest(model)



j[3,]$predictions[[1]] |>
  ggplot(aes(x = COV, y = .pred)) +
    geom_point(color = "gray70")


testdf = df[1,]$data[[1]]
as_a_function(testdf) |>
  summarize(S = mean(.pred))
