#############################################################################
# Quantifying inequalities in childhood immunization using summary measures of 
# health inequality: An application of WHO Stata and R ‘healthequal’ packages
#############################################################################

# Attach libraries
library(openxlsx)
library(dplyr)

# Install latest version of 'healthequal' from GitHub
remotes::install_github("WHOequity/healthequal")
library(healthequal)

# ---- Prepare immunization dataset from the Health Inequality Data Repository (HIDR) ----

## Import data using HIDR API
api <- "https://datasafe-h5afbhf4gwctabaa.z01.azurefd.net/api/Download/TOP/"
df_imm <- read.xlsx(paste0(api,"rep_imm/data"))

## Limit data to specific indicators, dimensions, years and completeness
data_imm <- df_imm %>%
  
  ### Keep required indicators and dimensions
  filter(indicator_abbr %in% c("vdpt","vzdpt"),
         dimension %in% c("Place of residence",
                          "Economic status (wealth quintile)",
                          "Subnational region"),
         wbincome2024 %in% c("Low income", "Lower middle income",
                             "Upper middle income")) %>%
  group_by(setting,
           date,
           indicator_abbr,
           dimension) %>%
  ### Drop if all subgroups have missing data or all values equal to zero
  mutate(count = n(),
         todrop = all(is.na(estimate)),
         avge = mean(estimate, na.rm = TRUE)) %>%
  filter(todrop != TRUE,
         avge != 0,
         !is.nan(avge),
        (dimension == "Economic status (wealth quintile)" & count == 5) | 
        (dimension == "Place of residence" & count == 2) | 
        dimension == "Subnational region") %>%
  ### Drop if >85% of subnational regions missing data 
  mutate(mis = is.na(estimate) == 1,
         lim15 = sum(mis)/count,
         nunique = length(unique(estimate))) %>%
  filter((dimension == "Subnational region" & lim15 < .15) |
         (dimension != "Subnational region" & lim15 == 0),
         nunique != 1) %>%
  mutate(rnumber = as.numeric(row_number() == 1)) %>% 
  ungroup() %>%
  mutate(favourable_indicator_new = 1) %>%
  
  ### Keep latest data between 2013-2022
  filter(date >= 2013 & date <= 2022) %>% 
  group_by(setting,
           indicator_abbr) %>%
  mutate(max_date = max(date)) %>%
  ungroup() %>%
  mutate(latest = date == max_date) %>%
  filter(latest == TRUE) %>% 
  select(-max_date)

# ---- Prepare Global Data Lab dataset from the Health Inequality Data Repository ----
  
## Import data using HIDR API
df_gdl <- read.xlsx(paste0(api,"rep_gdl2/data"))

## Keep latest data between 2013-2022
data_gdl <- df_gdl %>%
  filter(date >= 2013 & date <= 2022) %>% 
  group_by(setting,
           indicator_abbr) %>%
  mutate(max_date = max(date)) %>%
  ungroup() %>%
  mutate(latest = date == max_date) %>%
  filter(latest == TRUE) %>% 
  select(-max_date)

## Keep required immunization indicators
data_gdl <- data_gdl %>%
  filter(indicator_abbr %in% c("dtp3age1",
                               "dtp1age1") & 
                            dimension == "Subnational region") %>%
  mutate(estimate = ifelse(indicator_abbr == "dtp1age1", 100 - estimate, estimate),
         indicator_name = ifelse(indicator_abbr == "dtp1age1",
                                 "One-year-old children who did not receive 
                                 any doses of the DTP vaccine", indicator_name),
         indicator_abbr = ifelse(indicator_abbr == "dtp1age1",
                                 "vzdpt", indicator_abbr),
         indicator_abbr = ifelse(indicator_abbr == "dtp3age1",
                                 "vdpt", indicator_abbr),
         favourable_indicator = ifelse(indicator_abbr == "vzdpt",
                                       0, favourable_indicator)) %>%
  filter(wbincome2024 != "High income")

## Import subnational HDI values
df_hdi <- read.xlsx(paste0(api,"rep_gdl1/data"))
data_hdi <- df_hdi %>%
  filter(indicator_abbr == "shdi") %>%
  rename(hdi = estimate) %>%
  select(setting,
         date,
         subgroup,
         hdi)

## Merge subnational HDI values
data_merged <- data_gdl %>% 
  left_join(data_hdi, by = c("setting",
                             "date",
                             "subgroup")) %>%
  arrange(setting,
          date,
          indicator_abbr,
          hdi) 

## Order subnational regions
data_merged <- data_merged %>%
  group_by(setting,
           date,
           indicator_abbr) %>%
  mutate(subgroup_order = row_number(),
         ordered_dimension = 1,
         dimension = "Subnational region - HDI", 
         favourable_indicator_new = 1) %>%
  ungroup()

data_subnational_hdi <- data_merged

# ---- Append immunization and HDI datasets ----

data <- data_imm %>% 
  bind_rows(data_subnational_hdi)

# ---- Calculate summary measures of health inequality ----

## Binary dimension (place of residence)
measures_binary <- data %>%
  group_by(setting,
           date, 
           indicator_abbr,
           dimension) %>%
  filter(dimension == "Place of residence") %>%
  summarise(d = d(est = estimate, 
                  se = se, 
                  favourable_indicator = favourable_indicator_new, 
                  ordered_dimension = ordered_dimension, 
                  reference_subgroup = reference_subgroup),
            r = r(est = estimate, 
                  se = se, 
                  favourable_indicator = favourable_indicator_new, 
                  ordered_dimension = ordered_dimension, 
                  reference_subgroup = reference_subgroup))

summary_binary <- cbind(setting = rep(measures_binary$setting, 1),
                        date = rep(measures_binary$date, 1),
                        indicator = rep(measures_binary$indicator_abbr, 1),
                        dimension = rep(measures_binary$dimension, 1),
                        bind_rows(measures_binary$d, 
                                  measures_binary$r)) %>%
  arrange(setting,
          date,
          indicator,
          measure)

## Ordered dimensions (economic status and subnational region ordered by HDI)
measures_ordered <- data %>%
  group_by(setting,
           date,
           indicator_abbr,
           dimension) %>%
  filter(ordered_dimension == 1) %>%
  summarise(d = d(est = estimate, 
                se = se, 
                favourable_indicator = favourable_indicator_new, 
                ordered_dimension = ordered_dimension, 
                subgroup_order = subgroup_order, 
                reference_subgroup = reference_subgroup),
            r = r(est = estimate, 
                se = se, 
                favourable_indicator = favourable_indicator_new, 
                ordered_dimension = ordered_dimension, 
                subgroup_order = subgroup_order, 
                reference_subgroup = reference_subgroup),
            aci = aci(est = estimate,
                    subgroup_order = subgroup_order, 
                    pop=population),
            rci = rci(est = estimate,
                    subgroup_order = subgroup_order, 
                    pop = population),
            sii = sii(est = estimate,
                      subgroup_order = subgroup_order,
                      pop = population), 
            rii = rii(est = estimate,
                      subgroup_order = subgroup_order, 
                      pop = population), 
            par = parisk(est = estimate, 
                         ordered_dimension = ordered_dimension,
                         favourable_indicator = favourable_indicator,
                         scaleval = indicator_scale,
                         subgroup_order = subgroup_order, 
                         pop = population),
            paf = paf(est = estimate, 
                      ordered_dimension = ordered_dimension,
                      favourable_indicator = favourable_indicator,
                      scaleval = indicator_scale,
                      subgroup_order = subgroup_order, 
                      pop = population))

summary_ordered <- cbind(setting = rep(measures_ordered$setting, 1),
                         date = rep(measures_ordered$date, 1),
                         indicator = rep(measures_ordered$indicator_abbr, 1),
                         dimension = rep(measures_ordered$dimension, 1),
                         rbind(measures_ordered$d, 
                               measures_ordered$r,
                               measures_ordered$aci, 
                               measures_ordered$rci,
                               measures_ordered$sii, 
                               measures_ordered$rii,
                               measures_ordered$par, 
                               measures_ordered$paf)) %>%
  arrange(setting,
          date,
          indicator,
          measure)

## Non-ordered dimension (subnational region)
measures_nonordered <- data %>%
  group_by(setting,
           date,
           indicator_abbr,
           dimension) %>%
  filter(dimension == "Subnational region") %>%
  summarise(d = d(est = estimate,
                  se = se,
                  favourable_indicator = favourable_indicator_new,
                  ordered_dimension = ordered_dimension,
                  reference_subgroup = reference_subgroup),
            r = r(est = estimate,
                  se = se,
                  favourable_indicator = favourable_indicator_new,
                  ordered_dimension = ordered_dimension,
                  reference_subgroup = reference_subgroup),
            bgv = bgv(est = estimate,
                      se = se,
                      pop = population),
            bgsd = bgsd(est = estimate,
                        se = se,
                        pop = population,
                        scaleval = indicator_scale),
            cov = covar(est = estimate,
                        se = se,
                        pop = population,
                        scaleval = indicator_scale),
            mdmu = mdmu(est = estimate,
                        se = se,
                        pop = population,
                        scaleval = indicator_scale),
            mdmw = mdmw(est = estimate,
                        se = se,
                        pop = population,
                        scaleval = indicator_scale),
            idisu = idisu(est = estimate,
                          se = se,
                          pop = population,
                          scaleval = indicator_scale),
            idisw = idisw(est = estimate,
                          se = se,
                          pop = population,
                          scaleval = indicator_scale),
            ti = ti(est = estimate,
                    se = se,
                    pop = population),
            par = parisk(est = estimate,
                         ordered_dimension = ordered_dimension,
                         favourable_indicator = favourable_indicator,
                         scaleval = indicator_scale,
                         pop = population),
            paf = paf(est = estimate,
                      ordered_dimension = ordered_dimension,
                      favourable_indicator = favourable_indicator,
                      scaleval = indicator_scale,
                      pop = population))

summary_nonordered <- cbind(setting = rep(measures_nonordered$setting, 1),
                            date = rep(measures_nonordered$date, 1),
                            indicator = rep(measures_nonordered$indicator_abbr, 1),
                            dimension = rep(measures_nonordered$dimension, 1),
                            bind_rows(measures_nonordered$d, 
                                      measures_nonordered$r,
                                      measures_nonordered$bgv, 
                                      measures_nonordered$bgsd,
                                      measures_nonordered$cov, 
                                      measures_nonordered$mdmu,
                                      measures_nonordered$mdmw, 
                                      measures_nonordered$idisu,
                                      measures_nonordered$idisw, 
                                      measures_nonordered$ti,
                                      measures_nonordered$par, 
                                      measures_nonordered$paf)) %>%
  arrange(setting,
          date,
          indicator,
          measure)

# Append results
country_results <- summary_binary %>%
  bind_rows(summary_ordered) %>%
  bind_rows(summary_nonordered) %>%
  mutate(measure = case_when(
    measure == "d" ~ "Difference",
    measure == "r" ~ "Ratio",
    measure == "aci" ~ "Absolute concentration index",
    measure == "rci" ~ "Relative concentration index",
    measure == "sii" ~ "Slope index of inequality",
    measure == "rii" ~ "Relative index of inequality",
    measure == "bgv" ~ "Between-group variance",
    measure == "bgsd" ~ "Between-group standard deviation",
    measure == "cov" ~ "Coefficient of variation",
    measure == "mdmw" ~ "Weighted mean difference from mean",
    measure == "mdmu" ~ "Unweighted mean difference from mean",
    measure == "idisw" ~ "Weighted index of disparity",
    measure == "idisu" ~ "Unweighted index of disparity",
    measure == "ti" ~ "Theil index",
    measure == "par" ~ "Population attributable risk",
    measure == "paf" ~ "Population attributable fraction",
    TRUE ~ measure)) %>%
  arrange(measure,
          dimension,
          indicator,
          setting)

write.csv(country_results, "Results/R-country-results.csv")
