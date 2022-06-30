#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(plm); library(stargazer); library(papeR); library(ggrepel)  
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(IndexNumR); library(wid); library(scales);library(ISOcodes); library(labelled)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(countrycode)
library(WDI); library(lmtest); library(sandwich); library(interactions); library(ggpubr); library(gplots) 
library(fixest)
#-----------------------------------------------------------------------------------------------
names(mega_df)
#-----------------------------------------------------------------------------------------------
regression_subset_df <- mega_df %>%
  select(iso3, year,
         irr,
         marx_rop,
         unemployment_rate,
         inflation_rate,
         pct_gdp_govt_subsidies,
         pct_gdp_international_trade,
         pct_gdp_corporate_financial_assets,
         pct_gdp_national_financial_assets,
         pct_total_investment_GFCF,
         oecd_gfcf_pct_gdp) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% c(1980:2017))

mega_df %>%
  names()
#-----------------------------------------------------------------------------------------------
form_m1 <- oecd_gfcf_pct_gdp ~
  marx_rop +
  unemployment_rate +
  inflation_rate +
  factor(iso3) +
  factor(year)

form_m2 <- oecd_gfcf_pct_gdp ~
  marx_rop +
  unemployment_rate +
  inflation_rate +
  pct_gdp_international_trade +
  pct_gdp_govt_subsidies +
  factor(iso3) +
  factor(year)

form_m3 <- oecd_gfcf_pct_gdp ~
  irr +
  unemployment_rate +
  inflation_rate +
  factor(iso3) +
  factor(year)

form_m4 <- oecd_gfcf_pct_gdp ~
  irr +
  unemployment_rate +
  inflation_rate +
  pct_gdp_international_trade +
  pct_gdp_govt_subsidies +
  factor(iso3) +
  factor(year)
#-----------------------------------------------------------------------------------------------
# Models
#-----------------------------------------------------------------------------------------------
lm_m1 <- lm(form_m1, data = regression_subset_df)
lm_m2 <- lm(form_m2, data = regression_subset_df)
lm_m3 <- lm(form_m3, data = regression_subset_df)
lm_m4 <- lm(form_m4, data = regression_subset_df)
#-----------------------------------------------------------------------------------------------
# Robust Standard Error Objects
#-----------------------------------------------------------------------------------------------
hc_se_lm_m1 <- sqrt(diag(vcovHC(lm_m1, type = "HC1")))
hc_se_lm_m2 <- sqrt(diag(vcovHC(lm_m2, type = "HC1")))
hc_se_lm_m3 <- sqrt(diag(vcovHC(lm_m3, type = "HC1")))
hc_se_lm_m4 <- sqrt(diag(vcovHC(lm_m4, type = "HC1")))
#-----------------------------------------------------------------------------------------------
# PCSE Standard Error Objects
#-----------------------------------------------------------------------------------------------
pcse_se_lm_m1 <- sqrt(diag(vcovPC(lm_m1, cluster = ~ iso3 + year)))
pcse_se_lm_m2 <- sqrt(diag(vcovPC(lm_m2, cluster = ~ iso3 + year)))
pcse_se_lm_m3 <- sqrt(diag(vcovPC(lm_m3, cluster = ~ iso3 + year)))
pcse_se_lm_m4 <- sqrt(diag(vcovPC(lm_m4, cluster = ~ iso3 + year)))
#-----------------------------------------------------------------------------------------------
# Creating List of LM Objects
#-----------------------------------------------------------------------------------------------
lm_group <- list(lm_m1, lm_m2, lm_m3, lm_m4)
hc.se_group <- list(hc_se_lm_m1, hc_se_lm_m2, hc_se_lm_m3, hc_se_lm_m4)
pcse.se_group <- list(pcse_se_lm_m1, pcse_se_lm_m2, pcse_se_lm_m3, pcse_se_lm_m4)
#-----------------------------------------------------------------------------------------------
stargazer(lm_group,
          se = c(pcse.se_group),
          type = "text",
          omit = c("factor\\(iso3\\)",
                   "factor\\(year\\)"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          covariate.labels = c("Marxist ROP",
                               "Non-Marxist IRR",
                               "Unemployment",
                               "Inflation",
                               "Trade",
                               "Subsidies"),
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          header = FALSE,
          no.space = TRUE)
#-----------------------------------------------------------------------------------------------

