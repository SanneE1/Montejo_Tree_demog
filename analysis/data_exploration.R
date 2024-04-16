library(MASS)
library(tidyverse)
library(lme4)
library(caret)
library(leaps)
library(glmnet)


dbh_xls <- readxl::read_xlsx("data/datos temporales.xlsx", sheet = "Diameters") %>%
  rename(ID = 'ID Tree',
         UBI = "UBI (Localization on plot)",
         dbh_2005 = "DBH 2005 (cm)",
         vig_2005 = "FITNESS (VIG) 2005",
         dbh_2015 = "DBH 2015 (cm)",
         vig_2015 = "FITNESS (VIG) 2015",
         dbh_2021 = 'DBH 21/22                           (0 = Dead; White = NA)') %>% 
  select(-c('...11', '...12', '...13', '...14', '...15')) %>% # These columns are added because there's a single number in column 15, which is a repeat of that rows dbh in year 2015
  mutate(dbh_2021 = as.numeric(dbh_2021),
         tree_id = paste(Zone, PLOT, ID, SP, UBI, sep = "_"))  

plot_xls <- readxl::read_xlsx("data/datos temporales.xlsx", sheet = "Env.var", skip = 1) %>%
  rename(PLOT = `...1`,
         slope = `Slo (%)...4`,
         aspect = `Asp (º)`,
         altitude = `Alt (m)`,
         soilDepth = `Sdep (m)`,
         orgMat = `% OM`,
         nitrogen = `% N`,
         orgCarbon = `% C-org`,
         totCarbon = `% C-tot`
         ) %>%
  select(-c(`...12`, `Slo (%)...13`, `Slope (%)`))

trait_xls <- readxl::read_xlsx("data/datos temporales.xlsx", sheet = "Traits",n_max = 17) %>%
  select(-c(`Total individuals in FI`, "Specie")) %>%
  mutate(across(!"Abr", as.numeric)) %>%
  rename(SP = Abr)
  

dbh_xls <- left_join(dbh_xls, plot_xls) %>%
  left_join(., trait_xls)


recr_xls <- readxl::read_xlsx("data/datos temporales.xlsx", sheet = "Recruitment") %>%
  rename(SP = Specie,
         height_t0 = `Plant height (m)`,
         first_yr = `Ind. 1er year    (1 = yes/0 = no)`,
         year_t0 = `Year of measurment`)
  


# ---------------------------------------------------------------------------------------------
# Transform data to long format 
# ---------------------------------------------------------------------------------------------


df_0515 <- dbh_xls %>%
  select(-c(dbh_2021)) %>%
  rename(dbh_t0 = dbh_2005,
         dbh_t1 = dbh_2015) %>%
  mutate(year_t0 = 2005,
         year_t1 = 2015,
         dbh_t0 = case_when(vig_2005 != 1 ~ NA,
                            is.na(vig_2005) ~ NA,
                            TRUE ~ dbh_t0),
         dbh_t1 = case_when(vig_2015 != 1 ~ NA,
                            is.na(vig_2015) ~ NA,
                            TRUE ~ dbh_t1),
         surv_t1 = case_when(!is.na(dbh_t0) & !is.na(dbh_t1) ~ 1,
                             !is.na(dbh_t0) & is.na(dbh_t1) ~ 0,
                             TRUE ~ NA),
         ) %>% 
  select(-c("UBI", contains("vig"), "X", "Y"))

df_1521 <- dbh_xls %>%
  select(-c(dbh_2005, vig_2005)) %>%
  rename(dbh_t0 = dbh_2015,
         dbh_t1 = dbh_2021) %>%
  mutate(year_t0 = 2015,
         year_t1 = 2021,
         dbh_t0 = case_when(vig_2015 != 1 ~ NA,
                            is.na(vig_2015) ~ NA,
                            TRUE ~ dbh_t0),
         dbh_t1 = case_when(dbh_t1 > 0 ~ dbh_t1,
                            TRUE ~ NA),
         surv_t1 = case_when(!is.na(dbh_t0) & !is.na(dbh_t1) ~ 1,
                             !is.na(dbh_t0) & is.na(dbh_t1) ~ 0,
                             TRUE ~ NA),
  ) %>%
  select(-c("UBI", contains("vig"), "X", "Y"))


dbh_df <- rbind(df_0515, df_1521) %>% 
  arrange(tree_id) %>%
  mutate(dbh_t0 = log(dbh_t0),
         dbh_t1 = log(dbh_t1),
         duration = year_t1 - year_t0, 
         temp = case_when(year_t0 == 2005 ~ 0.239,
                          year_t0 == 2015 ~ 0.648,
                          TRUE ~ NA),
         precip = case_when(year_t0 == 2005 ~ 0.00794,
                            year_t0 == 2015 ~ -0.0446,
                            TRUE ~ NA))


# ---------------------------------------------------------------------------------------------
# Data Stats 
# ---------------------------------------------------------------------------------------------

b <- dbh_df %>% group_by(Zone, PLOT) %>% summarize(SP = n_distinct(SP), ID = n_distinct(ID))
a <- dbh_df %>% group_by(Zone) %>% summarize(SP = n_distinct(SP), ID = n_distinct(ID))

n_distinct(dbh_df$ID)

nrow(recr_xls)
nrow(recr_xls %>% filter(first_yr == 1))

recr_xls %>% group_by(SP) %>% summarise(n = n())

hist(log(recr_xls$height_t0), xlab = "height (log(meter)) of new recruits", main = "")

# ---------------------------------------------------------------------------------------------
# Survival 
# ---------------------------------------------------------------------------------------------

surv_0515 <- glm(surv_t1 ~ dbh_t0, data = dbh_df %>% filter(year_t0 == 2005), family = "binomial" )
surv_1521 <- glm(surv_t1 ~ dbh_t0, data = dbh_df %>% filter(year_t0 == 2015), family = "binomial" )

pred_df <- data.frame(dbh_t0 = seq(from= min(dbh_df$dbh_t0, na.rm = T), to= max(dbh_df$dbh_t0, na.rm = T), length.out = 100)) 
pred_df <- pred_df %>%
  mutate(`2005` = predict(surv_0515, newdata = pred_df, type = "response"),
         `2015` = predict(surv_1521, newdata = pred_df, type = "response")) %>%
  pivot_longer(c(`2005`, `2015`), names_to = "t0", values_to = "surv_predicted") %>%
  mutate(surv_annual = case_when(t0 == 2005 ~ surv_predicted^(1/10),
                            t0 == 2015 ~ surv_predicted^(1/6),
                            TRUE ~ NA))


ggplot(pred_df) + 
  geom_line(aes(x = dbh_t0, y = surv_annual, colour = t0, linetype = 'annual'), size = 1) +
  geom_line(aes(x = dbh_t0, y = surv_predicted, colour = t0, linetype = 'total'), size = 1) +
  ylab("Survival rates") + xlab("log(DBH) at t0") + 
  scale_colour_manual(name = "Years", 
                      values = c("2005", "2015"),
                      labels = c("2005-2015", "2015-2021")) +
  scale_linetype_manual(name = "Type",
                        values = c('solid','dashed'),
                        labels = c("Average annual survival", "Total period survival")) +
  theme_bw() + theme(text = element_text(size = 16))

# ## Cox proportional hazard rate
# 
# library(survival)
# library(survminer)
# 
# cox_model <- coxph(Surv(time = duration, event = surv_t1) ~ dbh_t0 + year_t0, data = dbh_df)
# 
# summary(cox_model)
# 
# ggsurvplot(survfit(cox_model), data = dbh_df)


## Stepwise regression
surv.step.full <- stepAIC(glm(surv_t1 ~ dbh_t0 + duration + SP * Zone + PLOT + 
                                slope + aspect + altitude + orgMat + nitrogen + orgCarbon + totCarbon + soilDepth + 
                                LDMC + SLA + Thickness + SDMC + HubVal + RDMC + SRL + SRA + TMDr + Rdi +d13C + LC + LN + LCN + RC + RN + RCN +
                                temp + precip, 
                              data = dbh_df[complete.cases(dbh_df),]), 
                          direction = "both", family = "binomial", trace = FALSE)
summary(surv.step.full)



# ---------------------------------------------------------------------------------------------
# Growth 
# ---------------------------------------------------------------------------------------------

growth_0515 <- glm((dbh_t1 - dbh_t0) ~ dbh_t0, data = dbh_df %>% filter(year_t0 == 2005 & surv_t1 == 1))
growth_1521 <- glm((dbh_t1 - dbh_t0) ~ dbh_t0, data = dbh_df %>% filter(year_t0 == 2015 & surv_t1 == 1))

pred_df <- pred_df %>%
  mutate(growth_predicted = case_when(t0 == 2005 ~ predict(growth_0515, newdata = pred_df, type = "response"),
                                      t0 == 2015 ~ predict(growth_1521, newdata = pred_df, type = "response"),
                                      TRUE ~ NA),
         growth_annual = case_when(t0 == 2005 ~ growth_predicted/10,
                                   t0 == 2015 ~ growth_predicted/6,
                                   TRUE ~ NA))
ggplot() + 
  geom_point(data = dbh_df, aes(x = dbh_t0, y = (dbh_t1 - dbh_t0)), alpha = 0.1) +
  geom_line(data = pred_df, aes(x = dbh_t0, y = growth_predicted, colour = t0, linetype = "dashed"), size = 1) +
  geom_line(data = pred_df, aes(x = dbh_t0, y = growth_annual, colour = t0, linetype = "solid"), size = 1) +
  ylab("Average annual growth (cm)") + xlab("DBH (cm) at first census") + 
  scale_colour_manual(name = "Periods", 
                      values = c("2005", "2015"),
                      labels = c("2005-2015", "2015-2021")) +
  scale_linetype_manual(name = "Type",
                        values = c('solid','dashed'),
                        labels = c("Average annual growth", "Total period growth")) +
  theme_bw() + theme(text = element_text(size = 16))



## Stepwise regression
grow.step.full <- stepAIC(lm(dbh_t1 ~ dbh_t0 + duration + SP * Zone + PLOT + 
                                slope + aspect + altitude + orgMat + nitrogen + orgCarbon + totCarbon + soilDepth + 
                                LDMC + SLA + Thickness + SDMC + HubVal + RDMC + SRL + SRA + TMDr + Rdi +d13C + LC + LN + LCN + RC + RN + RCN +
                                temp + precip, 
                              data = dbh_df[complete.cases(dbh_df),]), 
                          direction = "both", trace = FALSE)
summary(grow.step.full)












