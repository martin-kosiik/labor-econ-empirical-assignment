dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)

library(haven)
library(readxl)
library(tidyverse)
#library(janitor)
library(estimatr)
library(texreg)
library(oaxaca)
library(REAT)
library(xtable)


main_data<- read_dta("data/RQdata.dta" ) %>% as_factor()
ur95 <- read_excel("data/ur95.xls")



main_data <- main_data %>% 
  left_join(ur95, by = "code") %>% 
  mutate(ln_y = log(c14),
         exper = e02b - a09 - 6,     # age - years of schooling - 6
         exper_sq = exper^2,
         any_children = (e03 > 0)*1,
         one_child = (e03 == 1)*1,
         more_than_one_child = (e03 > 1)*1,
         prague = (code == 3100) * 1,
         edu_level = fct_recode(as.factor(a05), 'No school education' = '7',
                                                'Primary' = '1',
                                                'Apprenticeship (2 years)' = '2',
                                                'Apprenticeship (3 - 4 years)' = '3',
                                                'Secondary vocational with GCE' = '4',
                                                'Grammar school with GCE' = '5',
                                                'Higher education' = '6'),
         female = (e02a == "Female")*1)  %>% 
  rename(schooling_years = a09)

#main_data %>% 
#  filter(exper <0) %>% 
#  View()

# Check number of missing values for each column
main_data %>% 
  map_int(~ sum(is.na(.)))

# Variable c14 has 101 missing values, c13 22, and c06 18

# before dropping missing data, we have 3837 obs.


# drop the missing data 
main_data <- main_data %>% 
  filter(!is.na(c14), !is.na(c13), !is.na(c06), !is.na(ln_y), exper > -1) #%>%   map_int(~ sum(is.na(.)))

main_data %>% 
  write_csv('data/main_data.csv')

# 3)

main_data_summary <-
  main_data %>%
  dplyr::select(ln_y, c14,  exper, exper_sq, schooling_years, prague, any_children, urate) %>% 
  rename(earnings = c14) %>% 
  # Keep numeric variables
  select_if(is.numeric) %>%
  # gather variables
  gather(variable, value) %>%
  # Summarize by variable
  group_by(variable) %>%
  # summarise all columns
  summarise(n = sum(!is.na(value)),
            `Mean` = mean(value, na.rm = TRUE),
            `Std. Dev.` = sd(value, na.rm = TRUE),
            `Median` = median(value, na.rm = TRUE),
            `Min.` = min(value, na.rm = TRUE),
            `Max.` = max(value, na.rm = TRUE)) 
  
main_data_summary %>% 
  xtable(caption = "Descriptive statistics") %>% 
  print(caption.placement = "top", file = "tables/descriptive_stats.tex")

# 4)
# a) 
men_only <- main_data %>% filter(e02a == "Male")

men_only <- men_only %>% 
  mutate(full_time = ifelse((c13 >= 6) & (c13 <= 10), 1, 0))

summary(lm_robust(schooling_years ~ full_time, data = men_only))
summary(lm_robust(ln_y ~ full_time, data = men_only))

full_time_men <- men_only %>% filter(c13 >= 6, c13 <= 10)
basic_model <- lm_robust(ln_y ~ schooling_years + exper + exper_sq, full_time_men)

# 5)
# a)
# We might want to control for main activity of the bussiness since it might affect the earnings

remove_factor <- function(lm_model, factor_name = 'edu_level'){
  lm_model$term <- lm_model$term %>%  str_remove(str_c('^', factor_name))
  return(lm_model)
  
}

extended_model <- lm_robust(ln_y ~ schooling_years + exper + exper_sq + c06, full_time_men) %>% remove_factor('c06')


texreg(list(basic_model, extended_model), caption = 'Basic and extended models', file = 'tables/extended_model.tex',
       caption.above = TRUE, include.ci = FALSE) 


# c)
u2 <- (full_time_men$ln_y - basic_model$fitted.values)^2
y <- basic_model$fitted.values
Ru2<- summary(lm(u2 ~ y + I(y^2)))$r.squared
LM <- nrow(full_time_men)*Ru2
p.value <- 1-pchisq(LM, 2)
p.value
# We reject null hypothesis of homoskeasticity


# g)

full_time_men %>% 
  dplyr::select(ln_y, schooling_years, exper, exper_sq) %>% 
  write_csv('data/full_time_men.csv')

# f)
max_earn_exp <- -extended_model$coefficients['exper']/(2 * extended_model$coefficients['exper_sq'])



# 6)


edu_level_basic <- lm_robust(ln_y ~ edu_level + exper + exper_sq, full_time_men) %>% remove_factor()
edu_level_basic
((exp(edu_level_basic$coefficients[2]) - 1) /2) * 100
((exp(edu_level_basic$coefficients[3]) - 1) /3.5) * 100
((exp(edu_level_basic$coefficients[4]) - 1) /4) * 100
((exp(edu_level_basic$coefficients[5]) - 1) /4) * 100
((exp(edu_level_basic$coefficients[6]) - 1) /5) * 100


# 7)
# a)
one_child <- lm_robust(ln_y ~ any_children + edu_level + exper + exper_sq, full_time_men) %>% remove_factor()


# b)
one_vs_more <- lm_robust(ln_y ~ more_than_one_child + one_child + edu_level + exper + exper_sq, full_time_men) %>% 
  remove_factor()
texreg(list(edu_level_basic, one_child, one_vs_more), caption = 'Effects of children', file = 'tables/children_effects.tex',
       caption.above = TRUE, include.ci = FALSE) 

# H_0: coeff on   more_than_one_child  == has_child

R <- matrix(c(0,1,-1, rep(0, 7)), ncol = 10)
coef_mat <- one_vs_more$coefficients['more_than_one_child'] - one_vs_more$coefficients['one_child']

wald_stat <- t(coef_mat) %*% solve(R %*% one_vs_more$vcov %*% t(R)) %*% coef_mat
p_val <- 1 - pchisq(wald_stat, 1)


# 8)
# a)
replace_inter_sym <- function(lm_model, repl_with = '*'){
  lm_model$term <- lm_model$term %>%  str_replace(':', ' * ')
  return(lm_model)
}

prague_reg <- lm_robust(ln_y ~ schooling_years*prague + exper*prague + exper_sq*prague, full_time_men) %>% 
  replace_inter_sym()

#b)

R <- diag(c(rep(0, 2), 1, rep(0, 2), rep(1, 3)))[c(3,6:8), ]

coef_mat <-  matrix(prague_reg$coefficients)[c(3 ,6:8)]

wald_stat <- t(coef_mat) %*% solve(R %*% prague_reg$vcov %*% t(R)) %*% coef_mat
p_val <- 1 - pchisq(wald_stat, 4)


#c)
unemp_rate_reg <- lm_robust(ln_y ~ urate + schooling_years + exper + exper_sq, full_time_men)
texreg(list(prague_reg, unemp_rate_reg), caption = 'Prague and regional unemployment effect',
       file = 'tables/prague_reg_unemp.tex', caption.above = TRUE, include.ci = FALSE) 



# 9)

gender_inter <- lm_robust(ln_y ~ schooling_years*female + exper*female + exper_sq*female, main_data) %>% 
  replace_inter_sym()


main_data %>% 
  group_by(female) %>% 
  summarise(mean = mean(ln_y), sch = mean(schooling_years))

o_decomp <- oaxaca(ln_y ~ schooling_years + exper + exper_sq| female , data = main_data, R = 1000)


plot(o_decomp, decomposition = "twofold", group.weight = -1)

# 10)
# a)
gini_male <- gini(main_data[main_data$female == 0, ]$c14)
gini_female <- gini(main_data[main_data$female == 1, ]$c14)

decile_ratio <- function(x){
  deciles <- quantile(x, na.rm = TRUE, probs = c(0.1, 0.9)) %>% unname()
  return(deciles[2]/deciles[1])
  
}

dec_ratio_male <- decile_ratio(main_data[main_data$female == 0, ]$c14)
dec_ratio_female <- decile_ratio(main_data[main_data$female == 1, ]$c14)


# b)
by_households <- main_data %>% 
  group_by(identa) %>% 
  summarise(c14 = mean(c14, na.rm = TRUE))

gini_hh <- gini(by_households$c14)
dec_hh <- decile_ratio(by_households$c14)

