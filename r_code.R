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


main_data<- read_dta("data/RQdata.dta", ) %>% as_factor()
ur95 <- read_excel("data/ur95.xls")

main_data <- main_data %>% 
  left_join(ur95, by = "code") %>% 
  mutate(ln_y = log(c14),
         exper = e02b - a09 - 6,     # age - years of schooling - 6
         exper_sq = exper^2,
         has_child = (e03 > 0)*1,
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

# Check number of missing values for each column
main_data %>% 
  map_int(~ sum(is.na(.)))

# Variable c14 has 101 missing values, c13 22, and c06 18

# drop the missing data 
main_data <- main_data %>% 
  filter(!is.na(c14), !is.na(c13), !is.na(c06), !is.na(ln_y)) #%>%   map_int(~ sum(is.na(.)))

# 3)

main_data_summary <-
  main_data %>%
  dplyr::select(ln_y, exper, exper_sq, schooling_years, prague, has_child, urate) %>% 
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
full_time_men <- men_only %>% filter(c13 >= 6, c13 <= 10)
basic_model <- lm_robust(ln_y ~ schooling_years + exper + exper_sq, full_time_men)

# 5)
# a)
# We might want to control for main activity of the bussiness since it might affect the earnings

extended_model <- lm_robust(ln_y ~ schooling_years + exper + exper_sq + c06, full_time_men)

extended_model$term <- str_replace(extended_model$term,"^c06", "")

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


# f)
max_earn_exp <- -extended_model$coefficients['exper']/(2 * extended_model$coefficients['exper_sq'])


# 6)

edu_level_basic <- lm_robust(ln_y ~ edu_level + exper + exper_sq, full_time_men)

# 7)
# a)
one_child <- lm_robust(ln_y ~ has_child + edu_level + exper + exper_sq, full_time_men)

# b)
one_vs_more <- lm_robust(ln_y ~ I(more_than_one_child - has_child)+ edu_level + exper + exper_sq, full_time_men)
texreg(list(edu_level_basic, one_child, one_vs_more), caption = 'Effects of children', file = 'tables/children_effects.tex',
       caption.above = TRUE, include.ci = FALSE) 
  

# 8)
# a, b)
prague_reg <- lm_robust(ln_y ~ schooling_years*prague + exper + exper_sq, full_time_men)
#c)
unemp_rate_reg <- lm_robust(ln_y ~ urate + schooling_years + exper + exper_sq, full_time_men)
texreg(list(prague_reg, unemp_rate_reg), caption = 'Prague and regional unemployment effect',
       file = 'tables/prague_reg_unemp.tex', caption.above = TRUE, include.ci = FALSE) 



# 9)

gender_inter <- lm_robust(ln_y ~ schooling_years*female + exper*female + exper_sq*female, main_data)


gender_inter$term <- str_replace(gender_inter$term,":", "*")

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

