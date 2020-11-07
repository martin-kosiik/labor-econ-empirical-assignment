
setwd("C:/Users/marti/OneDrive/Plocha/skola/cerge/labor_econ/labor-econ-empirical-assignment")
library(haven)
library(readxl)
library(tidyverse)
library(janitor)
library(estimatr)

main_data<- read_dta("RQdata.dta", ) %>% as_factor()
ur95 <- read_excel("ur95.xls")

main_data <- main_data %>% 
  left_join(ur95, by = "code") %>% 
  mutate(ln_y = log(c14),
         exper = e02b - a09 - 6,     # age - years of schooling - 6
         exper_sq = exper^2,
         has_child = (e03 > 0)*1,
         more_than_one_child = (e03 > 1)*1,
         prague = (code == 3100) * 1)  %>% 
  rename(schooling_years = a09)

# Check number of missing values for each column
main_data %>% 
  map_int(~ sum(is.na(.)))

# Variable c14 has 101 missing values, c13 22, and c06 18


# drop the missing data 
main_data <- main_data %>% 
  filter(!is.na(c14), !is.na(c13), !is.na(c06), !is.na(ln_y)) #%>%   map_int(~ sum(is.na(.)))


# 4)
# a) 
men_only <- main_data %>% filter(e02a == "Male")
full_time_men <- men_only %>% filter(c13 >= 6, c13 <= 10)
lm_robust(ln_y ~ schooling_years + exper + exper_sq, full_time_men)


# 6)

lm_robust(ln_y ~ as.factor(a05) + exper + exper_sq, full_time_men)

# 7)
# a)
lm_robust(ln_y ~ has_child + as.factor(a05) + exper + exper_sq, full_time_men)

# b)
lm_robust(ln_y ~ I(more_than_one_child - has_child)+ as.factor(a05) + exper + exper_sq, full_time_men)

# 8)
# a, b)
lm_robust(ln_y ~ schooling_years*prague + exper + exper_sq, full_time_men)
#c)
lm_robust(ln_y ~ schooling_years + exper + exper_sq + urate, full_time_men)


