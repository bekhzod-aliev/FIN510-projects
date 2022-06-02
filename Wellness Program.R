haves# Problem 3 Case-Code
library(haven)
library(estimatr)
library(dplyr)
my_data <- read_dta("https://reifjulian.github.io/illinois-wellness-data/data/stata/claims.dta")

# 3.How many employees were in the treatment group?
sum(my_data$treat == 1)
# and how many in the control group?
sum(my_data$treat == 0)
# Of those in the treatment group, how many participated in the initial 
#(screening) segment of the wellness program in the first year?
sum(my_data$hra_c_yr1 == 1, na.rm=TRUE)

# 4.
names(my_data)
#linear regression for covg_0715_0716
summary(lm(covg_0715_0716 ~ as.factor(treat), my_data))

#linear regression for nonzero_spend_0715_0716
summary(lm(nonzero_spend_0715_0716 ~ as.factor(treat), my_data))

#linear regression for spendHosp_0715_0716
summary(lm(spendHosp_0715_0716 ~ as.factor(treat), my_data))

#linear regression for spendOff_0715_0716
summary(lm(spendOff_0715_0716 ~ as.factor(treat), my_data))

#linear regression for spendRx_0715_0716
summary(lm(spendRx_0715_0716 ~ as.factor(treat), my_data))

#linear regression for spend_0715_0716
summary(lm(spend_0715_0716 ~ as.factor(treat), my_data))

#Control and treatment group mean for variable covg_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(covg_0715_0716)) 

#Control and treatment group mean for variable nonzero_spend_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(nonzero_spend_0715_0716, na.rm = TRUE))

#Control and treatment group mean for variable spendHosp_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(spendHosp_0715_0716, na.rm = TRUE))

#Control and treatment group mean for variable spendOff_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(spendOff_0715_0716, na.rm = TRUE))

#Control and treatment group mean for variable spendRx_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(spendRx_0715_0716, na.rm = TRUE))

#Control and treatment group mean for variable spend_0715_0716
my_data %>%
  group_by(treat) %>%
  summarize(mean(spend_0715_0716, na.rm = TRUE))

# 5.
#linear regression betwwen T/C without demographic controls for covg_0816_0717
summary(lm(covg_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for covg_0816_0717
summary(lm(covg_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))

#linear regression betwwen T/C without demographic controls for nonzero_spend_0816_0717
summary(lm(nonzero_spend_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for nonzero_spend_0816_0717
summary(lm(nonzero_spend_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))

#linear regression betwwen T/C without demographic controls for spendHosp_0816_0717
summary(lm(spendHosp_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for spendHosp_0816_0717
summary(lm(spendHosp_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))

#linear regression betwwen T/C without demographic controls for spendOff_0816_0717
summary(lm(spendOff_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for spendOff_0816_0717
summary(lm(spendOff_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))

#linear regression betwwen T/C without demographic controls for spendRx_0816_0717
summary(lm(spendRx_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for spendRx_0816_0717
summary(lm(spendRx_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))

#linear regression betwwen T/C without demographic controls for spend_0816_0717
summary(lm(spend_0816_0717 ~ treat, my_data))
#linear regression betwwen T/C with demographic controls for spend_0816_0717
summary(lm(spend_0816_0717 ~ treat + male + age50 + age37_49 + white, 
           my_data))


# 6.
#for covg_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(covg_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for covg_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(covg_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))

#for nonzero_spend_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(nonzero_spend_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for nonzero_spend_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(nonzero_spend_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))

#for spendHosp_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(spendHosp_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for spendHosp_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(spendHosp_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))


#for spendOff_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(spendOff_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for spendOff_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(spendOff_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))


#for spendRx_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(spendRx_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for spendRx_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(spendRx_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))


#for spend_0816_0717 estimated difference between participants and non-participants (no demographic controls)
summary(lm(spend_0816_0717 ~ as.factor(hra_c_yr1), my_data))
#for spend_0816_0717 estimated difference between participants and non-participants (with demographic controls)
summary(lm(spend_0816_0717 ~ as.factor(hra_c_yr1) + male + age50 + age37_49 + white, my_data))










