library(tidyverse)
setwd("E:/STUDY/FIN 510/Problem Set 2")
mortality <- haven::read_dta("mortality.dta") 


# A. 
# Treatment: "above MDA"
# Outcome: rates of death "ROD" in units of deaths per 100,000 person-years
mortality <- mortality %>% 
  mutate(post = agemo_mda > 0,
         rod_any = cod_any * 100000 / (pop / 12),
         rod_MVA = cod_MVA * 100000 / (pop / 12))

# mortality rates due to any cause for individuals who are 1-24 months
# above the MLDA
  mortality %>%
  filter(between(agemo_mda, 1, 24)) %>% 
  summarise(mean(rod_any))

# mortality rates due to any cause for individuals who are 1-24 months
# below the MLDA
  mortality %>%
  filter(between(agemo_mda, -24, -1)) %>% 
  summarise(mean(rod_any))



# B
# Scatter plot of ROD by age
filter(mortality, between(agemo_mda, -24, 24)) %>% 
  ggplot(aes(x = agemo_mda)) + 
  geom_point(aes(y = rod_any), color = "black") + 
  geom_point(aes(y = rod_MVA), color = "blue") +
  geom_vline(xintercept = 0)
  


# C.Non-parametric "donut" RD.
# 48 bandwidth all-cause mortality
rd_any48 <- lm(rod_any ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(rd_any48)

# 48 bandwidth motor vehicle accidents
rd_mva48 <- lm(rod_MVA ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(rd_mva48)

# 24 bandwidth all-cause mortality
rd_any24 <- lm(rod_any ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(rd_any24)

# 24 bandwidth motor vehicle accidents
rd_mva24 <- lm(rod_MVA ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(rd_mva24)

# 12 bandwidth all-cause mortality
rd_any12 <- lm(rod_any ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(rd_any12)

# 12 bandwidth motor vehicle accidents
rd_mva12 <- lm(rod_MVA ~ post, 
               data = mortality, 
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(rd_mva12)

# 6 bandwidth all-cause mortality
rd_any6 <- lm(rod_any ~ post, 
              data = mortality, 
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(rd_any6)

# 6 bandwidth motor vehicle accidents
rd_mva6 <- lm(rod_MVA ~ post, 
              data = mortality, 
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(rd_mva6)


# D.Parametric "donut" RD.
# 48 bandwidth all-cause mortality
lm(rod_any ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -48, 48) & agemo_mda != 0)


# 48 bandwidth motor vehicle accidents
lm(rod_MVA ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -48, 48) & agemo_mda != 0)

# 24 bandwidth all-cause mortality
lm(rod_any ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -24, 24) & agemo_mda != 0)


# 24 bandwidth motor vehicle accidents
lm(rod_MVA ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -24, 24) & agemo_mda != 0)

# 12 bandwidth all-cause mortality
lm(rod_any ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -12, 12) & agemo_mda != 0)

# 12 bandwidth motor vehicle accidents
lm(rod_MVA ~ post * agemo_mda, 
                data = mortality, 
                subset = between(agemo_mda, -12, 12) & agemo_mda != 0)

# 6 bandwidth all-cause mortality
lm(rod_any ~ post * agemo_mda, 
               data = mortality, 
               subset = between(agemo_mda, -6, 6) & agemo_mda != 0)

# 6 bandwidth motor vehicle accidents
lm(rod_MVA ~ post * agemo_mda, 
               data = mortality, 
               subset = between(agemo_mda, -6, 6) & agemo_mda != 0)

