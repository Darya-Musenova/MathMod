library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tb1=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tb1=tb1[-1,] 
tb1  
tb1=tb1[tb1$DOY > 244 & tb1$DOY < 316,] 
tb1=tb1[tb1$daytime == FALSE,]
glimpse(tb1) 
tb1 = select(tb1, -(roll)) 
tb1 = tb1 %>% mutate_if(is.character, factor) 
names(tb1) = str_replace_all(names(tb1), "[!]","_emph_")
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tb1)
#????????? ?????? ????????? ?????? 
sapply(tb1,is.numeric)
tb1_numeric = tb1 [,sapply (tb1,is.numeric) ]
tb1_non_numeric = tb1[,!sapply(tb1,is.numeric) ]
cor_tb = cor(drop_na(tb1_numeric)) 
cor_tb
cor_tb = cor (drop_na(tb1_numeric))
cor_tb
cor_tb1 = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_tb1
vars = row.names(cor_tb1)[cor_tb1$co2_flux^2 > .2] %>% na.exclude 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
mod=lm(formula, data = tb1) 
anova(mod)  
summary(mod) 
formula1 = as.formula(paste("co2_flux ~ DOY + co2_flux + h2o_molar_density + h2o_mole_fraction + 
                            h2o_mixing_ratio + air_density + air_heat_capacity + 
                            e  + Tdew + un_co2_flux + 
                            h2o + h2o.1")) 
formula1 
mod2=lm(formula1, data = tb1) 
anova(mod2) 
summary(mod2) 
formula2 = as.formula(paste("co2_flux ~ DOY + co2_flux + h2o_molar_density + h2o_mole_fraction + 
                            h2o_mixing_ratio + air_density + air_heat_capacity + 
                            e + un_co2_flux + 
                            h2o + h2o.1")) 
mod3=lm(formula2, data = tb1) 
anova(mod3) 
summary(mod3) 
formula3 = as.formula(paste("co2_flux ~ co2_flux + h2o_mole_fraction + 
                            h2o_mixing_ratio + air_density + air_heat_capacity + 
                            e + un_co2_flux + 
                            h2o.1")) 
mod4=lm(formula3, data = tb1) 
anova(mod4) 
summary(mod4)
mod5 = lm(co2_flux ~ (co2_flux + h2o_mole_fraction + 
                            h2o_mixing_ratio + air_density + air_heat_capacity + 
                            e + un_co2_flux + h2o.1) ^2, data = tb1)
mod5
anova(mod5)
summary(mod5)
mod6 = lm(co2_flux ~ (co2_flux + h2o_mole_fraction + 
                        h2o_mixing_ratio + un_co2_flux + h2o.1) ^2, data = tb1)                 
mod6      
anova(mod6)
summary(mod6)
mod7 = lm(co2_flux ~ (co2_flux + h2o_mole_fraction + 
                            h2o_mixing_ratio + un_co2_flux + h2o.1+co2_flux:h2o_mole_fraction + 
                            co2_flux:h2o_mixing_ratio +co2_flux:un_co2_flux + co2_flux:h2o.1 + 
                            h2o_mole_fraction:un_co2_flux + h2o_mole_fraction:h2o.1 + 
                            h2o_mixing_ratio:un_co2_flux)- h2o_mixing_ratio:h2o.1 - un_co2_flux:h2o.1, data = tb1)  
anova(mod7)
summary(mod7)
mod8 = lm(co2_flux ~ (co2_flux + un_co2_flux + co2_flux:h2o_mole_fraction + 
                        co2_flux:h2o_mixing_ratio + h2o_mole_fraction:un_co2_flux  + 
                        h2o_mixing_ratio:un_co2_flux)- h2o_mixing_ratio:h2o.1 - un_co2_flux:h2o.1 - h2o_mole_fraction - 
                        h2o_mixing_ratio - h2o.1 - co2_flux:un_co2_flux - co2_flux:h2o.1 - h2o_mole_fraction:h2o.1, data = tb1) 
anova(mod8)
summary(mod8)

