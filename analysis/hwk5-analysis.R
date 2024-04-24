
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 5 Answers
## Author:        Ian McCarthy
## Date Created:  4/13/2020
## Date Edited:   4/24/2024
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               modelsummary, kableExtra, broom, cobalt, fixest)


# Read data and set workspace for knitr -------------------------------
final.data <- read_tsv('data/acs_medicaid.txt')

final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop) %>%
  filter(! State %in% c("Puerto Rico", "District of Columbia"))



# Create objects for markdown ---------------------------------------------


## Summary, Q1 - Share of direct purchase
direct.plot <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Direct Purchase",
    title="Share of Direct Purchase Insurance over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")


## Summary, Q3 - Share of medicaid
medicaid.plot <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Medicaid",
    title="Share of Medicaid Insurance over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")


## Summary, Q4 - Share uninsured
ins.plot.dat <- final.data %>% filter(is.na(expand_year) | expand_year==2014) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

uninsurance.plot <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )


## ATE, Q1 - DD Table
dd.table <- final.data %>% 
  filter(is.na(expand_year) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%  
  group_by(expand_ever, year) %>%
  summarize(uninsured=mean(perc_unins))

dd.table <- pivot_wider(dd.table, names_from="year", names_prefix="year", values_from="uninsured") %>% 
  ungroup() %>%
  mutate(expand_ever=case_when(
    expand_ever==FALSE ~ 'Non-expansion',
    expand_ever==TRUE ~ 'Expansion')
  ) %>%
  rename(Group=expand_ever,
         Pre=year2012,
         Post=year2015)

## ATE, Q2/3 - DD regression estimates, 2014 expansion only (with and without state/year fixed effects)
reg.data <- final.data %>% mutate(post=(year>=2014),
                                  treat=post*expand_ever) %>%
  filter(is.na(expand_year) | expand_year==2014)

dd.est <- lm(perc_unins~post + expand_ever + treat, data=reg.data)
fe.est <- feols(perc_unins~treat | State + year, data=reg.data)

## ATE, Q4 - DD with time varying treatment
reg.data2 <- final.data %>% 
  mutate(treat=case_when(
    year>=expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    year<expand_year & !is.na(expand_year) ~ 0)
  )
fe.est2 <- feols(perc_unins~treat | State + year, data=reg.data2)

## ATE, Q5 - Event study with constant treatment
mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.data)



## ATE, Q6 - Event study with time varying treatment
reg.data2 <- reg.data2 %>%
  mutate(time_to_treat=ifelse(expand_ever==TRUE, year-expand_year, -1),
         time_to_treat=ifelse(time_to_treat<=-4, -4, time_to_treat))

mod.twfe2 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.data2)



rm(list=c("final.data","ins.plot.dat"))
save.image("analysis/Hwk5_workspace.Rdata")



