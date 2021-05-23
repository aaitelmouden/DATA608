# DATA608 Project | Abdellah Ait Elmouden
# The global file will be used to import, and clean the DATA

library(shiny)
library(shinydashboard)
library(dygraphs)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(bslib)

####################################################
# Load the Data and Clean it
###################################################

death = read.csv("./data/nyc_deaths.csv") %>% 
    janitor::clean_names() %>% 
    filter(
        race_ethnicity != "Not Stated/Unknown",
        race_ethnicity != "Other Race/ Ethnicity",
        leading_cause != "All Other Causes"
    ) %>% 
    mutate(
        deaths = as.numeric(deaths),
        death_rate = as.numeric(death_rate),
        age_adjusted_death_rate = as.numeric(age_adjusted_death_rate))

####################################################
# Rename cause of death and Sex labels
####################################################

death = 
    death %>% 
    mutate(
        sex = str_replace(sex, "Female", "F"),
        sex = str_replace(sex, "Male", "M"),
        leading_cause = str_replace_all(leading_cause, "[//(//)]", ""),
        leading_cause = str_replace(leading_cause, "Influenza Flu and Pneumonia J09-J18", "Influenza & Pneumonia"),
        leading_cause = str_replace(leading_cause, "Accidents Except Drug Posioning V01-X39, X43, X45-X59, Y85-Y86", "Accidents"),
        leading_cause = str_replace(leading_cause, "Cerebrovascular Disease Stroke: I60-I69", "Cerebrovascular Disease"),
        leading_cause = str_replace(leading_cause, "Assault Homicide: Y87.1, X85-Y09", "Assault"),
        leading_cause = str_replace(leading_cause, "Essential Hypertension and Renal Diseases (I10, I12)", "Hypertension & Renal Dis."),
        leading_cause = str_replace(leading_cause, "Human Immunodeficiency Virus Disease HIV: B20-B24", "HIV"),
        leading_cause = str_replace(leading_cause, "Diseases of Heart I00-I09, I11, I13, I20-I51", "Diseases of Heart"),
        leading_cause = str_replace(leading_cause, "Alzheimer's Disease G30", "Alzheimer's Disease"),
        leading_cause = str_replace(leading_cause, "Chronic Liver Disease and Cirrhosis K70, K73", "Chronic Liver Disease/Cirrhosis"),
        leading_cause = str_replace(leading_cause, "Malignant Neoplasms Cancer: C00-C97", "Malignant Neoplasms"),
        leading_cause = str_replace(leading_cause, "Diabetes Mellitus E10-E14", "Diabetes Mellitus"),
        leading_cause = str_replace(leading_cause, "Mental and Behavioral Disorders due to Accidental Poisoning and Other Psychoactive Substance Use F11-F16, F18-F19, X40-X42, X44", "Accidental Poisoning/Substance Use"),
        leading_cause = str_replace(leading_cause, "Septicemia A40-A41", "Septicemia"),
        leading_cause = str_replace(leading_cause, "Chronic Lower Respiratory Diseases J40-J47", "Chronic Lower Respiratory Dis."),
        leading_cause = str_replace(leading_cause, "Nephritis, Nephrotic Syndrome and Nephrisis N00-N07, N17-N19, N25-N27", "Nephritis"),
        leading_cause = str_replace(leading_cause, "Certain Conditions originating in the Perinatal Period P00-P96", "Perinatal Period Conditions"),
        leading_cause = str_replace(leading_cause, "Viral Hepatitis B15-B19", "Viral Hepatitis"),
        leading_cause = str_replace(leading_cause, "Intentional Self-Harm Suicide: X60-X84, Y87.0", "Suicide"),
        leading_cause = str_replace(leading_cause, "Congenital Malformations, Deformations, and Chromosomal Abnormalities Q00-Q99", "Congenital Malformations")
    )

#########################################################
# Export the cleaned Data

write.csv(death, "./data/nyc_death_cleaned.csv")

#########################################################
# Load the clean data to be used on our shiny App

nyc_mortality <- read.csv("./data/nyc_death_cleaned.csv")
########################################################

nyc_aadr <- nyc_mortality %>% group_by(leading_cause, race_ethnicity, year) %>% dplyr::summarize(Total_Age_adjusted = sum(age_adjusted_death_rate))