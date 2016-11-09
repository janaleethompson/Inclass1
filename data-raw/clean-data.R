library(foreign)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)



library(foreign)
person_99 <- read.dbf("yearly_person_data/person_99")
person_00 <- read.dbf("yearly_person_data/person_00")
person_01 <- read.dbf("yearly_person_data/person_01")
person_02 <- read.dbf("yearly_person_data/person_02")
person_03 <- read.dbf("yearly_person_data/person_03")
person_04 <- read.dbf("yearly_person_data/person_04")
person_05 <- read.dbf("yearly_person_data/person_05")
person_06 <- read.dbf("yearly_person_data/person_06")
person_07 <- read.dbf("yearly_person_data/person_07")
person_08 <- read.dbf("yearly_person_data/person_08")
person_09 <- read.dbf("yearly_person_data/person_09")
person_10 <- read.dbf("yearly_person_data/person_10")


year <- c("99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10")


#create function
#create loop- add in function  
study_year = 1999
clean_yearly_person_file <- function(study_year){  
  study_year <- stringr::str_sub(study_year, -2)
  df <- read.dbf(paste0("yearly_person_data/person_", study_year))
   
  if (study_year == "99") {                                   
    df <- dplyr::mutate(df, study_year = paste0("19", study_year))     
  } else {                                                
    df <- dplyr::mutate(df, study_year = paste0("20", study_year))     
  }                                                      
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>%  
    
    dplyr::select(state, age, sex, per_typ, inj_sev, alc_res, drugres1, 
                  drugres2, drugres3, lag_hrs, lag_mins, st_case, study_year, 
                  per_no, veh_no) %>%
    
    dplyr::filter(per_typ == 1) %>%
    
    dplyr::select(-per_typ) %>%
    
    tidyr::unite(unique_id, state, veh_no, per_no, st_case, study_year, remove = FALSE) %>%
    
    dplyr::filter(state %in% c(6, 15, 17, 33, 44, 54)) %>%
    
    dplyr::mutate(state = factor(state, levels = c(6, 15, 17, 33, 44, 54),
                          labels = c("California", "Hawaii", "Illinois", "New Hampshire", 
                                     "Rhode Island", "West Virginia"))) %>%
    
    dplyr::select(-state) %>%
    
    dplyr::mutate(sex = ifelse(sex == 1, "Male", sex), 
           sex = ifelse(sex == 2, "Female", sex), 
           sex = ifelse(sex == 9, NA, sex),
           sex = factor(sex)) %>%
    
    dplyr::mutate(study_year = as.integer(study_year)) %>%
    
    dplyr::mutate(alc_res = ifelse(alc_res == 95, NA, alc_res),
    alc_res = ifelse(alc_res == 96, NA, alc_res),
    alc_res = ifelse(alc_res == 97, NA, alc_res),
    alc_res = ifelse(alc_res == 98, NA, alc_res),
    alc_res = ifelse(alc_res == 99, NA, alc_res)) %>%
    
    dplyr::mutate(Alcohol = ifelse(alc_res > 0, TRUE, FALSE))
    
    gathered_df <- df %>%
    tidyr::gather(drug_number, drug_type_raw, dplyr::contains("drugres")) %>%
    dplyr::mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                                     "Narcotic", NA),
                  drug_type = ifelse(drug_type_raw %in% 300:395,
                                     "Depressant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 400:495,
                                     "Stimulant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 600:695,
                                     "Cannabinoid", drug_type),
                  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                                     "Other", drug_type),
                  drug_type = ifelse(drug_type_raw == 1,
                                     "None", drug_type),
                  drug_type = factor(drug_type)) %>%
          dplyr::select(-drug_type_raw, -drug_number) %>%
    
      dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))
    
    non_missing_drugs <- gathered_df %>%
    dplyr::filter(!is.na(drug_type)) %>%
    dplyr::group_by(unique_id, drug_type) %>%
    dplyr::summarize(has_drug = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_num = 1:n()) %>%
    tidyr::spread(drug_type, has_drug, fill = FALSE) %>%
    dplyr::select(-row_num) %>%
    dplyr::group_by(unique_id) %>%
    dplyr::summarize(Cannabinoid = any(Cannabinoid),
              Depressant = any(Depressant),
              Narcotic = any(Narcotic),
              Other = any(Other),
              Stimulant = any(Stimulant)) %>%
    dplyr::ungroup()
  df <- df %>%
    dplyr::select(-dplyr::contains("drugres")) %>%
    dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
    tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    dplyr::mutate(drug_type = factor(drug_type)) %>%
    
    dplyr::mutate(lag_hrs = ifelse(lag_hrs == 999, NA, lag_hrs),
                        ifelse(lag_hrs == 99, NA, lag_hrs),
                        ifelse(lag_hrs == 88, NA, lag_hrs)) %>%
      dplyr::filter((lag_hrs == 1 & lag_mins == 0) | lag_hrs < 1) %>%
            
      dplyr::select(-lag_hrs, -lag_mins) %>%
    
    dplyr::mutate(age, ifelse(age == 99, NA, age),
           ifelse(age == 999, NA, age)) %>%
    
    dplyr::mutate(agecat = cut(age, breaks = c(0, 25, 44, 64, 98),
                  labels = c("< 25 years", "25--44 years", "45--64 years", "65 years +"))) %>%
    dplyr::select(-age) %>%
    
    dplyr::select(unique_id, sex, study_year, agecat, drug_type, positive_for_drug)
    return(df)
}

data_99 <- clean_yearly_person_file(1999)
data_99 %>%
  group_by(drug_type) %>%
  slice(1:3)

# After the df function is working correctly, we should be 
# able to run this loop (code from the homework pdf) to apply the function to 
# each year, and paste each year's dataframe together. 

for(year in 1999:2010){
  df <- clean_yearly_person_file(year)
  
  if(year == 1999){
    clean_fars <- df
  } else {
    clean_fars <- rbind(clean_fars, df)
  }
}
save(clean_fars, file = "../data/clean_fars.RData") 

load("../data/clean_fars.RData")
dim(clean_fars)
length(unique(clean_fars$unique_id))
summary(clean_fars)


    




              

              
        
                
                

              
              
              
            