library(foreign)
library(tidyverse)
library(stringr)
library(ggplot2)

getwd()


year <- c("99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
year

#create function
#create loop- add in function  
year = 99
clean_yearly_person_file <- function(year){  
  year <- stringr::str_sub(year, -2)
  df <- read.dbf(paste0("person_", year))     
  if (year == "99") {                                   
    df <- mutate(df, year = paste0("19", year))     
  } else {                                                
    df <- mutate(df, year = paste0("20", year))     
  }                                                      
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>%  
    
    dplyr::select(state, age, sex, per_typ, inj_sev, alc_res, drugres1, 
                  drugres2, drugres3, lag_hrs, lag_mins, st_case, year, 
                  per_no, veh_no) %>%
    
    filter(per_typ == 1) %>%
    
    select(-per_typ) %>%
    
    unite(unique_id, state, veh_no, per_no, st_case, year, remove = FALSE) %>%
    
    filter(state %in% c(6, 15, 17, 33, 44, 54)) %>%
    
    mutate(state = factor(state, levels = c(6, 15, 17, 33, 44, 54),
                          labels = c("California", "Hawaii", "Illonois", "New Hampshire", 
                                     "Rhode Island", "West Virginia"))) %>%
    
    select(-state) %>%
    
    mutate(sex = ifelse(sex == 1, "Male", sex), 
           sex = ifelse(sex == 2, "Female", sex), 
           sex = ifelse(sex == 9, NA, sex),
           sex = factor(sex)) %>%
    
    mutate(year = as.integer(year)) %>%
    
    mutate(alc_res = ifelse(alc_res == 95, NA, alc_res),
    alc_res = ifelse(alc_res == 96, NA, alc_res),
    alc_res = ifelse(alc_res == 97, NA, alc_res),
    alc_res = ifelse(alc_res == 98, NA, alc_res),
    alc_res = ifelse(alc_res == 99, NA, alc_res)) %>%
    
    mutate(Alcohol = ifelse(alc_res > 0, TRUE, FALSE))
    
    gathered_df <- df %>%
    tidyr::gather(drug_number, drug_type_raw, contains("drugres")) %>%
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
          select(-drug_type_raw, -drug_number) %>%
    
      filter(!(is.na(Alcohol) & is.na(drug_type)))
    
    non_missing_drugs <- gathered_df %>%
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    ungroup() %>%
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num) %>%
    group_by(unique_id) %>%
    summarize(Cannabinoid = any(Cannabinoid),
              Depressant = any(Depressant),
              Narcotic = any(Narcotic),
              Other = any(Other),
              Stimulant = any(Stimulant)) %>%
    ungroup()
  df <- df %>%
    dplyr::select(-contains("drugres")) %>%
    dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
    tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    dplyr::mutate(drug_type = factor(drug_type)) %>%
    
    mutate(lag_hrs = ifelse(lag_hrs == 999, NA, lag_hrs),
                        ifelse(lag_hrs == 99, NA, lag_hrs),
                        ifelse(lag_hrs == 88, NA, lag_hrs)) %>%
      filter((lag_hrs == 1 & lag_mins == 0) | lag_hrs < 1) %>%
            
      select(-lag_hrs, -lag_mins) %>%
    
    mutate(age, ifelse(age == 99, NA, age),
           ifelse(age == 999, NA, age)) %>%
    
    mutate(agecat = cut(age, breaks = c(0, 25, 44, 64, 98),
                  labels = c("< 25 years", "25--44 years", "45--64 years", "65 years +"))) %>%
    select(-age) %>%
    
    select(unique_id, sex, year, agecat, drug_type, positive_for_drug)
    return(df)
}

data_1999 <- clean_yearly_person_file(1999)
data_1999 %>%
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
save(clean_fars, file = "~/Desktop/Inclass1/data/clean_fars.RData") 

load("~/Desktop/Inclass1/data/clean_fars.RData")
dim(clean_fars)
length(unique(clean_fars$unique_id))
summary(clean_fars)

clean_fars %>%
  mutate(year_cat = cut(year, breaks = c(1999, 2002, 2006, 2010),
                        labels = c("1999-2002", "2003-2006",
                                   "2007-2010"),
                        include.lowest = TRUE, right = TRUE)) %>%
  
  filter(!is.na(sex)) %>%
  group_by(drug_type, sex, year_cat) %>%
  
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
  
  select(drug_type, sex, year_cat, perc_positive) %>%
  
  unite(sex_year_cat, sex, year_cat) %>%
  spread(sex_year_cat, perc_positive) %>%
  knitr::kable(col.names = c("Drug type", "F 1999-2002",
                             "F 2003-2006", "F 2007-2010",
                             "M 1999-2002", "M 2003-2006",
                             "M 2007-2010"))
    






              

              
        
                
                

              
              
              
            