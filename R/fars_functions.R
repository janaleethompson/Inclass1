library(tidyverse)
library(dplyr)
library(readr)

per_cis <- function(x, n){
 prop_est <- x/n
  se <- sqrt(prop_est*(1-prop_est)/n)
  lb <- prop_est - 1.96 * se
  ub <- prop_est + 1.96 * se
  prop_new <- paste0(round((prop_est * 100), 1), "% ", 
                     "(",round((lb * 100), 1),"%", ", ", round((ub * 100), 1),"%)")
  return(prop_new)
}
  per_cis(x = 9000, n = 23000)


  
#function I am trying to create (pg. 6 HW) 
  test_trend_ca <- function(drug, data = clean_fars){
    to_test <- data 
    if (drug == "Nonalcohol"){
      df <- to_test %>% 
        dplyr::filter(drug_type != "Alcohol") 
  } 
    else {
      df <- to_test %>% 
        filter(drug_type == drug)   
    }  
    df <- df %>% 
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = df$positive,
                                  n = df$trials)
      out <- data.frame(Z = (sqrt(ca_alcohol$statistic)),
                    p.value = (ca_alcohol$p.value))
      rownames(out) <- NULL
    return(out)
  }
  test_trend_ca("Stimulant")
  
  
#example from HW
  to_test <- clean_fars %>%
    filter(drug_type == "Alcohol") %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  ca_alcohol <- prop.trend.test(x = to_test$positive,
                                n = to_test$trials)
  sqrt(ca_alcohol$statistic)
  ca_alcohol$p.value
   
   
#use lapply to apply function over all drug categories for ca table
   drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
                  "Stimulant", "Cannabinoid", "Other")
   drug_trend_tests_ca <- lapply(drug_list, test_trend_ca)
   drug_trend_tests_ca <- dplyr::bind_rows(drug_trend_tests_ca) %>%
     dplyr::mutate(drug = drug_list) %>%
     dplyr::select(drug, Z, p.value)
   drug_trend_tests_ca %>% knitr::kable()
  
   #logistic reg function
   library(tidyverse)
   library(tidyverse)
   test_trend_log_reg <- function(drug, data = clean_fars){
     to_test <- data 
     if (drug == "Nonalcohol"){
       df <- to_test %>% 
         dplyr::filter(drug_type != "Alcohol") 
       log_reg <- glm(positive_for_drug ~ year, data = df,
                      family = binomial(link = "logit"))
       summary <- summary(log_reg)$coefficients
       Z = round(summary[2,3], digits = 2)
       p.value = round(summary[2,4], digits = 3)
       data.frame(Z, p.value)
     }
     else {
       df <- to_test %>% 
         filter(drug_type == drug)   
       log_reg <- glm(positive_for_drug ~ year, data = df,
                      family = binomial(link = "logit"))
       summary <- summary(log_reg)$coefficients
       Z = round(summary[2,3], digits = 2)
       p.value = round(summary[2,4], digits = 3)
       data.frame(Z, p.value)
     }
   }
   test_trend_log_reg("Alcohol")
   test_trend_log_reg("Stimulant")
   test_trend_log_reg("Nonalcohol")
   
   
# logistic regression: need to write function to fit this
   to_test <- clean_fars %>%
     filter(drug_type == "Alcohol")
   log_reg <- glm(positive_for_drug ~ year, data = to_test,
                  family = binomial(link = "logit"))
   summary(log_reg)$coefficients
   
   
   #using lapply with this function to create a table 
   drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
                  "Stimulant", "Cannabinoid", "Other")
   drug_trend_tests_log_reg <- lapply(drug_list, test_trend_log_reg)
   drug_trend_tests_log_reg <- dplyr::bind_rows(drug_trend_tests_log_reg) %>%
     dplyr::mutate(drug = drug_list) %>%
     dplyr::select(drug, Z, p.value)
   drug_trend_tests_log_reg %>% knitr::kable()
     
   