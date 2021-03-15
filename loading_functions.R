library(tidyverse)

getwd()

colleges_combined <- function(percent){
  
  cas <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Arts_Sciences_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  bus <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Business_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  des <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Design_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  coe <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Education_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  sojc <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Journalism_Communication_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  law <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Law_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  mus <- readr::read_csv('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Music_Dance_synthetic.csv') %>% 
    dplyr::sample_frac(percent)
  
  joined <- dplyr::full_join(cas, bus) %>% 
    dplyr::full_join(des) %>% 
    dplyr::full_join(coe) %>% 
    dplyr::full_join(sojc) %>% 
    dplyr::full_join(law) %>% 
    dplyr::full_join(mus)
  
  message('This is synthetic data for all UO Colleges/Schools')
  
  return(joined)
}


uo <- colleges_combined(percent = .01) # You may want to use around 1% of the data as each college has a large amount of data.


# create functions to combine course data and grad data for each college
load_college <- function(college, percent){
  
  college_name <- readr::read_csv(glue::glue('https://media.githubusercontent.com/media/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/{college}_synthetic.csv')) %>% 
    dplyr::sample_frac(percent)

  message(glue::glue('This is synthetic data for College/School of {college}.'))
  
  return(college_name)
}

cas <- load_college('Arts_Sciences', percent = .05) # You may want to use around 5% of the data for any given college, they are large data frames.
