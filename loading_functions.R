library(tidyverse)

getwd()

# create functions to combine course data and grad data for each college
load_college <- function(college){
  
  course <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/{college}_courses_synthetic.csv'))
  
  grad <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/{college}_synthetic.csv'))
  
  joined <- dplyr::left_join(course, grad)
  
  message(glue::glue('This is synthetic data for College/School of {college}.'))
  
  return(joined)
}

# cas <- load_college('Arts_Sciences') %>% 
  # sample_frac(.05) #you may want to use around 5% of the data. With synethic course data, the dataframe is large.


# function for all grad data
colleges_combined <- function(){
  
  cas <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Arts_Sciences_synthetic.csv')
  bus <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Business_synthetic.csv')
  des <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Design_synthetic.csv')
  coe <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Education_synthetic.csv')
  sojc <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Journalism_Communication_synthetic.csv')
  law <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Law_synthetic.csv')
  mus <- readr::read_csv('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Music_Dance_synthetic.csv')
  
  joined <- dplyr::full_join(cas, bus) %>% 
    dplyr::full_join(des) %>% 
    dplyr::full_join(coe) %>% 
    dplyr::full_join(sojc) %>% 
    dplyr::full_join(law) %>% 
    dplyr::full_join(mus)
  
  message('This is synthetic data for all UO Colleges/Schools')
  
  return(joined)
}

# grad_data <- colleges_combined() %>% 
                # sample_frac(.05) #you may want to use around 5% of the data. With synethic course data, the dataframe is large.

# function for all course data
college_courses_combined <- function(college){
  
  cas <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Arts_Sciences_courses_synthetic.csv'))
  bus <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Business_courses_synthetic.csv'))
  des <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Design_courses_synthetic.csv'))
  coe <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Education_courses_synthetic.csv'))
  sojc <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Journalism_Communication_courses_synthetic.csv'))
  law <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Law_courses_synthetic.csv'))
  mus <- readr::read_csv(glue::glue('https://raw.githubusercontent.com/jpedroza1228/gradschool_dashboard/main/synthetic_data/synthetic_csv/Music_Dance_courses_synthetic.csv'))
  
  joined <- dplyr::full_join(cas, bus) %>% 
    dplyr::full_join(des) %>% 
    dplyr::full_join(coe) %>% 
    dplyr::full_join(sojc) %>% 
    dplyr::full_join(law) %>% 
    dplyr::full_join(mus)
  
  message("This is synthetic data for all UO Colleges'/Schools' course data")
  
  return(joined)
}

# course_data <- college_courses_combined() %>% 
  # sample_frac(.05) #you may want to use around 5% of the data. With synethic course data, the dataframe is large.
