
library(synthpop)
library(tidyverse)

theme_set(theme_classic())

getwd()

# wide <- read_csv('C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/data/wide_data.csv')

# directory <- 'C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/'

# fs::dir_create(directory)

# wide %>%
#   group_by(college) %>%
#   nest() %>%
#   pwalk(~write_csv(x = .y,
#   path = paste0(path = directory, .x, '.csv')))


college_finder <- function(college){
  
  read_csv(glue::glue('C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/colleges/{college}'))
  
}

set.seed(01282021)

law_sch <- college_finder(college = 'School of Law.csv')  %>%
mutate(ge = as.factor(ge),
       ge_teach = as.factor(ge_teach),
       ge_rsch = as.factor(ge_rsch),
       ge_admin = as.factor(ge_admin),
       ge_general = as.factor(ge_general),
       ge_gen_teach = as.factor(ge_gen_teach),
       ge_gen_rsch = as.factor(ge_gen_rsch),
       ge_gen_admin = as.factor(ge_gen_admin),
       ge_rsch_funded = as.factor(ge_rsch_funded),
       dept_fund = as.factor(dept_fund),
       support = as.factor(support),
       remission = as.factor(remission),
       loan = as.factor(loan),
       work_study = as.factor(work_study),
       fyf = as.factor(fyf),
       nsf = as.factor(nsf),
       raymund = as.factor(raymund),
       lokey = as.factor(lokey),
       grantschol = as.factor(grantschol),
       general_funded = as.factor(general_funded),
       nsf_diss = as.factor(nsf_diss),
       term = as.factor(term),
       student_level = as.factor(student_level),
       course_dept = as.factor(course_dept),
       credit_per_class = credit/headcount,
       credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                       credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                       credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                       credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                       credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                       credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                       credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                       credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                       credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                       credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                       credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                       credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                       credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                       credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                       credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  sample_frac(.2)

law_fake <- syn(law_sch)
write.syn(law_fake, file = 'Law_synthetic', filetype = 'csv')


cas_sch <- college_finder(college = 'College of Arts & Sciences.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>%
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  sample_frac(.2)

cas_sch1 <- cas_sch %>%
  filter(course_dept == 'American English Institute' |
           course_dept == 'Anthropology' |
           course_dept == 'Asian Studies' |
           course_dept == 'Biology' |
           course_dept == 'Chemistry' |
           course_dept == 'Cinema Studies' |
           course_dept == 'Classics' |
           course_dept == 'Comparative Literature' |
           course_dept == 'Computer & Information Science' |
           course_dept == 'Creative Writing' |
           course_dept == 'E Asian Languages & Literature' |
           course_dept == 'Earth Sciences' |
           course_dept == 'Economics' |
           course_dept == 'English' |
           course_dept == 'Environmental Studies' |
           course_dept == 'Ethnic Studies' |
           course_dept == 'European Studies' |
           course_dept == 'Folklore' |
           course_dept == 'Freshman Honors Colloquium' |
           course_dept == 'Geography')

cas_sch2 <- cas_sch %>%
  filter(course_dept == 'German Languages & Literature' |
           course_dept == 'History' |
           course_dept == 'Human Physiology' |
           course_dept == 'Humanities Program' |
           course_dept == 'International Studies' |
           course_dept == 'Judaic Studies' |
           course_dept == 'Latin American Studies' |
           course_dept == 'Linguistics' |
           course_dept == 'Mathematics' |
           course_dept == 'Medieval Studies Program' |
           course_dept == 'Philosophy' |
           course_dept == 'Physics' |
           course_dept == 'Political Science' |
           course_dept == 'Psychology' |
           course_dept == 'Religious Studies' |
           course_dept == 'Roman Languages' |
           course_dept == 'Russian, East European, & Eurasian Studies' |
           course_dept == 'Sociology' |
           course_dept == 'Theatre Arts' |
           course_dept == "Women's, Gender And Sexuality Studies")

cas_fake1 <- syn(cas_sch1)
cas_fake2 <- syn(cas_sch2)

# write.syn(cas_fake1, file = 'Arts_Sciences_synthetic1', filetype = 'csv')
# write.syn(cas_fake2, file = 'Arts_Sciences_synthetic2', filetype = 'csv')

cas1 <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Arts_Sciences_synthetic1.csv")
cas2 <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Arts_Sciences_synthetic2.csv")

cas_fake_full <- full_join(cas1, cas2)

write.csv(cas_fake_full, 'Arts_Sciences_synthetic.csv')

des_sch <- college_finder(college = 'College of Design.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  dplyr::select(-course_number) %>% 
  sample_frac(.2)

# des_fake <- syn(des_sch)
# write.syn(des_fake, file = 'Design_synthetic', filetype = 'csv')


coe_sch <- college_finder(college = 'College of Education.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  sample_frac(.2)

# coe_fake <- syn(coe_sch)
# write.syn(coe_fake, file = 'Education_synthetic', filetype = 'csv')


bus_sch <- college_finder(college = 'Lundquist College of Business.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  dplyr::select(-course_number) %>% 
  sample_frac(.2)

# bus_fake <- syn(bus_sch)
# write.syn(bus_fake, file = 'Business_synthetic', filetype = 'csv')


sojc_sch <- college_finder(college = 'School of Journalism & Communication.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  dplyr::select(-course_number) %>% 
  sample_frac(.2)

# sojc_fake <- syn(sojc_sch)
# write.syn(sojc_fake, file = 'Journalism_Communication_synthetic', filetype = 'csv')

mus_sch <- college_finder(college = 'School of Music & Dance.csv')  %>%
  mutate(ge = as.factor(ge),
         ge_teach = as.factor(ge_teach),
         ge_rsch = as.factor(ge_rsch),
         ge_admin = as.factor(ge_admin),
         ge_general = as.factor(ge_general),
         ge_gen_teach = as.factor(ge_gen_teach),
         ge_gen_rsch = as.factor(ge_gen_rsch),
         ge_gen_admin = as.factor(ge_gen_admin),
         ge_rsch_funded = as.factor(ge_rsch_funded),
         dept_fund = as.factor(dept_fund),
         support = as.factor(support),
         remission = as.factor(remission),
         loan = as.factor(loan),
         work_study = as.factor(work_study),
         fyf = as.factor(fyf),
         nsf = as.factor(nsf),
         raymund = as.factor(raymund),
         lokey = as.factor(lokey),
         grantschol = as.factor(grantschol),
         general_funded = as.factor(general_funded),
         nsf_diss = as.factor(nsf_diss),
         term = as.factor(term),
         student_level = as.factor(student_level),
         course_dept = as.factor(course_dept),
         credit_per_class = credit/headcount,
         credit_per_full_num = case_when(credit_per_class < 2 ~ 1,
                                         credit_per_class >= 2 & credit_per_class < 3 ~ 2,
                                         credit_per_class >= 3 & credit_per_class < 4 ~ 3,
                                         credit_per_class >= 4 & credit_per_class < 5 ~ 4,
                                         credit_per_class >= 5 & credit_per_class < 6 ~ 5,
                                         credit_per_class >= 6 & credit_per_class < 7 ~ 6,
                                         credit_per_class >= 7 & credit_per_class < 8 ~ 7,
                                         credit_per_class >= 8 & credit_per_class < 9 ~ 8,
                                         credit_per_class >= 9 & credit_per_class < 10 ~ 9,
                                         credit_per_class >= 10 & credit_per_class < 11 ~ 10,
                                         credit_per_class >= 11 & credit_per_class < 12 ~ 11,
                                         credit_per_class >= 12 & credit_per_class < 13 ~ 12,
                                         credit_per_class >= 13 & credit_per_class < 14 ~ 13,
                                         credit_per_class >= 14 & credit_per_class < 15 ~ 14,
                                         credit_per_class == 15 ~ 15)) %>% 
  filter(year == 18 &
           course_level == 'Undergraduate') %>% 
  sample_frac(.2)

# mus_fake <- syn(mus_sch)
# write.syn(mus_fake, file = 'Music_Dance_synthetic', filetype = 'csv')



# only use 20% of the data for each college

# cas <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Arts_Sciences_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# bus <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Business_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# cod <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Design_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# coe <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Education_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# sojc <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Journalism_Communication_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# law <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Law_synthetic.csv") %>% 
#   sample_frac(.2)
# 
# mus <- read_csv("C:/Users/cpppe/Desktop/github_projects/gradschool_dashboard/synthetic_data/synthetic_csv/Music_Dance_synthetic.csv") %>% 
#   sample_frac(.2)

# write.csv(cas, 'Arts_Sciences_synthetic.csv')
# write.csv(bus, 'Business_synthetic.csv')
# write.csv(cod, 'Design_synthetic.csv')
# write.csv(coe, 'Education_synthetic.csv')
# write.csv(sojc, 'Journalism_Communication_synthetic.csv')
# write.csv(law, 'Law_synthetic.csv')
# write.csv(mus, 'Music_Dance_synthetic.csv')

