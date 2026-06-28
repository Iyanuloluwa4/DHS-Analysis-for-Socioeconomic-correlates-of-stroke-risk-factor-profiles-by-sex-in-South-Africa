library(dplyr)
library(tidyr)
library(haven)
library(survey)
library(labelled)

#=================
#Codes
#=================

#hh = household recode data
#ir = individual recode data
#mr = men recode data

#design_vars = survey design variables

#hv005 - DHS Sample Weighting
#hv021 - Primary Sampling unit
#hv022- Sampling Strata

#hv025 - DHS Code for Place of Residence (household recode file)
#hv270 - DHS Code for Household Wealth Index (household recode file)
#hv009 - Household size
#hv104 - Sex
#hv105 - Age
#hv106 - Educational Attainment
#hv219 - Sex of Household head

#sbp_long = systolic blood pressure data
#dbp_long = diastolic blood pressure data
#med_long = Data for those currently taking antihypertensive medication
#hypertension_final = final data for Hypertension
#sh228a_ = Second systolic BP reading for men
#sh232a_ = Third systolic BP reading for men
#sh228b_ = Second systolic BP reading for women
#sh232b_ = Third systolic BP reading for women
#sh224_ = Currently taking antihypertensive meds

#shmhba1c_ - Men's HbA1C laboratory measurement slots
#hb0_ - Person index identifying which household member each HbA1C measuement belongs to (for men)
#shwhba1c_ - Women's HbA1C laboratory measurement slots
#ha0_ Person index identifying which household member each HbA1C measuement belongs to (for ladies)

#hb40 - Men's BMI measurement status
#ha40 - Women's Bmi measurement status

#hb35 - Men Smoking status
#ha35 - Women Smoking status

#Blood Pressure
#994 = Missing
#995 = Not Present
#996 = Invalid Measurement

#HbA1C
#99993 - 99996 = Missing/Invalid Lab RESULTS

#BMI
#9998 - 9999 = Missing

#Data from the individual and Men recode for healthcare insurance variable
#v481 - individual recode for healthcare insurance access
#mv481 - men recode for healthcare insurance access


# ---------------------------
# Data Load
# ---------------------------
#Data could not be pasted here due to Data Confidentiality and DHS agreement

hh <- hh %>%
  mutate(
    hhid = paste0(hv001, "_", hv002)
  )

ir <- ir %>%
  mutate(
    hhid = paste0(v001,'_',v002),
    person = sprintf('%02d', v003)
  )

mr <- mr %>%
  mutate(
    hhid = paste0(mv001,'_',mv002),
    person = sprintf('%02d', mv003)
  )

# ---------------------------
# Survey design variables
# ---------------------------
design_vars <- hh %>%
  select(hhid, hv021, hv022, hv005, hv270, hv025, hv009)




#-------------------------
#HYPERTENSION
#-------------------------




# ---------------------------
# SBP (2nd and 3rd readings)
# ---------------------------
sbp_long <- hh %>%
  select(hhid,
         starts_with("sh228a_"),
         starts_with("sh232a_")) %>%
  pivot_longer(
    cols = -c(hhid),
    names_to = c("reading", "person"),
    names_pattern = "(sh\\d+a)_(\\d+)",
    values_to = "sbp"
  ) %>%
  mutate(
    sbp = ifelse(sbp %in% c(994, 995, 996), NA, sbp)
  )

# ---------------------------
# DBP (2nd and 3rd readings)
# ---------------------------
dbp_long <- hh %>%
  select(hhid,
         starts_with("sh228b_"),
         starts_with("sh232b_")) %>%
  pivot_longer(
    cols = -c(hhid),
    names_to = c("reading", "person"),
    names_pattern = "(sh\\d+b)_(\\d+)",
    values_to = "dbp"
  ) %>%
  mutate(
    dbp = ifelse(dbp %in% c(994, 995, 996), NA, dbp)
  )

# ---------------------------
# Age
# ---------------------------
age_long <- hh %>%
  select(hhid, starts_with("hv105_")) %>%
  pivot_longer(
    cols = starts_with("hv105_"),
    names_to = "person",
    names_prefix = "hv105_",
    values_to = "age"
  )

# ---------------------------
# Sex
# ---------------------------
sex_long <- hh %>%
  select(hhid, starts_with("hv104_")) %>%
  pivot_longer(
    cols = starts_with("hv104_"),
    names_to = "person",
    names_prefix = "hv104_",
    values_to = "sex"
  )

# ---------------------------
# Antihypertensive medication
# ---------------------------
med_long <- hh %>%
  select(hhid, starts_with("sh224_")) %>%
  pivot_longer(
    cols = starts_with("sh224_"),
    names_to = "person",
    names_prefix = "sh224_",
    values_to = "med_bp"
  )

# ---------------------------
# Average SBP per person
# ---------------------------
sbp_person <- sbp_long %>%
  group_by(hhid, person) %>%
  summarise(
    sbp = mean(sbp, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------
# Average DBP per person
# ---------------------------
dbp_person <- dbp_long %>%
  group_by(hhid, person) %>%
  summarise(
    dbp = mean(dbp, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------
# BP measurements combination
# ---------------------------
bp_person <- sbp_person %>%
  left_join(
    dbp_person,
    by = c("hhid", "person")
  )

# ---------------------------
#Final Data For Hypertension
# ---------------------------
hypertension_final <- bp_person %>%
  left_join(age_long,
            by = c("hhid", "person")) %>%
  left_join(sex_long,
            by = c("hhid", "person")) %>%
  left_join(med_long,
            by = c("hhid", "person")) %>%
  left_join(design_vars,
            by = "hhid")

# ---------------------------
# Age Filter
# ---------------------------
hypertension_final <- hypertension_final %>%
  filter(age>=15,
         !age %in% c(98,99))

# ---------------------------
# DHS Weights Application
# ---------------------------
hypertension_final <- hypertension_final %>%
  mutate(
    weight = hv005 / 1000000
  )

# ---------------------------------------------------------------------------------------
# Hypertension Definition (I included those currently taking antihypertensives as med_bp)
# ---------------------------------------------------------------------------------------
hypertension_final <- hypertension_final %>%
  mutate(
    hypertension = case_when(
      med_bp == 1 ~ 1,
      sbp >= 140 ~ 1,
      dbp >= 90 ~ 1,
      is.na(sbp) & is.na(dbp) ~ NA_real_,
      TRUE ~ 0
    )
  )

# ---------------------------
# DHS survey design
# ---------------------------
design <- svydesign(
  ids = ~hv021,
  strata = ~hv022,
  weights = ~weight,
  data = hypertension_final,
  nest = TRUE
)

# ---------------------------
# Hypertension prevalence
# ---------------------------
x <- svymean(
  ~hypertension,
  design,
  na.rm = TRUE
)

print(x)
confint(x)

# ---------------------------
# Hypertension prevalence by sex
# ---------------------------

svyby(
  ~hypertension,
  ~sex,
  design,
  svymean,
  vartype = 'ci',
  na.rm = TRUE
)

svychisq(~hypertension + sex, design)




#-----------------------
#DIABETES
#----------------------




#==============================
#Adjusted HBA1C IN Men
#==============================
diabetes_men <- hh %>%
  select(hhid, starts_with('shmhba1c_')) %>%
  pivot_longer(
    cols = starts_with('shmhba1c_'),
    names_to = 'slot',
    names_prefix = 'shmhba1c_',
    values_to = 'hba1c'
  )%>%
  mutate(
    hba1c = ifelse(hba1c %in% c(99993,99994,99995,99996), NA, hba1c)
  )%>%
  mutate(
    slot = as.integer(slot)
  )

diabetesm_index <- hh %>%
  select(hhid,
         starts_with("hb0_")) %>%
  pivot_longer(
    cols = starts_with("hb0_"),
    names_to = "slot",
    names_prefix = "hb0_",
    values_to = "person"
  ) %>%
  mutate(
    slot = as.integer(slot)
  )

diabetesm_long <- diabetes_men %>%
  left_join(
    diabetesm_index,
    by = c("hhid","slot")
  )%>% 
  mutate(person = sprintf("%02d", person))

#==============================
#Adjusted HBA1C IN Women
#==============================
diabetes_women <- hh %>%
  select(hhid, starts_with('shwhba1c_')) %>%
  pivot_longer(
    cols = starts_with('shwhba1c_'),
    names_to = 'slot',
    names_prefix = 'shwhba1c_',
    values_to = 'hba1c'
  )%>%
  mutate(
    hba1c = ifelse (hba1c %in% c(99993,99994,99995,99996), NA, hba1c)
  )%>%
  mutate(
    slot = as.integer(slot)
  )

diabetesw_index <- hh %>%
  select(hhid,
         starts_with("ha0_")) %>%
  pivot_longer(
    cols = starts_with("ha0_"),
    names_to = "slot",
    names_prefix = "ha0_",
    values_to = "person"
  ) %>%
  mutate(
    slot = as.integer(slot)
  )

diabetesw_long <- diabetes_women %>%
  left_join(
    diabetesw_index,
    by = c("hhid","slot")
  )%>% 
  mutate(person = sprintf("%02d", person))

#==============================
#Age
#==============================
age <- hh %>%
  select(hhid, starts_with('hv105_')) %>%
  pivot_longer(
    cols = starts_with('hv105_'),
    names_to = 'person',
    names_prefix = 'hv105_',
    values_to = 'age'
  )

#==============================
#Sex
#==============================
sex <- hh %>%
  select(hhid, starts_with('hv104_')) %>%
  pivot_longer(
    cols = starts_with('hv104_'),
    names_to = 'person',
    names_prefix = 'hv104_',
    values_to = 'sex'
  )

#==============================
#Diabetes Combination
#==============================
diabetes_final <- bind_rows(diabetesm_long,
                            diabetesw_long)%>%
  mutate(
    hba1c = ifelse (hba1c %in% c(99993,99994,99995,99996), NA, hba1c)
  )%>%
  left_join(age, by = c('hhid', 'person')) %>%
  left_join(sex, by = c('hhid', 'person')) %>%
  left_join(design_vars, by = 'hhid')

#==============================
#Age Filter
#==============================
diabetes_final <- diabetes_final %>%
  filter(age>=15,
         !age %in% c(98,99))

summary(diabetes_final$age)

#==============================
#DHS Weighting
#==============================
diabetes_final <- diabetes_final %>%
  mutate (weight = hv005 / 1000000)

#========================================================
#Diabetes cut off calculation include DHS Venous Sampling
#========================================================
diabetes_final <- diabetes_final %>%
  mutate(
    hba1c_adj = (hba1c/1000 - 0.228) / 0.9866,
    diabetes = case_when(
      hba1c_adj >=6.5 ~ 1,
      is.na(hba1c) ~ NA_real_,
      TRUE ~ 0
    )
  )

options(survey.lonely.psu = "adjust")

# ---------------------------
# DHS survey design
# ---------------------------
design <- svydesign(
  ids = ~hv021,
  strata = ~hv022,
  weights = ~weight,
  data = diabetes_final,
  nest = TRUE
)

# ---------------------------
# Diabetes Prevalence
# ---------------------------
x <- svymean(
  ~diabetes,
  design,
  na.rm = TRUE
)

print(x)
confint(x)

# ---------------------------
# Diabetes Prevalence By Sex
# ---------------------------

svyby(
  ~diabetes,
  ~sex,
  design,
  svymean,
  vartype = 'ci',
  na.rm = TRUE
)

svychisq(~diabetes + sex, design)



#--------------------
#Obesity
#--------------------




#==================
#Obesity For Men
#==================

men_bmi <- hh %>%
  select(hhid, starts_with('hb40_')) %>%
  pivot_longer(
    cols = starts_with('hb40_'),
    names_to = 'slot',
    values_to = 'bmi',
    names_prefix= 'hb40_'
  ) %>%
  mutate(
    bmi = ifelse(bmi %in% c(9998,9999), NA, bmi),
    bmi = bmi/100
  )%>%
  mutate(
    slot = as.integer(slot)
  )

men_index <- hh %>%
  select(hhid,
         starts_with("hb0_")) %>%
  pivot_longer(
    cols = starts_with("hb0_"),
    names_to = "slot",
    names_prefix = "hb0_",
    values_to = "person"
  ) %>%
  mutate(
    slot = as.integer(slot)
  )

men_long <- men_bmi %>%
  left_join(
    men_index,
    by = c("hhid","slot")
  )%>% 
  mutate(person = sprintf("%02d", person))

#==================
#Obesity For Women
#==================

women_bmi <- hh %>%
  select(hhid, starts_with('ha40_')) %>%
  pivot_longer(
    cols = starts_with('ha40_'),
    values_to = 'bmi',
    names_to = 'slot',
    names_prefix= 'ha40_'
  ) %>%
  mutate(bmi = ifelse(bmi %in% c(9998,9999), NA, bmi),
         bmi = bmi/100
  ) %>%
  mutate(
    slot = as.integer(slot)
  )

women_index <- hh %>%
  select(hhid,
         starts_with("ha0_")) %>%
  pivot_longer(
    cols = starts_with("ha0_"),
    names_to = "slot",
    names_prefix = "ha0_",
    values_to = "person"
  ) %>%
  mutate(
    slot = as.integer(slot)
  )

women_long <- women_bmi %>%
  left_join(
    women_index,
    by = c("hhid","slot")
  ) %>% 
  mutate(person = sprintf("%02d", person))

#================
#age
#================

age_long <- hh %>%
  select(hhid, starts_with('hv105')) %>%
  pivot_longer(
    cols = starts_with('hv105'),
    names_to = 'person',
    values_to = 'age',
    names_prefix = 'hv105_'
  )
#================
#sex
#================

sex_long <- hh %>%
  select(hhid, starts_with('hv104')) %>%
  pivot_longer(
    cols = starts_with('hv104'),
    names_to = 'person',
    values_to = 'sex',
    names_prefix = 'hv104_'
  )


#===============
#Obesity Combination
#==============

obesity_final <- bind_rows(men_long,
                           women_long) %>%
  left_join(age_long, by = c('person', 'hhid')) %>%
  left_join(sex_long, by = c('person', 'hhid'))  %>%
  left_join(design_vars, by = c('hhid'))

#==============
#Age Filter
#==============
obesity_final <- obesity_final %>%
  filter(age>=15,
         !age %in% c(98,99))

#=============
#DHS Weighting
#============
obesity_final <- obesity_final %>%
  mutate(
    weight = hv005 / 1000000
  )

#===================
#Obesity Cut off Calculation
#===================
obesity_final <- obesity_final %>%
  mutate(
    obesity = case_when(
      bmi >= 30 ~ 1,
      bmi <30 ~ 0,
      is.na(bmi) ~ NA_real_
    )
  )

#=============
#DHS design
#=============

design <- svydesign(
  ids = ~ hv021,
  strata = ~hv022,
  weights = ~weight,
  data = obesity_final,
  nest = TRUE
)

#=============
# OBESITY Prevalence
#=============

x <- svymean(
  ~obesity, design,
  na.rm = TRUE
)

print(x)
confint(x)


#=============
# OBESITY Prevalence by sex
#=============

svyby(
  ~obesity,
  ~sex,
  design,
  svymean,
  vartype = 'ci',
  na.rm = TRUE
)

svychisq(~obesity + sex, design)


#----------------
#Smoking
#----------------




#==================
#Smoking For Men
#==================

men_smoke <- hh %>%
  select(hhid, starts_with('hb35_')) %>%
  pivot_longer(
    cols = starts_with('hb35_'),
    names_to = 'slot',
    values_to = 'smoking_status',
    names_prefix= 'hb35_'
  ) %>%
  mutate(
    smoking_status = ifelse(smoking_status %in% c(994,995,996), NA, smoking_status
    )
  )%>%
  mutate(slot = as.integer(slot))

men_index <- hh %>%
  select(hhid, starts_with('hb0_')) %>%
  pivot_longer(
    cols = starts_with('hb0_'),
    names_to = 'slot',
    values_to = 'person',
    names_prefix= 'hb0_'
  )%>%
  mutate(slot = as.integer(slot))

men_long <- men_smoke %>%
  left_join(
    men_index, by = c('slot', 'hhid')
  )%>%
  mutate(person = sprintf("%02d", person))

#==================
#Smoking For Women
#==================

women_smoke <- hh %>%
  select(hhid, starts_with('ha35_')) %>%
  pivot_longer(
    cols = starts_with('ha35_'),
    names_to = 'slot',
    values_to = 'smoking_status',
    names_prefix= 'ha35_'
  ) %>%
  mutate(smoking_status = ifelse(smoking_status %in% c(994,995,996), NA, smoking_status)
  )%>%
  mutate(slot = as.integer(slot))

women_index <- hh %>%
  select(hhid, starts_with('ha0_')) %>%
  pivot_longer(
    cols = starts_with('ha0_'),
    names_to = 'slot',
    values_to = 'person',
    names_prefix= 'ha0_'
  )%>%
  mutate(slot = as.integer(slot))

women_long <- women_smoke %>%
  left_join(
    women_index, by = c('slot', 'hhid')
  )%>%
  mutate(person = sprintf("%02d", person))

#================
#Age
#================

age_long <- hh %>%
  select(hhid, starts_with('hv105')) %>%
  pivot_longer(
    cols = starts_with('hv105'),
    names_to = 'person',
    values_to = 'age',
    names_prefix = 'hv105_'
  )

#================
#Sex
#================

sex_long <- hh %>%
  select(hhid, starts_with('hv104')) %>%
  pivot_longer(
    cols = starts_with('hv104'),
    names_to = 'person',
    values_to = 'sex',
    names_prefix = 'hv104_'
  )


#===============
#Smoking Combination
#==============

smoking_final <- bind_rows(men_long,
                           women_long) %>%
  left_join(age_long, by = c('person', 'hhid')) %>%
  left_join(sex_long, by = c('person', 'hhid'))  %>%
  left_join(design_vars, by = c('hhid'))

#==============
#Age Filter
#==============
smoking_final <- smoking_final %>%
  filter(age>=15,
         !age %in% c(98,99))

#=============
#DHS Weighting
#============
smoking_final <- smoking_final %>%
  mutate(
    weight = hv005 / 1000000
  )

#=============
#Smoking Calculation
#=============
smoking_final <- smoking_final %>%
  mutate(
    smoking = case_when(
      smoking_status == 0 ~ 0,
      smoking_status %in% 1:80 ~ 1,
      smoking_status == 94 ~ 1,
      TRUE ~ NA_real_
    )
  )

#=============
#DHS design
#=============

design <- svydesign(
  ids = ~ hv021,
  strata = ~hv022,
  weights = ~weight,
  data = smoking_final,
  nest = TRUE
)

#=============
#Prevalence
#=============

x <- svymean(
  ~smoking, design,
  na.rm = TRUE
)

print(x)
confint(x)

svyby(
  ~smoking,
  ~sex,
  design,
  svymean,
  vartype = 'ci',
  na.rm = TRUE
)

svychisq(~smoking + sex, design)






#===================
#Composite Score
#==================
# I replace hypertension_final with diabetes_final whenever I am working with Diabetes Data and applicable for other outcomes too.

final_data <- hypertension_final %>%
  select(
    hhid,
    person,
    age,
    sex,
    weight,
    hv021,
    hv022,
    weight,
    hypertension
  ) 

nrow(final_data)




#=================
#PRELUDE TO OBJECTIVE 2
#=================




#===========================================
#Education Variable
#===========================================

education_long <- hh %>%
  select(hhid, starts_with("hv106")) %>%
  pivot_longer(
    cols = starts_with("hv106"),
    names_to = "person",
    values_to = "education",
    names_prefix = "hv106_"
  ) %>%
  mutate(
    person = sprintf("%02d", as.integer(person))
  )

#===========================================
#Household socioeconomic variables
#===========================================

ses_vars <- hh %>%
  select(
    hhid,
    hv270,   # wealth index
    hv025,   # urban/rural residence
    hv009,   # household size
    hv219, # sex of household head,
  ) %>%
  distinct()


#===========================================
# Adding Education to Final Data
#===========================================

final_data <- final_data %>%
  left_join(
    education_long,
    by = c("hhid", "person")
  )

#===========================================
# Adding Household SES var to Final Data
#===========================================

final_data <- final_data %>%
  left_join(
    ses_vars,
    by = "hhid"
  )

look_for(hh, 'hv009')


#===========================================
# AGE GROUPS AND HOSUEHOLD SIZE
#===========================================

final_data <- final_data %>%
  mutate(
    age_group = case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 44 ~ "25-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65             ~ "65+"
    ),
    household_size = case_when(
      hv009 <= 3 ~ "1-3",
      hv009 <= 6 ~ "4-6",
      hv009 <= 9 ~ "7-9",
      TRUE ~ "10+"
    )
  )

final_data$household_size <- factor(
  final_data$household_size,
  levels = c("1-3", "4-6", "7-9", "10+")
)


#===========================================
# variables to factors
#===========================================

final_data <- final_data %>%
  mutate(
    sex = factor(
      sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    age_group = factor(
      age_group,
      levels = c("15-24", "25-44", "45-64", "65+")
    ),
    
    education = factor(
      education,
      levels = c(0, 1, 2, 3),
      labels = c(
        "No education",
        "Primary",
        "Secondary",
        "Higher"
      )
    ),
    
    hv270 = factor(
      hv270,
      levels = 1:5,
      labels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
    ),
    
    hv025 = factor(
      hv025,
      levels = c(1, 2),
      labels = c(
        "Urban",
        "Rural"
      )
    ),
    
    v481 = factor(
      v481,
      levels = c(0,1),
      labels = c(
        'Yes',
        'No'
      )
    ),
    v394 = factor(
      v394,
      levels = c(0,1),
      labels = c(
        'Yes',
        'No'
      )
    )
  )

table(final_data$v481[!is.na(final_data$hypertension)])
table(final_data$hypertension)

#===========================================
# Survey design
#===========================================

design_final <- svydesign(
  ids = ~hv021,
  strata = ~hv022,
  weights = ~weight,
  data = final_data,
  nest = TRUE
)

#===========================================
# OBJECTIVE 2:
# Age-adjusted  model
#===========================================

model_age_sex <- svyglm(
  hypertension ~ sex + age_group,
  design = design_final,
  family = quasibinomial()
)

summary(model_age_sex)

exp(coef(model_age_sex))

exp(confint(model_age_sex))

#===========================================
# OBJECTIVE 2:
# Socioeconomic model
#===========================================

model_ses <- svyglm(
  hypertension ~
    hv270 +
    sex,
  design = design_final,
  family = quasibinomial()
)

summary(model_ses)

exp(coef(model_ses))

exp(confint(model_ses))


#===========================================
# OBJECTIVE 2:
# age-adjusted Socioeconomic model
#===========================================

model_ses_age <- svyglm(
   hypertension ~
    hv025 +
    sex +
    age_group,
  design = design_final,
  family = quasibinomial()
)

summary(model_ses_age)

exp(coef(model_ses_age))

exp(confint(model_ses_age))



#========================
#OBJECTIVE 3
#========================




#===========================================
#Socioeconomic interaction model
#===========================================

model_ses_int <- svyglm(
   hypertension ~
    sex *
    household_size +
    age_group,
  design = design_final,
  family = quasibinomial()
)

summary(model_ses_int)

exp(coef(model_ses_int))

exp(confint(model_ses_int))

#look_for(ir, 'v481')




#=====================
#OBJECTIVE 4
#=====================



#=================
#Healthcare Access
#================

#=================
#Healthcare Insurance
#=================

ir_insurance <- ir %>%
  select(
    hhid,
    person,
    v481,
    v394,
    v467d
  )

mr_insurance <- mr %>%
  select(
    hhid,
    person,
    mv481   # health insurance
  ) %>%
  rename(
    v481 = mv481
  )

insurance_final <- bind_rows(
  ir_insurance,
  mr_insurance
)

#==========================
# Adding Healthcare Insurance to Final Data
#==========================

final_data <- final_data %>%
  left_join(
    insurance_final,
    by = c("hhid", "person")
  )








