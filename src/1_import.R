# Libraries
library(tidyverse)
library(haven)
library(stringi)

# Import ##########################################

data_dir  <- "../data/raw/"
# Name survey dataset
# Downloaded from https://osf.io/yd5qv/files/osfstorage in June 2026

df_nm  <- read_sav(paste0(data_dir, "NameSurvey_DATARAW_2026.sav")) |> filter(country_survey %in% c("Ireland", "Spain"))

# Adding Ireland ethnicity variable to international dataset
df_nm$id_obs  <- paste0(df_nm$input_1, "_", df_nm$Name)
df_nm <- 
  df_nm |> 
    left_join(read_sav(paste0(data_dir, "ES2_NameSurvey_2025-11-28.sav")) |> 
      filter(country_survey == "Ireland") |> 
      rename(cong_ethn_ie = cong_ethn) |> 
      mutate(id_obs = paste0(input_1, "_", Name)) |> 
      select(id_obs, V001caHUNCZ, cong_ethn_ie), by = "id_obs") |> 
      mutate(
        #V001caHUNCZ = if_else(is.na(V001caHUNCZ), V001caHUNCZ_IE, V001caHUNCZ),
        cong_ethn = if_else(country_survey == "Ireland", cong_ethn_ie, df_nm$cong_ethn))
      
# Adding Traveller names observations not included in the international dataset
df_nmie <- read_sav(paste0(data_dir, "ES2_NameSurvey_2025-11-28.sav")) |> filter(country_name == "Irish traveller")
df_nmie <- df_nmie |> mutate(ladder_adj = case_when(V001h < 8 ~ (7 - V001h)/6, V001h %in% c(8, 99) ~ NA))
#df_nmie$cong_ethn = if_else(is.na(df_nmie$cong_ethn), 0, df_nmie$cong_ethn)
df_nm <- bind_rows(df_nm, df_nmie)


# Field experiment Ireland and Spain
df_ie <- read_dta(paste0(data_dir, "ES2_LM_IE_merged_processed.dta"))
df_sp <- read_dta(paste0(data_dir, "ES2_LM_ES_merged_processed.dta"))

# Transforming columns with different labels to factors to bind datasets
cols_difflabels <- c("ethnic_groups", "region_groups", "app_location", "deliveredat1", "app_photo", "female_photo", "country")
df_ie <- df_ie |> mutate_at(vars(all_of(cols_difflabels)), ~as_factor(.x))
df_sp <- df_sp |> mutate_at(vars(all_of(cols_difflabels)), ~as_factor(.x))

# Binding datasets
df_exp <- bind_rows(df_sp, df_ie)

df_exp$surname_applicant <-  word(df_exp$name_applicant, 2)

# Transform ##########################################

## New educ groups
df_nm  <-  df_nm |> mutate(educ_adj2 = case_when(
    VS3 == 1 ~ "Lower than secondary",
    VS3 == 2 ~ "Secondary +",
    VS3 %in% c(3,22,23) ~ "Other",
    VS3 == 4 ~ "Lower than secondary",
    VS3 > 4 & VS3 <= 21 ~ "Secondary +",
    .default = educ_adj
))
## Ethnicity 


df_nm  <-  df_nm |> mutate(educ_adj2 = case_when(
    VS3 == 1 ~ "Lower than secondary",
    VS3 == 2 ~ "Secondary +",
    VS3 %in% c(3,22,23) ~ "Other",
    VS3 == 4 ~ "Lower than secondary",
    VS3 > 4 & VS3 <= 21 ~ "Secondary +",
    .default = educ_adj
))

## Transform correlates 
df_nm <- df_nm |>
  mutate(resp_gender = recode_values(VS1, 1 ~ "Male", 2 ~ "Female", default ='Other'),
        resp_edu = if_else(educ_adj2 == 'Other', 'Not stated/Other', educ_adj2),
        resp_age = case_when(VS2 < 25 ~ "18-24",  VS2 > 24 & VS2 < 45 ~ "25-44",
                  VS2 > 44 & VS2 < 65 ~ "45-64", VS2 > 65 ~ "65+", .default = 'Not stated'),
        resp_cob_ie = case_when(
          VS5b_1 %in% c("Ireland", "ireland",  "Dublin", "Irish", "irish", "Mayo", "Kildare", 
          "Kerry", "Offaly", "Sligo", "Irelna", "Ireland sligo", "Irelan", "IRELAND", 
          "Eire", "Cork", "Cavan") ~ "Ireland", 
          VS5b_1 %in% c("Ireland", "ireland",  "Dublin", "Irish", "irish", "Mayo", "Kildare", 
          "Kerry", "Offaly", "Sligo", "Irelna", "Ireland sligo", "Irelan", "IRELAND", 
          "Eire", "Cork", "Cavan") ~ "Ireland", 
          .default =  "Foreign-born"),
        name_group = case_when(
          region_es == "SSA" ~ "Black",
          region_es == "MENAP" ~ "Muslim",
          country_name == "Spain" & ethnicity == 4 ~ "Majority",
          country_name == "Spain" & ethnicity == 3 ~ "Roma",
          country_name == "Ireland" ~ "Majority",
          country_name == "Irish traveller" ~ "Traveller",
          .default = NA_character_
        ))

# Creating congruence of origin (ethnicity + country of origin)
df_nm  <- 
  df_nm |> 
    mutate(cong_origin = case_when(
      country_survey == "Spain" ~ cong_ethn,
      country_name == "Ireland" & cong_country == 0 ~ 0,
      country_name == "Ireland" & cong_country == 1 & V001caHUNCZ != 9 ~ 0,
      country_name == "Ireland" & V001caHUNCZ == 9 ~ 1,
      country_name == "Irish traveller" & cong_country == 0 ~ 0,
      country_name == "Irish traveller" & cong_country == 1 & V001caHUNCZ != 10 ~ 0,
      country_name == "Irish traveller" & V001caHUNCZ == 10 ~ 1
    ))

# Adjust names to same format of field experiment dataset
df_nm  <- 
  df_nm |> 
    mutate(name_applicant = str_to_lower(stri_trans_general(Name, id = "Latin-ASCII"))) |> 
    mutate(name_applicant = gsub("'", "", name_applicant)) |> 
    mutate(name_applicant = case_when(
      country_survey == "Ireland" & Name == "Said El Moussaoui" ~ "said elmoussaoui",
      country_survey == "Ireland" & Name == "Naima El Amrani" ~ "naima elamrani",
      country_survey == "Ireland" & Name == "Rachida  El Moussaoui" ~ "rachida elmoussaoui",
      country_survey == "Ireland" & Name == "Ali El Amrani" ~ "naima elamrani",     
      .default = name_applicant
    )) |> 
    mutate(surname_applicant = word(name_applicant, 2))



# Indicate which names were used in the field experiment

exp_names <- unique(df_exp$name_applicant)
df_nm <- df_nm |> mutate(field_exp = if_else(name_applicant %in% exp_names, 1, 0))

# Outcome variable for multinomial model
df_exp <-
    df_exp |>
    mutate(
        lm_outcome = case_when(
            lm_anyresponse == 0 ~ "No response",
            lm_positive == 0 ~ "Other response",
            lm_positive == 1 ~ "Positive"))

# Grouping occupations

df_exp <- df_exp |>
    mutate(
        occup_edu = recode_values(
            occupation,
            c(0, 1, 2, 5) ~ "Low Educ.",
            c(3, 4) ~ "Medium Educ.",
            c(6, 7, 8) ~ "High Educ.",
            default = NA
        ),
        occup_contact = recode_values(
            occupation,
            c(0, 1, 2, 3, 8) ~ "Low contact",
            c(4, 5, 6, 7) ~ "High contact",
            default = NA
        ),
        occup_gender = recode_values(
            occupation,
            c(0, 1, 2, 8) ~ "Men",
            c(3, 5, 6) ~ "Balanced",
            c(4, 7) ~ "Women",
            default = NA
        )
    )


df_exp$occup_edu <- factor( df_exp$occup_edu, levels = c("Low Educ.", "Medium Educ.", "High Educ."))
df_exp$occup_contact <- factor(df_exp$occup_contact, levels = c("Low contact", "High contact"))
df_exp$occup_gender <- factor(df_exp$occup_gender, levels = c("Men", "Balanced", "Women"))
df_exp$country <- factor(df_exp$country)
df_exp$name_group <- df_exp$region_groups
df_exp$name_mingroup <- if_else(df_exp$region_groups %in% c("Roma", "Traveller"), "Roma/Traveller", df_exp$region_groups)
df_exp$name_mingroup <- factor(df_exp$name_mingroup,  levels = c("Majority", "Black", "Muslim", "Roma/Traveller"))

# Add names' mean and sd from name survey to field experiment dataset #####################################

agg_columns  <- c("cong_country", "cong_ethn", "cong_origin", "ladder_adj", "skin_adj", "religiosity")

tb_nm_nat <-
  df_nm |>
    group_by(country_survey) |> 
    mutate(across(all_of(agg_columns),.fns = list(std = ~as.numeric(scale(.x))))) |>
    ungroup() |> 
    summarise(
      across(all_of(agg_columns), ~weighted.mean(.x, w = Weging, na.rm = TRUE), .names = "{.col}_mean"),
      across(all_of(paste0(agg_columns, "_std")), ~weighted.mean(.x, w = Weging, na.rm = TRUE)),
      across(all_of(agg_columns),~sd(.x, na.rm = TRUE), .names = "{.col}_sd"),
      .by = c(name_group, country_survey, name_applicant)) |> 
    mutate(nm_signal_mean = case_when(
      name_group == "Majority" ~ cong_country_mean,
      name_group == "Black" ~ skin_adj_mean,
      name_group == "Muslim" ~ religiosity_mean,
      name_group == "Roma" ~ cong_ethn_mean,
      name_group == "Traveller" ~ cong_ethn_mean)) |> 
    mutate(nm_signal_std = case_when(
      name_group == "Majority" ~ cong_country_std,
      name_group == "Black" ~ skin_adj_std,
      name_group == "Muslim" ~ religiosity_std,
      name_group == "Roma" ~ cong_ethn_std,
      name_group == "Traveller" ~ cong_ethn_std))

df_exp <- 
    df_exp |>
        mutate(
          country_survey = case_when(
            country == "ES" ~ "Spain", country == "IE" ~ "Ireland", .default = NA_character_)) |>
        left_join(tb_nm_nat, by = c("name_applicant", "name_group", "country_survey"))
