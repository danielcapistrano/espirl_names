# Loading required packages
library(tidyverse) # for data manipulation
library(haven) # to import/export from/to SPSS/STATA formats
library(gt) # formatted tables
library(gtsummary)
library(plotly)
library(marginaleffects)

# Descriptive names ##########################################################

# Table
tab_desc <- 
  df_nm |> 
      filter(country_survey == "Ireland" & field_exp == 1 & !name_group %in% c("Black", "Muslim")) |> 
      mutate(ladder_adj_std = scale(ladder_adj)) |> 
      summarise(
        cong = weighted.mean(cong_ethn, na.rm = T),
        majority = weighted.mean(V001caHUNCZ == 9, w = Weging, na.rm = T),
        minority = weighted.mean(V001caHUNCZ == 10, w = Weging, na.rm = T),
        ses = weighted.mean(ladder_adj, w = Weging, na.rm = T),
        ses_std = weighted.mean(ladder_adj_std, w = Weging, na.rm = T),
        .by = c(country_survey, name_applicant, name_group)) |> 
    bind_rows(
      df_nm |>
        filter(country_survey == "Spain" & field_exp == 1 & !name_group %in% c("Black", "Muslim")) |> 
        mutate(ladder_adj_std = scale(ladder_adj)) |> 
        summarise(
          cong = weighted.mean(cong_ethn, na.rm = T),
          majority = weighted.mean(VRoma == 4, w = Weging, na.rm = T),
          minority = weighted.mean(VRoma == 3, w = Weging, na.rm = T),
          ses = weighted.mean(ladder_adj, w = Weging, na.rm = T),
          ses_std = weighted.mean(ladder_adj_std, w = Weging, na.rm = T),
          .by = c(country_survey, name_applicant, name_group))) |> 
    mutate(name = paste0("n", seq(1:n()))) 

# Plots

dc_plot_desc <- function(main_var, label_y, plot_height = 6, plot_width = 8, save_plot = "png"){
  se_var = paste0(main_var, "_se")
  plot_var <- 
    tab_desc |>  
      ggplot(aes(x = reorder(name, -get(main_var)), y = get(main_var), fill = name_group)) +
      geom_col() +
      labs(x = "Names", y = label_y, fill = "Name origin") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16)
      )
  
  if(save_plot == "html"){
    plotly_plot <- plotly::ggplotly(plot_var, width = plot_width*100, height = plot_height*100)
    htmlwidgets::saveWidget(plotly_plot, paste0("./output/plot_", main_var, ".html"))
  } else if(save_plot == "png"){
    ggsave(plot_var, filename =  paste0("./output/plot_", main_var, ".png"), width = plot_width, height = plot_height, )
  } else {
    return(plot_var)
  }
}

dc_plot_desc(main_var = "majority", label_y = "% identifying name as Majority")
dc_plot_desc(main_var = "minority", label_y = "% identifying name as Roma/Traveller")
dc_plot_desc(main_var = "ses_std", label_y = "Social status of a person with this name")
dc_plot_desc(main_var = "ses", label_y = "Social status of a person with this name")


# Descriptive experiment #####################################################

# Callback rates

plot_callback <- 
  df_exp |>
    group_by(grp = as_factor(region_groups), country_survey) |>
    count(lm_outcome) |>
    mutate(p = prop.table(n) * 100) |>
    ungroup() |> 
    ggplot(aes(x = grp, y = p, fill = lm_outcome)) +
    geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.6) +
    geom_text(
        aes(label = round(p)), 
        position = position_fill(vjust = 0.5, reverse = FALSE),
        size = 3, color = "white"
    ) +
    scale_fill_manual(values = c("grey80", "grey30", "#038131")) +
    theme_classic() +
    facet_wrap(~country_survey, scales = "free_x") +
    theme(legend.position = "bottom", axis.text.x = element_text(size = 12)) +
    labs(y = "Proportion", x = "", fill = "Outcome")

ggsave(plot_callback, filename = "./output/plot_callback.png", width = 6, height = 3.5)

plot_discratio <- 
  df_exp |>
    group_by(grp = as_factor(region_groups), Country = as_factor(country_survey)) |>
    summarise(Risk = mean(lm_positive)) |>
    ungroup() |> 
    pivot_wider(names_from = grp, values_from = Risk) |>
    mutate(across(-Country, ~ Majority / .x)) |>
    select(-Majority) |> 
    pivot_longer(-Country, names_to = "group", values_to = "ratio") |> 
    filter(!is.na(ratio)) |> 
    ggplot(aes(x = reorder(group, -ratio), y = ratio)) +
    geom_col(width = 0.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red" ) +
    geom_text(aes(label = round(ratio, 1)), vjust = 1, color = "white") +
    facet_wrap(~Country, scales = "free_x") +
    labs(x = "Group", y = "Discrimination ratio") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12))

ggsave(plot_discratio, filename = "./output/plot_discratio.png", width = 6, height = 3.5)

dc_plot_cor <- function(mainvar, x_label, save_plot = TRUE){
  plot_cor <- 
    df_exp |> 
      mutate(majority = if_else(name_group == "Majority", "Majority", "Minority")) |> 
      summarise(
        n = n(),
        Strength = mean(get(mainvar), na.rm = TRUE),
        Positive = mean(lm_positive, na.rm = TRUE),
        .by = c(majority, country_survey, name_applicant)) |> 
      filter(Positive > 0) |> 
      ggplot(aes(x = Strength, y = Positive)) +
      geom_point(aes(colour = country_survey)) +
      geom_smooth(method = "lm", colour = "grey70") +
      facet_wrap(~majority, scales = "free_x") +
      labs(x = x_label, y = "Positive response", color = "") +
      theme_classic() +
      theme(
        legend.position = "top", 
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 14)
      )
  
  if(save_plot){
      ggsave(plot_cor, filename = paste0("./output/plot_cor_", mainvar, ".png"), width = 6, height = 3.5)
  } else {
    return(plot_cor)
  }

}

dc_plot_cor("nm_signal_std", "Signal strength of the name")
dc_plot_cor("nm_signal_mean", "Signal strength of the name")
dc_plot_cor("ladder_adj_mean", "Social status of the name")
dc_plot_cor("ladder_adj_std", "Social status of the name")

# Models #####################################################################

vars_control <- c(
  "applicant_female", "parent", "app_citizenship", "past_unemployment",
  "fulltime_job", "childcare_success", "housing_success", "occupation"
)

fit_default_model <- function(covars, outcome = "lm_positive",  interaction = FALSE){
  rhs_vars <- c(covars, vars_control)
  get_disc_model(
    df_exp, outcome = outcome, covars = rhs_vars, 
    se = "cluster", cluster_var = "name_applicant", interaction = interaction
  )
}

get_output_table <- function(m1, m2, vars2){
  tbl_merge(list(
      tbl_regression(m1, include = "name_mingroup", conf.int = FALSE) |> bold_p(),
      tbl_regression(m2, include = c("name_mingroup", vars2), conf.int = FALSE) |> bold_p()),
    tab_spanner = c("Model 1", "Model 2")
  )
}

get_plot_interact <- function(model, mainvar, x_label) {
  plot_predictions(model, condition = c(mainvar, "name_mingroup")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12)) +
    labs(
      x = x_label,
      y = "Predicted positive callback",
      fill = "Ethnic group",
      color = "Ethnic group"
    )
}

# Model comparison
m_naive  <- fit_default_model(covars = c("name_mingroup", "country"))
m_signal  <- fit_default_model(covars = c("name_mingroup", "nm_signal_std", "country"))
get_output_table(m_naive, m_signal, c("name_mingroup", "nm_signal_std"))

# Interaction
m_interact  <- fit_default_model(covars = c("name_mingroup", "nm_signal_std", "country"), interaction = TRUE)
plot_interact <- get_plot_interact(m_interact, "nm_signal_std", "Signal strength of the name")
ggsave(plot_interact, filename = "./output/plot_interact.png", width = 7, height = 4)

# Additional analyses ########################################################

# Social Status
m_interact_ses  <- fit_default_model(covars = c("name_mingroup", "ladder_adj_mean", "nm_signal_std", "country"), interaction = TRUE)

plot_interact_ses <- 
  plot_predictions(m_interact_ses, condition = c("ladder_adj_mean", "name_mingroup")) +
    labs(x="", y="Predicted values") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12)) +
    labs(x = "Social status of the name", y = "Predicted positive callback", fill = "Ethnic group", color = "Ethnic group")

ggsave(plot_interact_ses, filename = "./output/plot_interact_ses.png", width = 7, height = 4)

# Invitation
m_naive_inv  <- fit_default_model(outcome = "lm_invite", covars = c("name_mingroup", "country"))
m_signal_inv  <- fit_default_model(outcome = "lm_invite", covars = c("name_mingroup", "nm_signal_std", "country"))
get_output_table(m_naive_inv, m_signal_inv, c("name_mingroup", "nm_signal_std"))

m_interact_inv  <- fit_default_model(outcome = "lm_invite", covars = c("name_mingroup", "nm_signal_std", "country"), interaction = TRUE)
plot_interact_inv <- get_plot_interact(m_interact, "nm_signal_std", "Signal strength of the name")
ggsave(plot_interact_inv, filename = "./output/plot_interact_inv.png", width = 7, height = 4)

# Rejection
m_naive_rej  <- fit_default_model(outcome = "lm_reject", covars = c("name_mingroup", "country"))
m_signal_rej  <- fit_default_model(outcome = "lm_reject", covars = c("name_mingroup", "nm_signal_std", "country"))
get_output_table(m_naive_rej, m_signal_rej, c("name_mingroup", "nm_signal_std"))

m_interact_rej  <- fit_default_model(outcome = "lm_reject", covars = c("name_mingroup", "nm_signal_std", "country"), interaction = TRUE)
plot_interact_rej <- get_plot_interact(m_interact_rej, "nm_signal_std", "Signal strength of the name")
ggsave(plot_interact_rej, filename = "./output/plot_interact_rej.png", width = 7, height = 4)


# Logistic
m_logit_naive  <- get_disc_model(df_exp, "lm_positive", covars = c("name_mingroup", "country", vars_control), type = "logistic")
m_logit_signal <- get_disc_model(df_exp, "lm_positive", covars = c("name_mingroup", "nm_signal_std", "country", vars_control), type = "logistic")
get_output_table(m_logit_naive, m_logit_signal, c("name_mingroup", "nm_signal_std"))

m_logit_interact <- get_disc_model(
  df_exp, "lm_positive", covars = c("name_mingroup", "nm_signal_std", "country", vars_control), 
  type = "logistic", interaction = TRUE
)

plot_logit_interact <- get_plot_interact(m_logit_interact, "nm_signal_std", "Signal strength of the name")
ggsave(plot_logit_interact, filename = "./output/plot_logit_interact.png", width = 7, height = 4)
