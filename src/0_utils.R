
# Getting n and proportion for each outcome
get_prop <- function(df, group_var, count_var){
    df |>
        group_by(Group = as_factor(get(group_var))) |> 
        count(outcome = get(count_var)) |> 
        mutate(p = round(prop.table(n) * 100)) |>
        mutate(value = paste0(n, " (", p, "%)")) |>
        select (-n, -p) |> 
        pivot_wider(names_from = outcome , values_from = value) |>
        mutate(variable = group_var) |>
        select(variable, everything()) |> 
        ungroup()  |> 
        left_join(
            df |>
                group_by(Group = as_factor(get(group_var))) |> 
                summarise(Total = n())
        )
}

# Getting model from different inputs

get_disc_model <- function(df, outcome, covars, type = "lpm", interaction = FALSE, se = "stata", cluster_var) {
  df_temp <- df

  # Transform indep vars to factor if labelled
  for (var in covars) {
    if (haven::is.labelled(df[[var]])) {
      df_temp[[var]] <- factor(haven::as_factor(df_temp[[var]]))
    }
  }

  if (interaction == TRUE) {
    #print("Adding interaction term for the first two variables")
    covars <- c(paste(covars[1:2], collapse = " * "), covars[-(1:2)])
  }

  myform <- as.formula(
    paste0(outcome, " ~ ", paste0(covars, collapse = " + "))
  )

  if (type == "logistic") {
    mymodel <- glm(formula = myform, family = "binomial", data = df_temp)
  } else if (type == "multinomial") {
    mymodel <- nnet::multinom(formula = myform, data = df_temp)
  } else if (type == "lpm") {
    #print("Fitting default Linear Probability Model")
    if (se == "stata"){
      mymodel <- estimatr::lm_robust(formula = myform, se_type = "stata", data = df_temp)
    } else if(se == "cluster"){
      mymodel <- estimatr::lm_robust(formula = myform, se_type = "CR2", clusters = df_temp[[cluster_var]], data = df_temp)
    }
    
  } else {
    print("Indicate type 'logistic', 'multinomial', or 'lpm'")
  }

  return(mymodel)
}

# Getting multinomial model table in wider format
get_tb_wider <- function(x){

  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))

  df$tbl <-
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~dplyr::filter(.x, groupname_col %in% lvl) |> 
            dplyr::ungroup()  |>
            dplyr::select(-groupname_col)
        )
      }
    )
  
  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_level, "**"))
}
