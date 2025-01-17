# Helper function for formatting percentages and statistics
format_table <- function(cont_tab) {
    row_percents <- prop.table(cont_tab, margin = 1) * 100
    col_percents <- prop.table(cont_tab, margin = 2) * 100
    
    formatted_table <- NULL
    for(i in 1:nrow(cont_tab)) {
        formatted_table <- rbind(
            formatted_table,
            col_percents[i, ],  # Column percentages
            row_percents[i, ]   # Row percentages
        )
    }
    # Create row names
    row_labels <- rep(rownames(cont_tab), each = 2)
    row_types <- rep(c("(Column %)", "(Row %)"), length(rownames(cont_tab)))
    rownames(formatted_table) <- paste(row_labels, row_types)
    
    return(formatted_table)
}

generate_confounding_table1 <- function(full_data) {
    # Optimism categories
    tertile1 <- quantile(full_data$B1SORIEN, 1/3)
    tertile2 <- quantile(full_data$B1SORIEN, 2/3)
    
    full_data$optimism_category <- factor(case_when(
        full_data$B1SORIEN < tertile1 ~ "low",
        full_data$B1SORIEN >= tertile1 & full_data$B1SORIEN < tertile2 ~ "moderate",
        full_data$B1SORIEN >= tertile2 ~ "high"
    ), levels = c("low", "moderate", "high"))
    
    # Age summary
    age_summary <- full_data %>% 
        group_by(optimism_category) %>% 
        summarize(mean_sd = sprintf("%.2f ± %.2f", mean(age), sd(age))) %>%
        t()
    
    # Education
    print("Education Level")
    education_table <- format_table(table(full_data$education_categorical, 
                                          full_data$optimism_category))
    print(education_table)
    print(chisq.test(table(full_data$education_categorical, full_data$optimism_category)))
    
    # Race
    print("Race")
    race_table <- format_table(table(full_data$race, full_data$optimism_category))
    print(race_table)
    print(chisq.test(table(full_data$race, full_data$optimism_category)))
    
    # Gender
    print("Gender")
    gender_table <- format_table(table(full_data$sex, full_data$optimism_category))
    print(gender_table)
    print(chisq.test(table(full_data$sex, full_data$optimism_category)))
    
    # Chronic condition
    print("Chronic Condition")
    chronic_table <- format_table(table(full_data$chronic_condition, 
                                        full_data$optimism_category))
    print(chronic_table)
    print(chisq.test(table(full_data$chronic_condition, full_data$optimism_category)))
    
    # Income and visit interval
    print("Income and Visit Interval")
    interval_income_summary <- full_data %>% 
        group_by(optimism_category) %>% 
        summarize(
            visit_interval = sprintf("%.2f ± %.2f", mean(visit_interval), sd(visit_interval)),
            household_income = sprintf("%.2f ± %.2f", mean(household_income), sd(household_income))
        ) %>%
        t()
    
    # Blood pressure medication
    print("Blood Pressure Medication")
    bp_table <- table(full_data$blood_pressure_med, full_data$optimism_category)
    print(format_table(bp_table))
    print(chisq.test(table(full_data$blood_pressure_med, full_data$optimism_category)))
    
    # Negative affect
    print("Negative Affect")
    negative_affect_summary <- full_data %>% 
        group_by(optimism_category) %>% 
        summarize(mean_sd = sprintf("%.2f ± %.2f", 
                                    mean(negative_affect), 
                                    sd(negative_affect))) %>%
        t()
    print(negative_affect_summary)
    print(chisq.test(bp_table))
}
