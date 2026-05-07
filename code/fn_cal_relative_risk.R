#---
# fn_cal_relative_risk.R
#
#' @title Calculate relative risk from a model's predictions
#'
#' @description
#' Calculate the relative risk (RR) and its approximate 95% confidence interval
#' for a given GAM model, specifically for the effect of `temp_maxC` 
#' (maximum temperature in Celsius) while holding other covariates 
#' at their reference values. 
#' The relative risk is calculated with respect to a user-defined `ref_temp` 
#' (reference temperature).
#' 
#' The fitted GAM formula: 
#' EDvisit ~ s(trend, k) + 
#'           s(temp_maxC, k) + 
#'           s(DTR, k) +
#'           as.factor(day_of_week) +
#'           as.factor(holiday_flag)
#'
#' @param model A fitted GAM object as produced by `mgcv::gam()`.
#' @param ref_temp Numeric. The reference temperature against which
#'   relative risks are calculated. This value should be within the range of 
#'   `temp_maxC` in the `model`'s original data.
#'
#' @return A dataframe containing the predicted values, 
#' standard errors, and calculated relative risks along with their approximate 
#' 95% confidence intervals across a range of `temp_maxC`. 
#' The returned columns include:
#'   \itemize{
#'     \item `trend`: Reference value used for 'trend'.
#'     \item `DTR`: Reference value used for 'DTR' (Diurnal Temperature Range).
#'     \item `day_of_week`: Reference value used for 'day_of_week' (set to Monday).
#'     \item `holiday_flag`: Reference value used for 'holiday_flag' (set to non-holiday).
#'     \item `temp_maxC`: The range of maximum temperatures for which predictions were made.
#'     \item `fit_link`: Predicted values on the link (log) scale.
#'     \item `se_link`: Standard error of predictions on the link scale.
#'     \item `lower_ci_link`: Lower bound of the 95% confidence interval on the link scale.
#'     \item `upper_ci_link`: Upper bound of the 95% confidence interval on the link scale.
#'     \item `rr`: Calculated relative risk.
#'     \item `se_log_rr_approx`: Approximate standard error of log(RR) used for CI calculation.
#'     \item `rr_lower_ci`: Lower bound of the approximate 95% confidence interval for the relative risk.
#'     \item `rr_upper_ci`: Upper bound of the approximate 95% confidence interval for the relative risk.
#'   }
#'
#---

cal_relative_risk <- function(model,
                              ref_temp,
                              temp_range = seq(-5, 40, length.out = 500)
                              ){
        
        # get original data
        data <- model$model
        # 1. Choose reference values for non-varying covariates
        # For continuous variables, often the mean or median is used.
        # For factors, pick a reference level (e.g., the most common one, or the first level).
        
        # reference values
        ref_data <- data.frame(
                trend = mean(data$trend),
                DTR = mean(data$DTR),
                day_of_week = as.factor(2), # Monday, highest effect
                holiday_flag = as.factor(1) # non-holiday
        )
        
        # 2. Create new data for prediction, varying only temp_maxC
        predict_data <- ref_data %>%
                tibble::tibble() %>% 
                # Replicate the single reference row
                dplyr::slice(rep(1, length(c(temp_range, ref_temp)))) %>% 
                dplyr::mutate(temp_maxC = c(temp_range, ref_temp))
        
        # 3. Get Predictions and Standard Errors on the Link Scale
        # 'type = "link"' returns log(EDvisit) predictions.
        # 'se.fit = TRUE' returns the standard errors for these log predictions.
        preds <- mgcv::predict.gam(model,
                                   newdata = predict_data,
                                   type = "link",
                                   se.fit = TRUE)

        # 4. Calculate Confidence Intervals on the Link Scale ---
        # For a 95% CI, the critical z-value is 1.96.
        z_crit <- qnorm(0.975)
        
        predict_data <- predict_data %>%
                dplyr::mutate(fit_link = preds$fit,
                              se_link = preds$se.fit,
                              lower_ci_link = fit_link - z_crit * se_link,
                              upper_ci_link = fit_link + z_crit * se_link)

        # 5. Find the predicted link value at the chosen reference temperature
        # pick the closest point in our `predict_data` grid to the `ref_temp_val`.
        ref_row <- predict_data %>%
                filter(temp_maxC == ref_temp) %>%
                slice(1) # In case of ties, take the first one
        
        ref_fit_link <- as.numeric(ref_row$fit_link)
        ref_se_link <- as.numeric(ref_row$se_link)
        
        # 6. Calculate relative risk and its approximate confidence interval
        predict_df <- predict_data %>%
                slice(-n()) %>% 
                dplyr::mutate(
                        # Relative Risk
                        rr = exp(fit_link - ref_fit_link),
                        # Standard error of log(RR)
                        se_log_rr_approx = se_link,
                        # Approximate 95% confidence interval for RR
                        rr_lower_ci = exp( (fit_link - ref_fit_link) - z_crit * se_log_rr_approx ),
                        rr_upper_ci = exp( (fit_link - ref_fit_link) + z_crit * se_log_rr_approx )
                )
        return(predict_df)
}

