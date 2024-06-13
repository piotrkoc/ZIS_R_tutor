#===============================================================================
# STEP 1: Data & settings
#-------------------------------------------------------------------------------
# Required packages: lavaan, psych
Data <- lavaan::HolzingerSwineford1939

#===============================================================================
# STEP 2: ZIS - Sample
#-------------------------------------------------------------------------------
# Describe the characteristic features of the sample(s) (e.g., gender, age,
# educational achievement, mother language, socioeconomic status, geographic region).

# Descriptive statistics for nominal sample characteristics
# Define nominal variables
nominal_variables <- c("sex", "school", "grade")

# Estimation for a single variable
table(Data$sex,
      useNA = "always")

# Estimate descriptive statistics
nominal_sample_statistics <- list()
for (variable in nominal_variables) {
  frequency_table <- table(Data[, variable],
                           useNA = "always")
  nominal_sample_statistics[[variable]] <- frequency_table
}
nominal_sample_statistics

# Descriptive statistics for metric sample characteristics
   # Compute aggregated age variable of year and months
Data$age <- (Data$ageyr * 12 + Data$agemo) / 12

# Estimate mean, sd, skew, kurtosis, and percentage missing
mean(Data$age, na.rm = TRUE)
sd(Data$age, na.rm = TRUE)
psych::skew(Data$age, na.rm = TRUE)
psych::kurtosi(Data$age, na.rm = TRUE)
sum(is.na(Data$age)) / nrow(Data)

#===============================================================================
# STEP 3: ZIS - Item analyses: Dimensionality
#-------------------------------------------------------------------------------
# Build lookup table for required items
lookup_table <- data.frame(item = paste("x", c(1:9), sep = ""),
                           subscale = c(rep(c("visual", "textual", "speed"),
                                            each = 3))
                           )

#===============================================================================
# STEP 3.1: Exploratory factor analyses
#-------------------------------------------------------------------------------
# Method to explore dimensionality: efa
# extraction method: minimal residuals (minres, also known as ordinary least square)
# rotation method: oblimin (oblique rotation)

# Conduct parallel analysis display screeplot
parallel_analyses_efa <- psych::fa.parallel(Data[ , unlist(lookup_table$item)],
                                            fa = "fa",
                                            fm = "minres", nfactors = 1, SMC = F,
                                            n.iter = 20, # quant = .95,
                                            cor = "cor")
# Display eigen values
parallel_analyses_efa$fa.values

# Estimate exploratory factor analyses
efa <- psych::fa(Data[ , unlist(lookup_table$item)],
                 fm = "pa",
                 nfactors = 3,
                 rotate = "oblimin",
                 scores = "regression", oblique.scores = F,
                 SMC = TRUE,
                 cor = "cor")
efa

# matrix of factor loadings
round(efa[["loadings"]], digits = 2)

# factor intercorrelation
round(efa[["Phi"]], digits = 2)

# item communalities
round(efa[["communality"]], digits = 2)

# eigen values
round(efa[["values"]], digits = 2) # or e.values?

#===============================================================================
# STEP 3.2: Confirmatory factor analyses (CFA)
#-------------------------------------------------------------------------------
# Method to explore dimensionality: cfa
# Model estimator: MLR
# Handling of missing data: no missing data
# model specifications: Compare 'Define models'
# model fit indices: Compare 'Fit evaluation'

# Define models
one_fator_model <- '
g_factor =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

three_fator_model <- '
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
'

three_factor_tau_model <- '
visual =~ a*x1 + a*x2 + a*x3
textual =~ b*x4 + b*x5 + b*x6
speed =~ c*x7 + c*x8 + c*x9
'

# Estimate cfa
# One factor model
one_factor_cfa <- lavaan::cfa(model = one_fator_model,
                              data = Data,
                              estimator = 'mlr',
                              std.lv = TRUE)
lavaan::summary(one_factor_cfa,
                fit.measures = TRUE,
                standardized = TRUE)

# Three factor model
three_factor_cfa <- lavaan::cfa(model = three_fator_model,
                                data = Data,
                                estimator = 'mlr',
                                std.lv = TRUE)
lavaan::summary(three_factor_cfa,
                fit.measures = TRUE,
                standardized = TRUE)

# Three factor model model with essential tau equivalence
three_factor_tau_cfa <- lavaan::cfa(model = three_factor_tau_model,
                                    data = Data,
                                    estimator = 'mlr',
                                    std.lv = TRUE)
lavaan::summary(three_factor_tau_cfa,
                fit.measures = TRUE,
                standardized = TRUE)

# model fit
# Define fit measures of interest
# use robust versions
fit_measures <- c("chisq", "df", "pvalue",
                  "cfi.robust", "rmsea.robust", "srmr",
                  "aic", "bic", "bic2")

# Extract model fit
round(lavaan::fitMeasures(one_factor_cfa, fit.measures = fit_measures), digits = 3)
round(lavaan::fitMeasures(three_factor_cfa, fit.measures = fit_measures), digits = 3)
round(lavaan::fitMeasures(three_factor_tau_cfa, fit.measures = fit_measures), digits = 3)

# Evaluate local model fit
# Extract residual correlaton matrix
lavaan::lavResiduals(one_factor_cfa)$cov
lavaan::lavResiduals(three_factor_cfa)$cov
lavaan::lavResiduals(three_factor_tau_cfa)$cov

# Extract standardized factor loadings
lavaan::lavInspect(one_factor_cfa, "std")$lambda

# Extract standardized factor loadings & interfactor correlation
lavaan::lavInspect(three_factor_cfa, "std")$lambda
lavaan::lavInspect(three_factor_cfa, "std")$psi

lavaan::lavInspect(three_factor_tau_cfa, "std")$lambda
lavaan::lavInspect(three_factor_tau_cfa, "std")$psi

#===============================================================================
# STEP 4: ZIS - Item parameter
#-------------------------------------------------------------------------------
# For standardized factor loadings (i.e., selectivity) compare 3.3
# Estimate descriptive statistics
# Mean, SD, Skewness, Excess kurtosis
psych::describe(Data[ , unlist(lookup_table$item)])

#===============================================================================
# STEP 5: ZIS - Reliability
#-------------------------------------------------------------------------------
# Estimate Cronbachs alpha as example for scale "textual"
textual_alphas <- psych::alpha(Data[, lookup_table[lookup_table$subscale == "textual", "item"]],
                               n.iter = 1000)

# Extract point estimated of alpha
round(textual_alphas[["total"]][["raw_alpha"]], digits = 2)

# Extract confidence intervals for point estimates
round(textual_alphas[["boot.ci"]], digits = 2) 

# McDonalds omega
semTools::reliability(one_factor_cfa)
semTools::reliability(three_factor_cfa)
semTools::reliability(three_factor_tau_cfa)

#===============================================================================
# STEP 6: ZIS - Validity: Criterion validity
#-------------------------------------------------------------------------------
# Build scale scores by unweighted means
for(subscale in unique(lookup_table$subscale)) {
  subscale_name <- paste(subscale, "mean", sep = "_")
  items <- lookup_table[lookup_table$subscale == subscale, "item"]
  mean_score <- rowMeans(Data[, items], na.rm = FALSE)
  Data[, subscale_name] <- mean_score
  }

# Transform school variable to a numeric variable
Data$school_numeric <- as.numeric(Data$school)
  
# Estimate correlation with sex, age, school, and grade
# Define variables for correlational analyses
cor_variables <- c("visual_mean", "textual_mean", "speed_mean",
                   "sex", "age", "school_numeric", "grade")

# Estimate correlations
cor(Data[, cor_variables],
    use = "pairwise.complete.obs")

#===============================================================================
# STEP 7: ZIS - Further quality criteria: Measurement invariance
#-------------------------------------------------------------------------------
# Estimate measurement invariance across sex
# Estimate multi group cfa to evaluate cofigural measurement invariance
configural_model <- lavaan::cfa(model = three_fator_model,
                                data = Data,
                                estimator = 'mlr',
                                group = 'sex',
                                std.lv = TRUE)
lavaan::summary(configural_model,
                fit.measures = TRUE,
                standardized = TRUE)

# Estimate multi group cfa to evaluate metric measurement invariance
metric_model <- lavaan::cfa(model = three_fator_model,
                            data = Data,
                            estimator = 'mlr',
                            group = 'sex',
                            group.equal = c("loadings"),
                            std.lv = TRUE)
lavaan::summary(metric_model,
                fit.measures = TRUE,
                standardized = TRUE)

# Estimate multi group cfa to evaluate scalar measurement invariance
scalar_model <- lavaan::cfa(model = three_fator_model,
                            data = Data,
                            estimator = 'mlr',
                            group = 'sex',
                            group.equal = c("loadings", "intercepts"),
                            std.lv = TRUE)
lavaan::summary(scalar_model,
                fit.measures = TRUE,
                standardized = TRUE)



# Extract model fit of multi-group cfa
# Define models
list_mi_cfa <- c(configural_model, metric_model, scalar_model)

# Extract model fit
fit_MI <- round(lavaan::fitMeasures(list_mi_cfa[[1]], fit.measures = fit_measures), digits = 3)

for(i in c(2:length(list_mi_cfa))) {
  fit_MI <- rbind(fit_MI,
    round(lavaan::fitMeasures(list_mi_cfa[[i]], fit.measures = fit_measures), digits = 3)
  )
}
rownames(fit_MI) <- c("cofigural_model", "metric_model", "scaler_model")

#===============================================================================
# STEP 8: ZIS - Descriptive statistics
#-------------------------------------------------------------------------------
psych::describe(Data[, c("visual_mean", "textual_mean", "speed_mean")])
