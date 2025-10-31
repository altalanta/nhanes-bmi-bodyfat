# R/targets/analysis.R

# This file defines the statistical analysis portion of the pipeline,
# including creating the survey design and running the core analyses.

analysis_targets <- list(
  # 6. Create survey design (cached)
  tar_target(
    survey_design,
    create_survey_design(cleaned_data, config)
  ),
  
  # 7. Correlation analysis (parallel by sex)
  tar_target(
    correlation_results,
    compute_correlations_parallel(survey_design)
  ),
  
  # 8. BMI class analysis (parallel processing)
  tar_target(
    bmi_class_results,
    compute_bmi_class_stats_parallel(survey_design, cleaned_data)
  ),
  
  # 9. Linearity assessment
  tar_target(
    linearity_results,
    assess_linearity(survey_design)
  ),
  
  # 11. Advanced analyses (optional, parallel)
  tar_target(
    sensitivity_results,
    run_sensitivity_analyses(survey_design, cleaned_data),
    deployment = "worker" # Run in background
  ),
  
  tar_target(
    advanced_stats,
    run_advanced_statistics(survey_design, cleaned_data),
    deployment = "worker"
  )
)

