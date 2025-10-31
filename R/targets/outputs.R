# R/targets/outputs.R

# This file defines the final output portion of the pipeline,
# including visualizations, reports, and metadata.

outputs_targets <- list(
  # 10. Visualization (parallel rendering)
  tar_target(
    main_plot,
    create_bmi_bodyfat_plot(cleaned_data, config$outputs$main_plot_file),
    format = "file"
  ),
  
  tar_target(
    bmi_class_plot,
    create_bmi_class_plot(bmi_class_results, config$outputs$bmi_class_plot_file),
    format = "file"
  ),
  
  # 13. Export results
  tar_target(
    results_export,
    export_all_results(
      correlation_results, bmi_class_results, linearity_results,
      sensitivity_results, advanced_stats,
      config$outputs$correlation_results_file,
      config$outputs$bmi_class_results_file,
      config$outputs$methods_file
    ),
    format = "file"
  ),
  
  # 14. Generate report
  tar_target(
    report_html,
    render_quarto_report(results_export, config$outputs$final_report_file),
    format = "file"
  ),
  
  # 15. Pipeline metadata
  tar_target(
    pipeline_metadata,
    list(
      targets_version = packageVersion("targets"),
      future_version = packageVersion("future"),
      furrr_version = packageVersion("furrr"),
      workers = availableCores() - 1,
      created_at = Sys.time()
    )
  )
)

