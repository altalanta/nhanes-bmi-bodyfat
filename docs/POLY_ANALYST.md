# Poly-Analyst Framework: A Multi-Source Data Integration & Harmonization Engine

## 1. 🌐 Overview

The Poly-Analyst Framework is an advanced component of the NHANES BMI Body Fat Analysis Platform designed to move beyond single-source analysis. It provides the infrastructure to ingest, harmonize, and analyze data from multiple, disparate epidemiological studies simultaneously. This enables powerful meta-analyses, comparative studies, and the generation of more robust and generalizable scientific findings.

### Key Capabilities:
- **Multi-Source Integration**: Seamlessly load data from different studies, such as NHANES and the UK Biobank.
- **Modular Data Connectors**: An extensible system for adding new data sources with minimal code changes.
- **Ontology-Driven Harmonization**: A sophisticated engine that uses a centralized "ontology" to standardize variable names, formats, and codings across studies.
- **Integrated Meta-Analysis**: Perform meta-analyses directly on the harmonized data to synthesize evidence across studies.
- **Automated Pipeline**: A simple command-line interface to orchestrate the entire complex workflow.

## 2. 🏛️ Framework Architecture

The framework is built on a modular and configurable architecture, centered around the `R/poly_analyst_framework.R` script.

### Core Components:
1.  **Configuration Object (`POLY_ANALYST_CONFIG`)**: A centralized list in R that defines all aspects of the pipeline.
    *   `data_sources`: A registry of available studies, including their metadata and connector configurations.
    *   `harmonization_ontology`: The "dictionary" that defines the rules for standardizing variables (e.g., mapping `RIDAGEYR` in NHANES to a standard `age_years` variable).
    *   `meta_analysis_settings`: Parameters for conducting the meta-analysis, such as the statistical model to use.

2.  **Data Connectors**: A factory system (`create_data_connector`) that generates the appropriate data-loading logic based on the source type (e.g., `nhanes_local`, `csv_local`). This makes the system easily extensible to databases, APIs, or other file formats.

3.  **Harmonization Engine (`harmonize_dataset`)**: This is the core of the framework. It iterates through the variables defined in the ontology, extracts the corresponding data from the source-specific column, performs any required transformations (like recoding categorical values), and maps it to a standard output column.

4.  **Integration & Analysis**: Functions to combine the harmonized datasets (`integrate_harmonized_data`) and run a meta-analysis (`run_meta_analysis`) on the pooled data.

## 3. 🚀 How to Use the Framework

The entire Poly-Analyst pipeline can be executed with a single command, thanks to its integration with the project's `Makefile`.

### Running the Pipeline:
Simply run the following command from your terminal at the root of the project:
```bash
make poly-analyst
```
This command will:
1.  Execute the `scripts/poly_analyst.R` script.
2.  Load data from the configured sources (NHANES and the mock UK Biobank data).
3.  Harmonize the datasets according to the ontology.
4.  Combine them into a single, integrated dataset.
5.  Perform a meta-analysis of the correlation between BMI and body fat percentage across the studies.
6.  Save all outputs to the `outputs/` directory.

## 4. 📄 Outputs

After a successful run, the following files will be generated:

-   `outputs/derived_data/integrated_harmonized_data.csv`: The final, analysis-ready dataset containing the harmonized data from all sources.
-   `outputs/derived_data/meta_analysis_results.rds`: An R object containing the full results of the `meta` package analysis.
-   `outputs/tables/meta_analysis_summary.txt`: A human-readable summary of the meta-analysis results.
-   `outputs/tables/integration_report_summary.txt`: A summary of the data integration process, including record counts from each source.
-   `outputs/figures/meta_analysis_forest_plot.png`: A forest plot visualizing the results of the meta-analysis.

## 5. 🛠️ Extending the Framework

The Poly-Analyst Framework is designed for extension.

### Adding a New Data Source:
1.  **Add a Configuration**: Open `R/poly_analyst_framework.R` and add a new entry to the `data_sources` list in `POLY_ANALYST_CONFIG`. Define its `id`, `name`, `connector_type`, and any parameters (like file paths).
    ```r
    my_new_study = list(
      id = "my_study",
      name = "My Awesome New Study (Mock)",
      connector_type = "csv_local", # Or a new connector type
      params = list(file_path = "data/raw/my_study_data.csv")
    )
    ```

2.  **Update the Ontology**: For each variable in the `harmonization_ontology`, add a mapping for your new data source's variable name.
    ```r
    bmi = list(
      standard_name = "bmi_kg_m2",
      sources = list(
        nhanes = "BMXBMI",
        uk_biobank = "body_mass_index",
        my_study = "BMI_value" # Add your study's variable name
      )
    )
    ```

3.  **(Optional) Create a New Connector**: If your data is not in a local CSV or RDS file, you can add a new `else if` block to the `create_data_connector` function to handle your specific data format.

4.  **Run the Pipeline**: Update the `sources_to_analyze` vector in `scripts/poly_analyst.R` to include your new study's ID and re-run `make poly-analyst`.
