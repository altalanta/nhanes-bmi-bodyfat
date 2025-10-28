# The Dissemination Engine: Automated Publication & Interactive Reporting Suite

## 1. 🚀 Overview

The Dissemination Engine is a powerful suite of tools designed to automate the "last mile" of the research lifecycle. It transforms the raw outputs of the analysis pipelines (such as tables, figures, and statistical models) directly into high-quality, communication-ready artifacts. This drastically reduces the manual effort required to prepare findings for publication, presentation, and sharing, while simultaneously improving reproducibility.

### Key Features:
- **Automated Manuscript Generation**: Create publication-ready manuscripts in PDF or DOCX format from a customizable Quarto template with a single command.
- **Interactive Dashboard Generation**: Instantly generate a self-contained, interactive Shiny web application to explore key results visually.
- **One-Click Reproducibility Packages**: Bundle the entire analysis—including code, environment, and data pointers—into a single, shareable zip archive to ensure full reproducibility.

## 2. ⚙️ How to Use the Engine

All features of the Dissemination Engine are accessible via simple `make` commands from your terminal.

*Note: The dissemination targets depend on the successful completion of the main analysis pipeline. Run `make all` first to ensure all required analysis artifacts are present.*

### Generate a Manuscript
To generate a professional, publication-ready manuscript in PDF format:
```bash
make manuscript
```
This command will create a file named `manuscript_YYYY-MM-DD.pdf` in the `outputs/manuscripts/` directory.

To generate the same manuscript in Microsoft Word (DOCX) format:
```bash
make manuscript-docx
```

### Generate an Interactive Dashboard
To create a shareable, interactive Shiny dashboard for exploring the meta-analysis results:
```bash
make dashboard
```
This will create a new directory (e.g., `outputs/dashboards/meta_analysis_dashboard_YYYY-MM-DD/`). To run the dashboard, open R and execute:
```r
shiny::runApp("outputs/dashboards/meta_analysis_dashboard_YYYY-MM-DD/")
```

### Create a Reproducibility Package
To bundle the entire project into a single zip file for sharing or archiving:
```bash
make repro-package
```
This will create a file named `repro_package_YYYY-MM-DD.zip` in the `outputs/repro_packages/` directory.

## 3. 🏛️ Architecture & Customization

The engine is designed to be both powerful and customizable. The core logic resides in `R/dissemination_engine.R`, with templates located in the `templates/` directory.

### Customizing the Manuscript
You can easily modify the content and style of the generated manuscript by editing the Quarto template file: `templates/manuscript_template.qmd`.

- **Metadata**: Change the default title, authors, or abstract by editing the `project_metadata` list within `R/dissemination_engine.R`.
- **Text**: Edit the Markdown text directly within the `.qmd` file to change the introduction, conclusion, or other narrative sections.
- **Analysis**: The template uses R code chunks to load results directly from the `outputs` directory. You can modify these chunks to change which results are displayed or how they are formatted.

### Architecture
- **`R/dissemination_engine.R`**: Contains the core functions for `generate_manuscript`, `generate_interactive_dashboard`, and `create_reproducibility_package`. It handles the logic of checking for files, copying templates, and calling external tools like Quarto.
- **`scripts/disseminate.R`**: A command-line interface that parses user arguments and calls the appropriate function from the core engine.
- **`templates/`**: This directory holds the building blocks for the generated artifacts.
  - `manuscript_template.qmd`: A parameterized Quarto file that defines the structure and content of the manuscript.
  - `dashboard_app.R`: A self-contained Shiny application script that serves as the template for the interactive dashboard.

This modular structure allows for advanced customization. For example, you could add new Quarto templates for different journal formats or create new dashboard templates to visualize different aspects of the analysis.

