# NHANES 2017-2018 BMI vs Body Fat Analysis

Analysis of the association between Body Mass Index (BMI) and whole-body percent body fat using NHANES 2017-2018 data for U.S. civilian non-institutionalized adults aged 20-59 years.

## Overview

This analysis uses design-based (survey-weighted) statistical methods to estimate:

- Survey-weighted Pearson correlations between BMI and DXA-measured % body fat
- Mean % body fat by BMI class and sex with 95% confidence intervals
- Distribution of % body fat (5th, 50th, 95th percentiles) by BMI class and sex
- Assessment of linearity between BMI and % body fat

All estimates account for NHANES complex sampling design using MEC examination weights (WTMEC2YR), stratification (SDMVSTRA), and primary sampling units (SDMVPSU) with Taylor linearization for variance estimation.

## Key Results

- **Overall BMI-body fat correlation**: 0.914 (95% CI: 0.885-0.943)
- **Male correlation**: 0.917 (95% CI: 0.885-0.949)  
- **Female correlation**: 0.954 (95% CI: 0.941-0.967)
- **Sample size**: 2,240 adults with complete data
- **Significant non-linearity detected** (BMI² term p = 0.007)

## Reproduction

### Data Requirements

Raw NHANES 2017-2018 data files (not included in this repository):
- `DEMO_J.XPT` - Demographics
- `BMX_J.XPT` - Body measures  
- `DXX_J.XPT` - DXA whole-body scan
- `DXXAG_J.XPT` - DXA android/gynoid

Expected locations:
- `~/Downloads/DEMO_J.xpt`
- `~/Downloads/BMX_J.xpt` 
- `~/Downloads/DXX_J.xpt`
- `~/Downloads/DXXAG_J.xpt`

Alternatively, place files in `data/raw/` directory.

### Running the Analysis

```bash
# Run complete analysis
make all

# Or run components separately
make analysis    # Generate tables and initial plots
make viz        # Create publication-ready visualization
```

### Requirements

- R with packages: foreign, survey, dplyr, ggplot2, tidyr, patchwork, cowplot
- NHANES 2017-2018 data files (download separately from CDC)

## File Structure

- `scripts/` - R analysis scripts
- `outputs/tables/` - CSV result tables
- `outputs/figures/` - PNG/PDF visualizations  
- `outputs/logs/` - Analysis logs
- `reports/` - Methods documentation
- `data/raw/` - Raw NHANES files (not tracked)
- `data/derived/` - Processed datasets (not tracked)

## Methods

See `reports/methods.txt` for detailed methodology including:
- Sample inclusion/exclusion criteria
- Survey design specifications
- Statistical methods and software versions
- Missing data handling

## License

MIT License - see LICENSE file for details.