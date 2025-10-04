R=Rscript

.PHONY: all analysis viz clean help

all: analysis viz

analysis:
	$(R) scripts/nhanes_bmi_bodyfat_analysis.R

viz:
	$(R) scripts/make_visualization.R

clean:
	rm -f outputs/tables/*.csv
	rm -f outputs/figures/*.png outputs/figures/*.pdf
	rm -f outputs/logs/*.txt

help:
	@echo "Available targets:"
	@echo "  all       - Run complete analysis pipeline"
	@echo "  analysis  - Run main NHANES BMI vs body fat analysis"
	@echo "  viz       - Generate publication-ready visualization"
	@echo "  clean     - Remove generated output files"
	@echo "  help      - Show this help message"