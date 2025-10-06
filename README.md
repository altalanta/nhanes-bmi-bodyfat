# CytoFlow-QC

Automated, reproducible quality control, compensation, and gating for flow cytometry experiments. CytoFlow-QC replaces fragile manual FlowJo pipelines with a scriptable workflow that ingests FCS batches, applies spillover compensation, performs automated QC + gating, detects batch drift, runs basic effect-size statistics, and emits a publication-ready HTML report.

## Install

### Conda / mamba

```bash
mamba env create -f env/environment.yml
mamba activate cytoflow-qc
pip install -e .[dev]
```

### Docker

```bash
docker build -t cytoflow-qc .
docker run --rm -v $(pwd):/workspace cytoflow-qc cytoflow-qc --help
```

## Quickstart

1. Drop FCS files (or the provided synthetic CSV surrogates) into `data/raw/`.
2. Copy/modify `samplesheets/example_samplesheet.csv` for your experiment.
3. Tune `configs/example_config.yaml` for channel names and QC heuristics.
4. Run the end-to-end pipeline:
   ```bash
   cytoflow-qc run --samplesheet samplesheets/example_samplesheet.csv \
     --config configs/example_config.yaml --out results
   ```
5. Open `results/report.html` to review QC, gating, drift, and effect-size outputs.

Reusable Make targets:

- `make setup` – create the conda env and install pre-commit hooks.
- `make lint` / `make format` – ruff + black.
- `make test` – pytest suite with synthetic data.
- `make smoke` – one-shot CLI run that checks the HTML report exists.
- `make report` – rebuild the HTML report from existing artifacts.

## Methods (brief)

- **Compensation:** auto-detect `$SPILLOVER` matrices or accept external CSV overrides. Compensation is applied only to matched fluorescence channels.
- **QC:** debris removal via low-percentile FSC/SSC thresholds, doublet detection via FSC-H vs FSC-A deviation, saturation + dynamic range metrics, and aggregate per-sample summaries.
- **Gating:** default strategy composes debris removal, singlet selection, lymphocyte density gate, and optional viability dye thresholding. Additional strategies can plug into `gate.auto_gate`.
- **Batch drift:** per-sample feature tables (medians, IQRs, gated fractions) feed ANOVA/Kruskal tests and PCA/UMAP projections to flag drifted batches.
- **Statistics:** effect sizes (Hedges' g, Cliff's delta) and Mann–Whitney tests across experimental conditions with Holm–Bonferroni correction.
- **Reporting:** static plots (Matplotlib) and tables rendered into HTML via Jinja2 templates or nbconvert workflows.

## Outputs

A typical single-run layout under `results/`:

```
results/
├─ ingest/
│  ├─ manifest.csv
│  └─ events/<sample>.parquet
├─ compensate/
│  └─ events/<sample>.parquet
├─ qc/
│  ├─ events/<sample>.parquet
│  └─ summary.csv
├─ gate/
│  ├─ events/<sample>.parquet
│  ├─ params/<sample>.json
│  └─ summary.csv
├─ drift/
│  ├─ features.csv
│  ├─ tests.csv
│  ├─ pca.csv
│  └─ figures/*.png
├─ stats/
│  └─ effect_sizes.csv
└─ report.html
```

## Extending

- Custom gates: add new primitives in `src/cytoflow_qc/gate.py` and expose them via the Typer CLI options.
- Additional QC heuristics: extend `src/cytoflow_qc/qc.py` and accompanying config schema.
- Alternate reporting: adapt `configs/report_template.html.j2` or drive a Quarto/nbconvert notebook via `notebooks/report_notebook.ipynb`.

## Citation

If you use CytoFlow-QC in a publication, please cite this repository and include the MIT license notice.

## License

MIT License – see `LICENSE` for details.
