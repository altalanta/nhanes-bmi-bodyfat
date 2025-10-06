FROM python:3.11-slim

ENV POETRY_VERSION=1.8.3 \
    PYTHONUNBUFFERED=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    CYTOFLOW_QC_HOME=/opt/cytoflow-qc

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR ${CYTOFLOW_QC_HOME}

COPY pyproject.toml README.md LICENSE ./
COPY env/requirements.txt env/requirements.txt

RUN pip install --upgrade pip && pip install -r env/requirements.txt

COPY src src
COPY configs configs
COPY samplesheets samplesheets
COPY notebooks notebooks

RUN pip install -e .

ENTRYPOINT ["cytoflow-qc"]
CMD ["--help"]
