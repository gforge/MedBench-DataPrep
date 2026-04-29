# MedBench-DataPrep

## Overview

This repository prepares fictional Electronic Health Records (EHRs) for the MedBench study, which benchmarks Large Language Models on medical discharge summary generation. It also contains the scripts for running automated metric evaluation (BLEU/ROUGE) after LLM inference.

The full pipeline spans three repositories:

| Step                    | Repository                       | What happens                                        |
| ----------------------- | -------------------------------- | --------------------------------------------------- |
| 1. Download charts      | **DataPrep** (this repo)         | Export case data from the Platform → `allData.json` |
| 2. Convert to LLM input | **DataPrep**                     | R pipeline converts Word/Excel → merged markdown    |
| 3. Run LLM              | [MedBench/LLM](../LLM)           | Generate discharge summaries                        |
| 4. Upload summaries     | [MedBench/LLM](../LLM)           | Push `.txt` outputs back to the Platform            |
| 5. Manual evaluation    | [MedBench/Platform](../Platform) | Create a review campaign in the UI                  |
| 6. Automated evaluation | **DataPrep** (this repo)         | BLEU/ROUGE scoring                                  |

---

## Step 1: Download charts from the Platform

The R pipeline requires `data/output/allData.json`, which is the Platform's export of all case data. This file also provides the `chartTranslationId` values needed for uploading summaries in Step 4.

**Prerequisites:**

- The Platform is running (development or production)
- You have an admin account

**Run:**

```bash
cd /path/to/MedBench/DataPrep

# Login interactively (recommended)
python download_platform_charts.py \
    --url https://label.cairlab.ki.se/graphql \
    --email admin@example.com

# Or pass a JWT token directly
python download_platform_charts.py \
    --url https://label.cairlab.ki.se/graphql \
    --token <jwt_token>

# Or set environment variables
export MEDBENCH_URL=https://label.cairlab.ki.se/graphql
export MEDBENCH_EMAIL=admin@example.com
export MEDBENCH_PASSWORD='yourpassword'
python download_platform_charts.py
```

> Do not pass the password directly on the command line to avoid leaking it in shell history.

**Output:** `data/output/allData.json`

---

## Step 2: Convert charts to LLM input

The R pipeline reads chart data from `data/output/allData.json` and writes three output formats for downstream LLM use.

**Prerequisites:**

- R with packages: `tidyverse`, `glue`, `magrittr`, `lubridate`, `officer`, `readxl`, `jsonlite`, `knitr`, `snakecase`
- `data/output/allData.json` exists (from Step 1)

Note: The build no longer depends on local `.docx` / `.xlsx` files.

**Run:**

```bash
cd /path/to/MedBench/DataPrep
Rscript build_processed_output.R
```

**Outputs in `data/processed/`:**

| Format          | Path                                                     | Used by                    |
| --------------- | -------------------------------------------------------- | -------------------------- |
| Raw JSON        | `raw/raw_{Specialty}_{CaseID}_{Language}.json`           | Reference / debugging      |
| Markdown JSON   | `markdown/markdown_{Specialty}_{CaseID}_{Language}.json` | Alternative LLM input      |
| Merged markdown | `merged/merged_{Specialty}_{CaseID}_{Language}.md`       | **LLM inference (Step 3)** |

After running, copy the processed data to the LLM repository:

```bash
rsync -a data/processed/ ../LLM/data/processed/
```

**Next:** Proceed to **Step 3: LLM inference** in the [MedBench/LLM README](../LLM/README.md#running-evaluations).

---

## Steps 3–5: LLM inference, upload, and manual evaluation

See the [MedBench/LLM README](../LLM/README.md) and [MedBench/Platform README](../Platform/README.md) for these steps.

---

## Step 6: Automated BLEU/ROUGE evaluation

Compares LLM-generated summaries against human reference summaries using corpus-level BLEU and ROUGE metrics. Human references are read from `allData.json`.

**Prerequisites:**

```bash
cd /path/to/MedBench/DataPrep
poetry install   # installs sacrebleu, py-rouge, pandas
```

**Run:**

```bash
poetry run python evaluate_bleu_rouge.py \
    --llm-dir ../LLM/data/output \
    --alldata data/output/allData.json \
    --out results/bleu_rouge.csv
```

**Filters:**

```bash
# Only a specific specialty
poetry run python evaluate_bleu_rouge.py --specialty Medicine

# Only a specific model run (substring match on the generatedBy string)
poetry run python evaluate_bleu_rouge.py --model "gpt-5.2_2025-12-11@temp=0.0@basic"
```

**Output:** `results/bleu_rouge.csv` with columns:

| Column         | Description                           |
| -------------- | ------------------------------------- |
| `specialty`    | Medical specialty                     |
| `case_id`      | Case identifier (e.g. "Case 1")       |
| `language`     | Language (e.g. "original", "Swedish") |
| `generated_by` | Full model+approach identifier        |
| `bleu`         | Corpus BLEU score (0–100)             |
| `rouge1_f`     | ROUGE-1 F1                            |
| `rouge2_f`     | ROUGE-2 F1                            |
| `rougeL_f`     | ROUGE-L F1                            |

The script prints mean scores across all matched cases.

---

## Repository layout

```
DataPrep/
├── build_processed_output.R       # Main R pipeline (Step 2)
├── download_platform_charts.py    # Download from Platform (Step 1)
├── evaluate_bleu_rouge.py         # BLEU/ROUGE evaluation (Step 6)
├── helpers/                       # R helper functions (sourced by build_processed_output.R)
│   ├── getFileTibble.R            # Discover input files
│   ├── word2tibble.R              # Parse Word documents
│   ├── lab2tibble.R               # Parse lab values from Excel
│   ├── medications2tibble.R       # Parse medications from Excel
│   ├── readSummaries.R            # Parse existing summary files
│   ├── merge_and_format_data.R    # Combine all data
│   ├── convert2markdown_list.R    # Convert tables to markdown
│   ├── merge_into_single_markdown.R # Merge into one .md file per case
│   ├── extend_with_platform_JSON.R  # Merge with Platform data
│   └── find_invalid_case_data.R   # Validation
├── data/
│   ├── Cases/                     # Source Word/Excel files (by specialty)
│   ├── output/allData.json        # Platform export (generated by Step 1)
│   └── processed/                 # Generated by Step 2
│       ├── raw/
│       ├── markdown/
│       └── merged/
└── results/                       # BLEU/ROUGE output (generated by Step 6)
```

---

## Study objectives

- **Dataset creation**: Fictional EHRs across multiple specialties and languages
- **Benchmarking**: Assess LLM performance on discharge summary generation
- **Evaluation**: Quantitative (BLEU/ROUGE) and qualitative (human review campaigns) assessment
