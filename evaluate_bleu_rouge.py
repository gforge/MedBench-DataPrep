"""Compute BLEU and ROUGE scores for LLM-generated summaries vs human references.

Reads human summaries from allData.json and LLM summaries from output .txt files,
matches them by (specialty, case_id, language), and outputs a CSV with scores.

Usage:
    cd /home/max/workspace/MedBench/DataPrep
    poetry run python evaluate_bleu_rouge.py \
        --llm-dir ../LLM/data/output \
        --alldata data/output/allData.json \
        --out results/bleu_rouge.csv

    # Filter to a single specialty or model
    poetry run python evaluate_bleu_rouge.py --specialty Medicine --model "gpt-5.2_2025-12-11"
"""

import argparse
import csv
import json
from pathlib import Path

import sacrebleu
from rouge import Rouge


def load_human_summaries(alldata_path: Path) -> dict[tuple[str, str, str], list[str]]:
    """Return {(specialty, case_id, language): [human_summary_text, ...]}."""
    with open(alldata_path, encoding="utf-8") as f:
        cases = json.load(f)
    index: dict[tuple[str, str, str], list[str]] = {}
    for case in cases:
        for chart in case.get("charts", []):
            key = (chart["specialty"], chart["name"], chart["language"])
            human_texts = [
                s["text"]
                for s in chart.get("summaries", [])
                if s.get("generatorType") == "HUMAN" and s.get("text", "").strip()
            ]
            if human_texts:
                index[key] = human_texts
    return index


def parse_summary_filename(path: Path) -> tuple[str, str, str] | None:
    """Parse Summary_4_{case_id}@{language}@{generatedBy}.txt → (case_id, language, generatedBy)."""
    name = path.stem
    if not name.startswith("Summary_4_"):
        return None
    rest = name[len("Summary_4_"):]
    parts = rest.split("@")
    if len(parts) < 3:
        return None
    case_id = parts[0]
    language = parts[1]
    generated_by = "@".join(parts[2:])
    return case_id, language, generated_by


def compute_rouge(hypothesis: str, references: list[str]) -> dict[str, float]:
    rouge = Rouge()
    scores_per_ref = []
    for ref in references:
        try:
            s = rouge.get_scores(hypothesis, ref)[0]
            scores_per_ref.append(s)
        except Exception:
            continue
    if not scores_per_ref:
        return {"rouge1_f": 0.0, "rouge2_f": 0.0, "rougeL_f": 0.0}
    return {
        "rouge1_f": max(s["rouge-1"]["f"] for s in scores_per_ref),
        "rouge2_f": max(s["rouge-2"]["f"] for s in scores_per_ref),
        "rougeL_f": max(s["rouge-l"]["f"] for s in scores_per_ref),
    }


def compute_bleu(hypothesis: str, references: list[str]) -> float:
    # corpus_bleu expects sys_stream=[hyp, ...] and ref_streams=[[ref1_for_hyp, ...], [ref2_for_hyp, ...]]
    ref_streams = [[ref] for ref in references]
    result = sacrebleu.corpus_bleu([hypothesis], ref_streams)
    return result.score


def collect_llm_summaries(llm_dir: Path, specialty_filter: str | None, model_filter: str | None) -> list[dict]:
    records = []
    for specialty_dir in sorted(llm_dir.iterdir()):
        if not specialty_dir.is_dir():
            continue
        specialty = specialty_dir.name
        if specialty_filter and specialty != specialty_filter:
            continue
        for txt_file in sorted(specialty_dir.glob("Summary_4_*.txt")):
            parsed = parse_summary_filename(txt_file)
            if not parsed:
                continue
            case_id, language, generated_by = parsed
            if model_filter and model_filter not in generated_by:
                continue
            text = txt_file.read_text(encoding="utf-8").strip()
            records.append({
                "specialty": specialty,
                "case_id": case_id,
                "language": language,
                "generated_by": generated_by,
                "text": text,
            })
    return records


def main() -> None:
    parser = argparse.ArgumentParser(description="Compute BLEU/ROUGE scores for LLM summaries")
    parser.add_argument("--llm-dir", default="../LLM/data/output", help="Directory with LLM output subdirs per specialty")
    parser.add_argument("--alldata", default="data/output/allData.json", help="allData.json from the Platform")
    parser.add_argument("--out", default="results/bleu_rouge.csv")
    parser.add_argument("--specialty", default=None, help="Filter to a single specialty")
    parser.add_argument("--model", default=None, help="Filter by model string (substring match on generatedBy)")
    args = parser.parse_args()

    alldata_path = Path(args.alldata)
    if not alldata_path.exists():
        raise FileNotFoundError(f"allData.json not found: {alldata_path}")
    llm_dir = Path(args.llm_dir)
    if not llm_dir.exists():
        raise FileNotFoundError(f"LLM output directory not found: {llm_dir}")

    print(f"Loading human summaries from {alldata_path}...")
    human_index = load_human_summaries(alldata_path)
    print(f"Found human summaries for {len(human_index)} chart translations.")

    print(f"Scanning LLM output in {llm_dir}...")
    llm_records = collect_llm_summaries(llm_dir, args.specialty, args.model)
    print(f"Found {len(llm_records)} LLM summaries.")

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    results = []
    skipped = 0
    for rec in llm_records:
        key = (rec["specialty"], rec["case_id"], rec["language"])
        refs = human_index.get(key)
        if not refs:
            skipped += 1
            continue

        bleu = compute_bleu(rec["text"], refs)
        rouge = compute_rouge(rec["text"], refs)
        results.append({
            "specialty": rec["specialty"],
            "case_id": rec["case_id"],
            "language": rec["language"],
            "generated_by": rec["generated_by"],
            "bleu": round(bleu, 4),
            "rouge1_f": round(rouge["rouge1_f"], 4),
            "rouge2_f": round(rouge["rouge2_f"], 4),
            "rougeL_f": round(rouge["rougeL_f"], 4),
        })

    if skipped:
        print(f"Skipped {skipped} LLM summaries with no matching human reference.")

    if not results:
        print("No results to write.")
        return

    fieldnames = ["specialty", "case_id", "language", "generated_by", "bleu", "rouge1_f", "rouge2_f", "rougeL_f"]
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(results)

    print(f"\nResults written to {out_path} ({len(results)} rows).")

    # Print summary stats
    if results:
        avg_bleu = sum(r["bleu"] for r in results) / len(results)
        avg_rouge1 = sum(r["rouge1_f"] for r in results) / len(results)
        avg_rouge2 = sum(r["rouge2_f"] for r in results) / len(results)
        avg_rougeL = sum(r["rougeL_f"] for r in results) / len(results)
        print(f"\nMean scores ({len(results)} summaries):")
        print(f"  BLEU:    {avg_bleu:.4f}")
        print(f"  ROUGE-1: {avg_rouge1:.4f}")
        print(f"  ROUGE-2: {avg_rouge2:.4f}")
        print(f"  ROUGE-L: {avg_rougeL:.4f}")


if __name__ == "__main__":
    main()
