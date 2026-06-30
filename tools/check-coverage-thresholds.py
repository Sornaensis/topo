#!/usr/bin/env python3
"""Enforce release coverage thresholds from Stack/HPC report output.

The script intentionally parses the plain text emitted by ``stack hpc report``
so CI can enforce a minimum coverage gate without depending on extra Python
packages or committing generated HPC artifacts.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


# Conservative 1.0 release gate. These numbers are deliberately lower than the
# current suite baseline so the first enforced gate prevents accidental drops;
# raise them in later hardening work as coverage is ratcheted.
THRESHOLDS: dict[str, float] = {
    "expressions": 25.0,
    "top-level declarations": 15.0,
    "alternatives": 20.0,
}

COVERAGE_LINE = re.compile(r"^\s*(\d+(?:\.\d+)?)%\s+(.+?)\s+\((\d+)\/(\d+)\)")


def normalize_metric(raw: str) -> str:
    metric = raw.strip().lower()
    for suffix in (" used", " coverage"):
        if metric.endswith(suffix):
            return metric[: -len(suffix)].strip()
    return metric


def parse_report(report_text: str) -> dict[str, list[float]]:
    metrics: dict[str, list[float]] = {}
    for line in report_text.splitlines():
        match = COVERAGE_LINE.match(line)
        if match is None:
            continue
        percent_text, raw_metric, _covered, _total = match.groups()
        metric = normalize_metric(raw_metric)
        if metric not in THRESHOLDS:
            continue
        metrics.setdefault(metric, []).append(float(percent_text))
    return metrics


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: check-coverage-thresholds.py <stack-hpc-report-log>", file=sys.stderr)
        return 2

    report_path = Path(argv[1])
    try:
        report_text = report_path.read_text(encoding="utf-8")
    except FileNotFoundError:
        print(f"coverage threshold check failed: report not found: {report_path}", file=sys.stderr)
        return 1

    parsed = parse_report(report_text)
    missing = sorted(metric for metric in THRESHOLDS if metric not in parsed)
    failures: list[str] = []

    if missing:
        failures.append("missing metrics: " + ", ".join(missing))

    for metric, required in sorted(THRESHOLDS.items()):
        values = parsed.get(metric, [])
        if not values:
            continue
        observed = min(values)
        if observed < required:
            failures.append(f"{metric}: {observed:.1f}% < required {required:.1f}%")

    if failures:
        print("coverage threshold check failed:", file=sys.stderr)
        for failure in failures:
            print(f"- {failure}", file=sys.stderr)
        return 1

    summary = ", ".join(
        f"{metric} {min(parsed[metric]):.1f}% >= {required:.1f}%"
        for metric, required in sorted(THRESHOLDS.items())
    )
    print(f"Coverage thresholds OK: {summary}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
