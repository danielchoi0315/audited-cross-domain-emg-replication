# Reproducibility Ledger

Package: `TNSRE_Audit_Gated_EMG_TNSRE_FinalAligned_20260616_ReviewReady`

## Package Status

| Item | Status | Notes |
|---|---|---|
| Main manuscript source | Present | `main.tex` uses IEEEtran and anonymized author metadata. |
| Supplement source | Present | `supplement.tex` contains moved ledgers and auxiliary diagnostic detail. |
| Compiled manuscript PDFs | Present after review rebuild | `main.pdf` and `supplement.pdf` are regenerated from the anonymized review sources before packaging. |
| Review source data | Present | `source_data/` contains manuscript-ready CSV/JSON summaries needed to trace figure and table values. |
| Checksum coverage | Present | `SHA256SUMS.txt` is regenerated after source, figure, and PDF edits. |

## Source-Data Coverage

The `source_data/` directory contains manuscript-ready summaries for:

- alignment pair sensitivity,
- downstream absolute accuracy curves,
- audit checks,
- canonical baseline and downstream screens,
- canonical release tables,
- fixed-probe domain-recoverability extension,
- post-freeze confirmatory audit summaries,
- ontology controls, and
- six-packaged-domain fixed-probe diagnostics.

Raw third-party EMG data, model checkpoints, embedding caches, prediction caches, local launcher logs, machine-local paths, public repository identifiers, and public archive identifiers are not included in this review package. Users must obtain original datasets under the providers' terms.

## Verification Command

```bash
sha256sum -c SHA256SUMS.txt
```

Any source, figure, table, PDF, or archive-reference change requires checksum regeneration.
