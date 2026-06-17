# TNSRE Review Source Package

This anonymized package contains the manuscript sources, compiled PDFs, figures, source-data summaries, and verification files needed for peer review of:

**Auditing Cross-Dataset Claims in Wearable EMG Representation Learning: A Public-Data Case Study**

## Included Files

- `main.tex` and `main.pdf` -- anonymized main manuscript source and compiled PDF
- `supplement.tex` and `supplement.pdf` -- anonymized supplementary material source and compiled PDF
- `refs.bib` -- bibliography
- `figures_compiled/` -- vector manuscript figure assets
- `scripts/build_submission_figures.R` -- R/ggplot2/ggprism figure-generation script
- `source_data/` -- manuscript-ready source tables and diagnostics
- `release_manifest.json` -- review package metadata and audit boundaries
- `REPRODUCIBILITY_LEDGER.md` -- package-level review ledger
- `PACKAGE_FILE_MANIFEST.json` and `SHA256SUMS.txt` -- file inventory and checksums

The author-facing cover letter and public repository/archive identifiers are intentionally excluded from this review copy.

## Compilation

Compiled review PDFs are included:

- `main.pdf` -- main manuscript
- `supplement.pdf` -- supplementary material

If any `.tex`, figure, source-data, README, manifest, or checksum text is edited, recompile `main.tex` and `supplement.tex`, then regenerate `SHA256SUMS.txt`.

## Verification

From this directory, verify staged files on POSIX/GNU systems with:

```bash
sha256sum -c SHA256SUMS.txt
```

## Review Boundary

This package is limited to peer-review materials. It includes manuscript-ready summaries and figure-generation inputs, but it does not redistribute raw third-party EMG data, model checkpoints, embedding caches, raw-window feature parquet files, prediction caches, local launcher logs, machine-local paths, public repository identifiers, or public archive identifiers.

## Claim Boundary

The paper is framed as a reproducible claim-control and audit framework for cross-dataset EMG representation learning. It does not claim a deployable EMG controller, domain-invariant physiology, reduced recalibration burden, prosthetic-control transfer, or clinical readiness. Stronger claims require calibrated domain-recoverability diagnostics, ontology-valid transfer tasks, and downstream task-valid superiority over raw/classical baselines.
