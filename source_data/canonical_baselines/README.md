# Canonical Baseline Source Tables

This folder stages manuscript-facing source tables for the downstream baseline checks discussed in the main paper and supplement. The tables are derived summaries from the frozen post-audit downstream screens; raw third-party EMG files, raw-window feature caches, model checkpoints, trial-level prediction caches, and local run logs are intentionally excluded.

Included checks:

- `same_subject_frequency_*`: same-subject cross-session frequency-domain comparator using per-channel log-bandpower-style spectral features, raw statistics, pretrained embeddings, and combined feature sets.
- `heldout_subject_classical_signal_*`: held-out-subject few-shot comparator using raw statistics plus classical signal features versus raw statistics, and raw-plus-classical-plus-pretrained versus raw-plus-classical. The compact `heldout_subject_classical_signal_positive_control.csv` records the raw-plus-classical positive-control row cited in the main manuscript.

Claim boundary:

- These are offline ridge-readout sensitivity checks, not online control tests.
- They are used as claim-inclusion gates. They show that the downstream screen can detect useful signal from conventional features, but the frozen pretrained representation did not pass the prespecified positive-claim gate.
- They are not EMGBench leaderboard submissions and do not establish clinical, prosthetic-control, or reduced-recalibration readiness.
