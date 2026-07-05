# CalciumInsights App Architecture

Recommended design: one app, multiple modules.

## Modules

- `mod_fft_baseline_sensitivity.R`: original FFT + findpeaks workflow with baseline sensitivity analysis.
- `mod_wavelet_ridgewalking.R`: wavelet ridgewalking-inspired event detection.
- `mod_method_comparison.R`: interpretation and future side-by-side comparison.

## Why this architecture?

Keeping both workflows in one app allows the user to analyze the same type of calcium trace with different event-detection assumptions. This is scientifically stronger than creating two independent apps, because the methodological question is whether results are robust to the analysis pipeline.
