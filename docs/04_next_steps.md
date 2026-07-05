# Next Steps

## Phase 1: Current release

- Keep the original FFT + findpeaks workflow.
- Add baseline sensitivity analysis.
- Add a wavelet ridgewalking-inspired module as a separate tab.
- Add methodological interpretation.

## Phase 2: Shared data architecture

Refactor the app so the uploaded dataset and selected ROI are controlled by a single data module. Both FFT and wavelet modules should receive the same trace as a reactive input.

## Phase 3: Direct comparison

Modify each analysis module so it returns metrics as a reactive object. Then the Method Comparison module can generate:

- Side-by-side metric tables.
- Agreement plots.
- Event-time overlap.
- Differences in number of events, frequency, duration, amplitude, and rise rate.
