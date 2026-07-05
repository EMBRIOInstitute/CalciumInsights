# Wavelet Ridgewalking Module Specification

## Goal

Detect calcium events with less direct dependence on the baseline definition.

## Workflow

1. Select ROI trace.
2. Standardize signal.
3. Compute a continuous wavelet transform-like time-scale map using the Ricker/Mexican hat wavelet over a user-defined range of scales.
4. Use the absolute wavelet coefficients to generate the CWT amplitude map.
5. Detect local maxima in the CWT amplitude map.
6. Connect local maxima across neighboring scales to form ridge-like structures.
7. Keep ridges longer than a user-defined minimum ridge length.
8. Merge events that occur too close in time.
9. Report event table and summary metrics.

## Wavelet used in the current code

The current implementation uses a real Ricker/Mexican hat wavelet approximation, not a Morlet wavelet. The wavelet is defined as:

```text
psi(u) = (1 - u^2) * exp(-u^2 / 2)
```

For each scale `s` and time point `tau`, the app computes an approximate coefficient using the standardized signal, the scaled wavelet `psi((time - tau) / s) / sqrt(s)`, and a time-step estimate based on the median difference between consecutive time values. The CWT amplitude map is calculated as the absolute value of these coefficients.

## Main parameters

- Minimum scale
- Maximum scale
- Number of scales
- CWT amplitude quantile threshold
- Ridge connection tolerance
- Minimum ridge length
- Minimum event distance
- Metric window around event

## Output metrics

- Number of wavelet events
- Frequency
- Mean amplitude above baseline
- Mean peak time
- Mean wavelet event time
- Mean ridge length
- Mean CWT amplitude
- Mean scale
