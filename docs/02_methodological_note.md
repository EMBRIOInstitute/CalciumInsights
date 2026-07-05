# Methodological Note

The paper discussed with Norma emphasizes that calcium event detection can be strongly affected by the definition of baseline fluorescence or F0. Traditional workflows often transform fluorescence into dF/F0 and then apply a threshold. However, F0 may be estimated from an initial segment, a low-variance segment, a smoothed moving baseline, or another rule.

For this reason, CalciumInsights now includes a baseline sensitivity analysis panel. This panel compares how transient metrics change across baseline definitions.

The wavelet ridgewalking-inspired module adds a complementary strategy. Instead of starting from a single F0 definition, it searches for event-like structures in a time-scale representation of the signal. Events that persist across multiple temporal scales are more likely to represent true calcium transients than isolated noise fluctuations.
