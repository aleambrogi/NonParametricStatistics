# Saved results and limitations

## Source

The following values are transcribed from the saved outputs of
`notebooks/02_real_population_forecasting.ipynb`, originally committed under
`REAL TEST POPULATION/Population_prediction_real.ipynb` at revision `e12666f`.
The portfolio cleanup has not rerun full training or independently reproduced
these metrics. The notebook is configured with training years 1982-2013,
validation years 2014-2024 and `ALPHA = 0.10`.

| Method | MAE | RMSE | Empirical coverage | Average interval width |
| --- | ---: | ---: | ---: | ---: |
| LSTM-based method (original label SOTA) | 256.15 | 1297.39 | 53.41% | 350.24 |
| Naive baseline | 322.54 | 3793.47 | 40.11% | 412.87 |
| GAM | 291.86 | 1548.73 | 39.02% | 413.95 |

MAE, RMSE and interval width are on the population-count scale. These are the
values in the notebook, which is the source used here rather than presentation
slides. The GAM values come from its final saved dictionary; the displayed final
summary table contains only the LSTM-based and naive rows.

## Interpretation

In this saved comparison, the LSTM-based method improves point-prediction error
relative to the naive baseline and GAM. This does not establish universal
superiority. The original SOTA label is retained only to identify its source row.

Empirical interval coverage is materially below the configured nominal 90%.
The outputs therefore do not demonstrate calibrated 90% uncertainty intervals.
The separate adaptive-validation diagnostic has a different coverage value and
must not be substituted for this comparison without explaining its procedure.

## Limits to address in further research

- Review coverage by horizon and geographic group, including regime changes.
- Use an additional untouched test period before making generalisation claims.
- Check missing geographic identifiers, municipal boundary changes and duplicate
  municipality/year records against the source data.
- Trace each export to the exact notebook settings and preprocessing version.
- Evaluate assumptions about fixed healthcare infrastructure before interpreting
  accessibility projections as operational guidance.

## Validation of this portfolio package

Run `python scripts/preview_results.py` for data diagnostics and figures, and
`python -m unittest discover -s tests -v` for the automated checks. The preview
reads saved CSVs without retraining. The model smoke check performs a small
synthetic forward/backward pass; it does not reproduce the saved comparison.
