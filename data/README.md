# Data provenance and coverage

## Included historical data

`processed/pop1982-2024_adj.csv` is the municipal population table distributed
with the original coursework repository. The project presentation identifies
ISTAT as the source of the annual population inputs. The repository does not
record the exact extraction URLs, download dates or source release identifiers;
this file should be described as a project-prepared table, not a fresh official
ISTAT release.

Columns: `COD_COM`, `COD_PROV`, `COD_REG`, `ANNO`, `POPOLAZIONE`.
The notebook explicitly renames `COD_PROV` to `COD_PRO` for compatibility with
the original model code.

Municipal identifiers can change over time. Missing identifiers and duplicate
municipality/year pairs are reported by `scripts/preview_results.py`; they have
not been silently repaired. Changes to these records require checking the source
and would change the modelling experiment.

## Included forecasts

`../results/df_predicted_agaci.csv` is an original saved export for 2025-2040.
Columns include geographic identifiers, `POP_PRED`, `POP_LO`, `POP_HI` and
`ALPHA_USED`. The filename does not claim regional reconciliation. This export
is not the held-out evaluation table used for the headline comparison.

## Additional inputs

The R accessibility analysis also expects hospital accessibility inputs,
municipal/provincial boundaries and historical population source extracts.
See `../scripts/r/README.md`. These are not downloaded automatically.

No new redistribution licence is asserted for third-party data. Consult the
original source terms before redistributing data in another context.
