# Portfolio cleanup

- Renamed the synthetic and real notebooks so their data sources are explicit.
- Moved historical data to `data/processed` and saved predictions to `results`.
- Removed two alternate notebook filenames whose code cells duplicated the
  retained notebooks. Original files remain available in Git history.
- Added portable notebook paths and the explicit `COD_PROV`/`COD_PRO` rename.
- Routed new notebook exports to separate `results/runs` directories.
- Moved R code out of `Nuova cartella` and replaced personal absolute paths with
  configurable data roots. Documented external inputs that are not bundled.
- Added project overview, contributors, setup, saved metrics, limitations,
  lightweight data diagnostics and smoke checks.
- Preserved original model definitions and saved experiment outputs. Full model
  training has not been repeated as part of the packaging work.
