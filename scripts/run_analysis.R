# Run the three analyses from the repository root using the committed snapshots.
for (script in c("02_sector_leverage_analysis.R", "03_repo_vix_forecasting.R",
                 "04_five_bank_leverage_analysis.R")) {
  message("Running ", script)
  sys.source(file.path("scripts", script), envir = new.env())
}
