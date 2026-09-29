# manifest.json must stay in step with renv.lock
#
# Connect Cloud builds from manifest.json, not renv.lock, so a manifest left
# behind after a package change would deploy the wrong versions. Regenerate it
# with:  Rscript -e "rsconnect::writeManifest()"

check("manifest.json exists", file.exists("manifest.json"))

if (file.exists("manifest.json")) {
  mf <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  lk <- jsonlite::fromJSON("renv.lock",     simplifyVector = FALSE)

  man <- vapply(mf$packages, function(p) p$description$Version, character(1))
  lock <- vapply(lk$Packages, function(p) p$Version, character(1))

  check("manifest records the same R version as renv.lock",
        identical(mf$platform, lk$R$Version),
        sprintf("manifest %s vs lockfile %s", mf$platform, lk$R$Version))

  check("manifest covers exactly the lockfile packages",
        setequal(names(man), names(lock)),
        paste("only in manifest:", paste(setdiff(names(man), names(lock)), collapse = ", "),
              "| only in lockfile:", paste(setdiff(names(lock), names(man)), collapse = ", ")))

  shared <- intersect(names(man), names(lock))
  drift  <- shared[man[shared] != lock[shared]]
  check("manifest package versions match renv.lock",
        length(drift) == 0,
        paste("drifted:", paste(drift, collapse = ", ")))

  check("the app entrypoint is bundled", "app.R" %in% names(mf$files))
}
