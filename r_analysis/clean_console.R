# ---- clear_workspace.R ----
clear_workspace <- function(keep = character()) {
  ## Alle Objekte in .GlobalEnv holen (inkl. versteckte wie .Random.seed):
  alles <- ls(envir = .GlobalEnv, all.names = TRUE)
  
  ## Gewünschte Objekte davon ausnehmen:
  to_remove <- setdiff(alles, keep)
  
  ## Löschen:
  rm(list = to_remove, envir = .GlobalEnv)
  
  ## Speicher freigeben (optional):
  invisible(gc())
}




# Alles löschen
clear_workspace()

# Alles löschen, außer „model“ und „config“
#clear_workspace(keep = c("model", "config"))


