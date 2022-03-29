# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()



# checks before running the app -------------------------------------------

#> in fct_add_intro_method_notes, set year and note number





# Run the application
run_app()

# Profile the app if needed

profvis::profvis(print(OPHIDataBankGlobalComparison::run_app()))

