db <- installed.packages()
cat('htmlwidgets Depends:', db['htmlwidgets', 'Depends'], '\n')
cat('htmlwidgets Imports:', db['htmlwidgets', 'Imports'], '\n')
cat('htmlwidgets Suggests:', db['htmlwidgets', 'Suggests'], '\n')

cat("\nDT Imports:", db['DT', 'Imports'], '\n')
cat("DT Suggests:", db['DT', 'Suggests'], '\n')

cat("\nrhandsontable Imports:", db['rhandsontable', 'Imports'], '\n')
cat("rhandsontable Suggests:", db['rhandsontable', 'Suggests'], '\n')
