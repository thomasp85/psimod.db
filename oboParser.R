# Parse the PSI-MOD.obo into a list of lists
oboFile <- 'inst/PSI-MOD.obo'

obo <- readLines(oboFile)
obo <- obo[obo != '']
firstTerm <- which(obo == "[Term]")[1]
oboHeader <- obo[1:(firstTerm-1)]
oboTerms <- obo[firstTerm:length(obo)]
oboTerms <- sub('^xref: ', '', oboTerms)
