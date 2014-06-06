# Parse the PSI-MOD.obo into a list of lists
oboFile <- 'inst/PSI-MOD.obo'

obo <- readLines(oboFile)
obo <- obo[obo != '']
firstTerm <- which(obo == "[Term]")[1]
oboHeader <- obo[1:(firstTerm-1)]
oboTerms <- obo[firstTerm:length(obo)]
oboTerms <- sub('^xref: ', '(xref) ', oboTerms)
oboTerms <- gsub('\"', '', oboTerms)
oboTerms <- strsplit(oboTerms, split=': ', fixed=TRUE)
oboTerms <- lapply(oboTerms, function(x) {c(x[1], if(!is.na(x[2])) paste(x[-1], collapse=': ') else NULL)})
counter <- 0
currentTerm <- list()
termList <- list()
for(i in 1:length(oboTerms)) {
    if(oboTerms[[i]][1] == '[Term]') {
        if (counter != 0) termList[[counter]] <- currentTerm
        currentTerm <- list()
        counter <- counter + 1
    } else if(oboTerms[[i]][1] == '[Typedef]') {
        termList[[counter]] <- currentTerm
        break
    } else {
        currentTerm[[oboTerms[[i]][1]]] <- c(currentTerm[[oboTerms[[i]][1]]], oboTerms[[i]][2])
    }
}


mod_id <- data.frame('_id'=1:length(termList), mod_id=as.character(NA), name=as.character(NA), definition=as.character(NA), check.names=FALSE, stringsAsFactors=FALSE)

nSynonym <- length(unlist(sapply(termList, function(x) {x$synonym})))
mod_synonym <- data.frame('_id'=rep(as.integer(NA), nSynonym), synonym=as.character(NA), check.names=FALSE, stringsAsFactors=FALSE)
nObsolete <- length(unlist(sapply(termList, function(x) {x$is_obsolete})))
mod_obsolete <- data.frame('_id'=rep(as.integer(NA), nObsolete), check.names=FALSE, stringsAsFactors=FALSE)
nComment <- length(unlist(sapply(termList, function(x) {x$comment})))
mod_comment <- data.frame('_id'=rep(as.integer(NA), nComment), comment=as.character(NA), check.names=FALSE, stringsAsFactors=FALSE)
nSubset <- length(unlist(sapply(termList, function(x) {x$subset})))
mod_subset <- data.frame('_id'=rep(as.integer(NA), nSubset), '_subset_id'=as.integer(NA), check.names=FALSE, stringsAsFactors=FALSE)
mod_subset_def <- data.frame('_subset_id'= 1:length(unique(unlist(sapply(termList, function(x) {x$subset})))), 'subset'=unique(unlist(sapply(termList, function(x) {x$subset}))), check.names=FALSE, stringsAsFactors=FALSE)
nXref <- sum(unlist(sapply(termList, function(x) {any(grepl('(xref)', names(x)))})))
mod_info <- data.frame('_id'=rep(as.integer(NA), nXref), 'diffAvg'=as.numeric(NA), 'diffFormula'=as.character(NA), 'diffMono'=as.numeric(NA), 'formula'=as.character(NA), 'massAvg'=as.numeric(NA), 'massMono'=as.numeric(NA), 'origin'=as.character(NA), 'source'=as.character(NA), 'termSpec'=as.character(NA), check.names=FALSE, stringsAsFactors=FALSE)

for(i in mod_id$`_id`) {
    currentTerm <- termList[[i]]
    
    mod_id$mod_id[i] <- currentTerm$id
    mod_id$name[i] <- currentTerm$name
    mod_id$definition[i] <- currentTerm$def
    
    if(!is.null(currentTerm$synonym)) {
        nextLine <- which(is.na(mod_synonym$`_id`))[1]
        index <- seq(nextLine, length.out=length(currentTerm$synonym))
        mod_synonym$`_id`[index] <- i
        mod_synonym$synonym[index] <- currentTerm$synonym
    }
    
    if(!is.null(currentTerm$is_obsolete)) {
        nextLine <- which(is.na(mod_obsolete$`_id`))[1]
        mod_obsolete$`_id`[nextLine] <- i
    }
    
    if(!is.null(currentTerm$comment)) {
        nextLine <- which(is.na(mod_comment$`_id`))[1]
        index <- seq(nextLine, length.out=length(currentTerm$comment))
        mod_comment$`_id`[index] <- i
        mod_comment$comment[index] <- currentTerm$comment
    }
    
    if(!is.null(currentTerm$subset)) {
        nextLine <- which(is.na(mod_subset$`_id`))[1]
        index <- seq(nextLine, length.out=length(currentTerm$subset))
        mod_subset$`_id`[index] <- i
        mod_subset$`_subset_id`[index] <- mod_subset_def$`_subset_id`[match(currentTerm$subset, mod_subset_def$subset)]
    }
    
    if(any(grepl('(xref)', names(currentTerm)))) {
        nextLine <- which(is.na(mod_info$`_id`))[1]
        mod_info$`_id`[nextLine] <- i
        suppressWarnings({
            if(!is.null(currentTerm$`(xref) DiffAvg`)) mod_info$diffAvg[nextLine] <- as.numeric(currentTerm$`(xref) DiffAvg`)
            if(!is.null(currentTerm$`(xref) DiffFormula`)) mod_info$diffFormula[nextLine] <- currentTerm$`(xref) DiffFormula`
            if(!is.null(currentTerm$`(xref) DiffMono`)) mod_info$diffMono[nextLine] <- as.numeric(currentTerm$`(xref) DiffMono`)
            if(!is.null(currentTerm$`(xref) Formula`)) mod_info$formula[nextLine] <- currentTerm$`(xref) Formula`
            if(!is.null(currentTerm$`(xref) MassAvg`)) mod_info$massAvg[nextLine] <- as.numeric(currentTerm$`(xref) MassAvg`)
            if(!is.null(currentTerm$`(xref) MassMono`)) mod_info$massMono[nextLine] <- as.numeric(currentTerm$`(xref) MassMono`)
            if(!is.null(currentTerm$`(xref) Origin`)) mod_info$origin[nextLine] <- currentTerm$`(xref) Origin`
            if(!is.null(currentTerm$`(xref) Source`)) mod_info$`source`[nextLine] <- currentTerm$`(xref) Source`
            if(!is.null(currentTerm$`(xref) TermSpec`)) mod_info$termSpec[nextLine] <- currentTerm$`(xref) TermSpec`
        })
    }
}

nChildren <- length(unlist(sapply(termList, function(x) {x$is_a})))
mod_children <- data.frame('_id'=rep(as.integer(NA), nChildren), '_child_id'=as.integer(NA), check.names=FALSE, stringsAsFactors=FALSE)

for(i in mod_id$`_id`) {
    if(!is.null(termList[[i]]$is_a)) {
        parents <- sub(' .*$', '',termList[[i]]$is_a)
        nextLine <- which(is.na(mod_children$`_child_id`))[1]
        index <- seq(nextLine, length.out=length(parents))
        mod_children$`_id`[index] <- mod_id$`_id`[match(parents, mod_id$mod_id)]
        mod_children$`_child_id`[index] <- i
    }
}

nRelations <- length(unlist(sapply(termList, function(x) {x$relationship})))
mod_relationships <- data.frame('_id'=rep(as.integer(NA), nRelations), '_related_id'=as.integer(NA), 'relationType'=as.character(NA), check.names=FALSE, stringsAsFactors=FALSE)

for(i in mod_id$`_id`) {
    if(!is.null(termList[[i]]$relationship)) {
        relations <- sub(' ! .*$', '',termList[[i]]$relationship)
        relations <- sub('^.* ', '', relations)
        relationType <- sub(' .*$', '',termList[[i]]$relationship)
        nextLine <- which(is.na(mod_relationships$`_id`))[1]
        index <- seq(nextLine, length.out=length(relations))
        mod_relationships$`_id`[index] <- i
        mod_relationships$`_related_id`[index] <- mod_id$`_id`[match(relations, mod_id$mod_id)]
        mod_relationships$relationType[index] <- relationType
    }
}

### Creating the SQLite database

dbFile <- '/Users/Thomas/Dropbox/GitHub/psimod.db/inst/extdata/psimod.sqlite'
file.remove(dbFile)
con <- dbConnect(dbDriver('SQLite'), dbname=dbFile)

#### Add main table
dbGetQuery(con, 'CREATE Table mod_id (_id INTEGER, mod_id TEXT, name TEXT, definition TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_id VALUES ($_id, $mod_id, $name, $definition)', bind.data=mod_id)
dbCommit(con)

#### Add synonyms
dbGetQuery(con, 'CREATE Table mod_synonym (_id INTEGER, synonym TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_synonym VALUES ($_id, $synonym)', bind.data=mod_synonym)
dbCommit(con)

#### Add obsolete
dbGetQuery(con, 'CREATE Table mod_obsolete (_id INTEGER)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_obsolete VALUES ($_id)', bind.data=mod_obsolete)
dbCommit(con)

#### Add comment
dbGetQuery(con, 'CREATE Table mod_comment (_id INTEGER, comment TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_comment VALUES ($_id, $comment)', bind.data=mod_comment)
dbCommit(con)

#### Add subset
dbGetQuery(con, 'CREATE Table mod_subset (_id INTEGER, _subset_id INTEGER)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_subset VALUES ($_id, $_subset_id)', bind.data=mod_subset)
dbCommit(con)

#### Add subset defs
dbGetQuery(con, 'CREATE Table mod_subset_def (_subset_id INTEGER, subset TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_subset_def VALUES ($_subset_id, $subset)', bind.data=mod_subset_def)
dbCommit(con)

#### Add info
dbGetQuery(con, 'CREATE Table mod_info (_id INTEGER, diffAvg REAL, diffFormula TEXT, diffMono REAL, formula TEXT, massAvg REAL, massMono REAL, origin TEXT, source TEXT, termSpec TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_info VALUES ($_id, $diffAvg, $diffFormula, $diffMono, $formula, $massAvg, $massMono, $origin, $source, $termSpec)', bind.data=mod_info)
dbCommit(con)

#### Add children
dbGetQuery(con, 'CREATE Table mod_children (_id INTEGER, _child_id INTEGER)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_children VALUES ($_id, $_child_id)', bind.data=mod_children)
dbCommit(con)

#### Add subset
dbGetQuery(con, 'CREATE Table mod_relationships (_id INTEGER, _related_id INTEGER, relationType TEXT)')
dbBeginTransaction(con)
dbGetPreparedQuery(con, 'INSERT INTO mod_relationships VALUES ($_id, $_related_id, $relationType)', bind.data=mod_relationships)
dbCommit(con)
