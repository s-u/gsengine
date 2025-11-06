# Convert a Genstat "Analysis of variance" table (as parsed by rvest)
# into an object that prints like summary.aov / anova tables.
genstatAnovaToAnova <- function(tab) {
  # tab is a data.frame like e$gs_tables_last[[1]]
  
  # ensure character + trim; turn "" into NA
  tab[] <- lapply(tab, function(x) trimws(as.character(x)))
  tab[tab == ""] <- NA
  
  # first row are the headers
  headers <- as.character(tab[1, , drop = TRUE])
  colnames(tab) <- headers
  tab <- tab[-1, , drop = FALSE]
  
  # drop rows that are entirely NA
  tab <- tab[rowSums(!is.na(tab)) > 0, , drop = FALSE]
  
  # drop obvious separator/section rows (e.g., "Rep.*Units* stratum" spanning line)
  firstColName <- headers[1]
  if (!is.na(firstColName) && firstColName %in% colnames(tab)) {
    sepIdx <- grepl("^Rep\\.\\*Units\\* stratum$", tab[[firstColName]]) |
      is.na(tab[[firstColName]])
    tab <- tab[!sepIdx, , drop = FALSE]
  }
  
  # Map Genstat headers -> standard anova headers
  # (use only the columns that exist in this table)
  nameMap <- c(
    "Source of variation" = "Source",
    "d.f."                = "Df",
    "s.s."                = "Sum Sq",
    "m.s."                = "Mean Sq",
    "v.r."                = "F value",
    "F pr."               = "Pr(>F)"
  )
  present <- intersect(names(nameMap), colnames(tab))
  tab <- tab[, present, drop = FALSE]
  colnames(tab) <- unname(nameMap[present])
  
  # row names come from "Source" (formerly "Source of variation")
  rowNames <- if ("Source" %in% colnames(tab)) tab[["Source"]] else NULL
  if (!is.null(rowNames)) {
    rownames(tab) <- rowNames
    tab[["Source"]] <- NULL
  }
  
  # coerce numeric columns
  numCols <- setdiff(colnames(tab), character(0))
  tab[numCols] <- lapply(tab[numCols], function(x) {
    suppressWarnings(as.numeric(gsub(",", "", x)))
  })
  
  # set class so print() uses print.anova
  class(tab) <- c("anova", "data.frame")
  attr(tab, "heading") <- "Analysis of Variance Table"
  
  tab
}
