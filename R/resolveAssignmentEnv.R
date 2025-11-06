resolveAssignmentEnv <- function() {
  if (isTRUE(getOption("knitr.in.progress"))) {
    return(knitr::knit_global())
  } else {
    return(globalenv())
  }
}
