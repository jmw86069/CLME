#' Decide an appropriate number of cpu cores to use for parallel processing
#'
#' @description Uses methods in \code{doParallel} to determine the available
#' cpu cores, then by default utilizes only a fraction of those available.
#' This function is currently called by \code{summary.clme} to enable
#' parallel bootstrap processing.
#'
#' @param useFraction numeric fraction of detected cpu cores to use,
#'        rounding down to the nearest integer.
#' @param useCores numeric If NULL, then useFraction is used. If supplied,
#'        this exact number of cores is defined in registerDoParallel().
#' @param verbose logical indicates whether to print the number of cores as
#'        verbose output.
#'
#' @return
#' The output of \code{decide_clme_cores} is invisible, the number of cores
#' defined in registerDoParallel().
#'
#' @examples
#'   decide_clme_cores(useFraction=0.8)
#'
decide_clme_cores <- function
(useFraction=0.8,
 useCores=NULL,
 verbose=TRUE,
 ...)
{
   ## Purpose is simply to initialize doParallel
   ## and decide an appropriate number of cores
   ## to use
   registerDoParallel();
   if (is.null(useCores)) {
      numCores <- getDoParWorkers();
      useCores <- floor(numCores * useFraction);
   }
   if (head(verbose, 1)) {
      print(paste0("Initialized doParallel with ", useCores, " cores."));
   }
   registerDoParallel(cores=useCores);
   invisible(useCores);
}
