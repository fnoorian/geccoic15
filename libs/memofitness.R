# A function for caching fitness function results
# very similar to what was implemented in the diskmemoiser package

library("digest")

memo_fitness_function = function(fitfunc, memo.dir = "./memo") {
# expects a fitness function of form:
  # fitfunc(expression, ....)
  
  if (!file.exists(memo.dir)) {
    dir.create(memo.dir, showWarnings = FALSE)
  }
  
  func.name = as.character(substitute(fitfunc))
  
  memo_fitfunc <- function(expr, ...) {
    hash1 <- tolower(digest(as.character(expr)))
    hash2 <- tolower(digest(list(...)))
    
    file.name = file.path(memo.dir, paste0("FF_", func.name, "_", hash2, hash1, "-cache.RData"))
    
    if (file.exists(file.name)) {
      load(file.name)
    } else {
      result = fitfunc(expr, ...)
      save(result, file=file.name)
    }
    return (result)
  }
  
  return(memo_fitfunc)
}
