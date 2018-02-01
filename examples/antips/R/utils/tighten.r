
### return a data frame where all factors (ordered) 
### have been refactored to take out redundant levels

"tighten" <- function(data) {

  vars <- seq(length = length(data))

  for(j in vars) {
    x <- data[[j]]
    if(is.factor(x)) {
      if(any(class(x) == "ordered"))
        data[[j]] <- ordered(x)
      else data[[j]] <- factor(x)
    }
    
  }
  data
}
