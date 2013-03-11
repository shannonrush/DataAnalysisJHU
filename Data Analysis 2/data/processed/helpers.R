showMissing <- function(data) {
  print(apply(data,2,function(x)sum(is.na(x))))
}
