getData <- function() {
  download.file("https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", method="curl", destfile="samsungData.rda")
  d<- date()
  save(d,file="dateDownloaded.rda")
}