getData <- function() {
  download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda",destfile="loansData.rda",method="curl")
  download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf",destfile="loansCodebook.pdf",method="curl")
  d <- date()
  save(d,file="dateDownloaded.rda")
}