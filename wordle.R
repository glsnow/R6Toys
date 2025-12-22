words <- scan('12Dicts/International/3of6game.txt', what='')
words <- sub("[^a-z]", "", words)

words5 <- grep("^[a-z]{5}$", words, value=TRUE)

wordle <- function(pattern, include='', exclude='') {
  mywords <- words5
  inc <- strsplit(include, "")[[1]]
  for(i in inc) {
    mywords <- mywords[grepl(i, mywords)]
  }
  exc <- strsplit(exclude, "")[[1]]
  for(i in exc) {
    mywords <- mywords[!grepl(i, mywords)]
  }
  grep(pattern, mywords, value=TRUE)
}

wordle(".[^a][^ia]i[^n]", include="ain", exclude="rsemout")

wordle(".la.e", include="k", exclude="rismountpchv")
