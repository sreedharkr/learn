#require(lsr)
#require(bootES)
#require(corrgram)

require(foreign)
require(MASS)
library(lsa)

td = tempfile()
dir.create(td, showWarnings = TRUE)
write( c("man", "cat", "donkey"), file=paste(td, "D1", sep="/"))
write( c("hamster", "donkey", "sushi"), file=paste(td, "D2", sep="/"))
write( c("man", "monster", "monster"), file=paste(td, "D3", sep="/"))
write( c("man", "donkey", "man"), file=paste(td, "D4", sep="/"))
myMatrix = textmatrix(td, minWordLength=1)
myMatrix
myLSAspace = lsa(myMatrix, dims=dimcalc_share())
myLSAspace
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix
associate(myNewMatrix, "donkey")
unlink(td, recursive=TRUE)
