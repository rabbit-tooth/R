#Author by 2017/9/1   R --vanilla --slave --args chr < LPmerge_concensusmap.R
library("LPmerge")
args <- commandArgs()
chr <- args[5]

getMap <- function(chromosome,population){
  inp <- paste0(chromosome,population)
  map <- read.delim(inp, header=T, sep=",", comment.char="", fill=T, stringsAsFactors=F)#no return
}

mapI <- getMap(chr,"_ak.csv")
mapII <- getMap(chr,"_xia.csv")
mapIII <- getMap(chr,"_jing.csv")
mapIV <- getMap(chr,"_zhang.csv")
maps <- list(I=mapI,II=mapII,III=mapIII,IV=mapIV)
ans <- LPmerge(maps)

ans <- as.data.frame(ans)
ans <- data.frame(marker=ans[,"marker.1"],position=ans[,"consensus.1"],ak=ans[,"I.1"],xia=ans[,"II.1"],jing=ans[,"III.1"],zhang=ans[,"IV.1"])
write.csv(ans, file=paste0(chr,"_c.csv"), row.names=F, quote=F)
ans <- ans[,c(1,2)]
write.csv(ans, file=paste0(chr,".csv"), row.names=F, quote=F)