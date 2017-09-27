#Author by 2017/9/26   R --vanilla --slave --args limy < Sankey.R
dfI <- read.delim("1d.csv", header=T, sep=",", comment.char="", fill=T, stringsAsFactors=F)
dfII <- read.delim("P.csv", header=F, sep=",", comment.char="", fill=T)
chr <- "chr1D"

getCommonSNPDist <- function(dI,dII,Num,chromosome){
  name <- dI[Num,1]
  distI <- dI[Num,2]
  
  subII <- dII[dII$V1==name,]
  if(nrow(subII)!=0){
    if(subII[1,2]==chr){
      distII <- subII[1,3]/1000000
      position <- paste(distI,"_",distII)
    }else{
      position <- "Not common chr"
    }
  }else{
    position <- "Not common SNP"
  }
}

pdf(file=paste0(chr,".pdf"),width=5,height=5,bg="white")
par(mar=c(2,4,2,4))
x <- c(0,30)
limy <- as.numeric(commandArgs()[5])
flag <- 0

for(i in 1:nrow(dfI)){
  cat("Processing SNP ",i,"\n")
  y <- getCommonSNPDist(dfI,dfII,i,chr)
  if(y=="Not common SNP"){
    next
  }
  if(y=="Not common chr"){
    next
  }
  y <- unlist(strsplit(y,"_"))
  y <- c(y[1],y[2])             #use string as float
  
  if(flag==0){
    pic <- plot(x,y,main=chr,xlim=c(0,30),ylim=c(0,limy),xlab="",ylab="AK58 (cM)",
                type="l",lwd=0.2,axes=F,xaxs="i",col="green",col.lab="grey")
    flag <- 1
    next
  }
  
  par(new=TRUE)
  plot(x,y,xlim=c(0,30),ylim=c(0,limy),ylab="",type="l",lwd=0.2,
       axes=F,xaxs="i",col="green")
  axis(side=2,col="grey",col.ticks="grey",col.axis="grey",cex.axis=0.8)
  axis(4,col="grey",col.ticks="grey",col.axis="grey",cex.axis=0.8)
  mtext(4,text="CS (Mb)",line=3,col="grey")
}

dev.off()
#limy <- max(max(dfI$distance),max(dfII[dfII$V2==chr,]$V3/1000000))
#yaxt="no"  Hide axis and ticks
#legend("topright",legend=c("AK58","CS"),pch=c(1,1))