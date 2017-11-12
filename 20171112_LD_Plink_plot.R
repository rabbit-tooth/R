#Author by 2017/10/16         R --vanilla --slave --args chr < Plink_LD_plot.R
args <- commandArgs()         #Plot Plink LD block
chr <- args[5]

plotLD <- function(p){
  popu <- paste0(p,chr)
  df <- read.delim(paste0(popu,".blocks.det"), header=T, sep="", comment.char="", fill=T, stringsAsFactors=F)
  x <- ((df[,3]-df[,2])/2+df[,2])/1000000
  y <- df[,4]/1000
  xlm <- max(df$BP2)/1000000
  ylm <- max(df$KB)/1000
  
  curl <- spline(x,y,n=1000)
  pic <- plot(curl,main=chr,xlim=c(0,xlm),ylim=c(0,ylm),xlab="Position (Mb)",ylab="BlockSize (Mb)",type="l",lwd=1,xaxs="i",col="green",col.lab="2")
  grid(nx=15,ny=2,lwd=0.5)
}

pdf(file=paste0(chr,".pdf"),width=10,height=6,bg=rgb(1,1,1))
par(mfrow=c(3,1),mar=c(3,3,3,1),mgp=c(1.5,0.5,0))
options(scipen=200)

for(i in 1:3){
  if(i==1){
    plotLD("W")
  }
  if(i==2){
    plotLD("L")
  }
  if(i==3){
    plotLD("M")
  }
}

dev.off()