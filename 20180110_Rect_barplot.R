#Author by 2018/01/10                       Rect for barplot
args <- commandArgs(T)                      #Rscript script.R input
stt <- 2
end <- 3
hgt <- 4

df <- read.delim(args[1], header=T, sep="", comment.char="", fill=T, stringsAsFactors=F)
df[,stt] <- df[,stt]/1000000
df[,end] <- df[,end]/1000000
df[,hgt] <- df[,hgt]/1000

prefix <- unlist(strsplit(args[1],split="[.]"))[1]      #fixed=TRUE
pdf(file=paste0(prefix,".pdf"),width=10,height=6,bg=rgb(1,1,1))
par(mar=c(3,3,2,1),mgp=c(2,0.6,0))
plot(c(0,max(df[,end])), c(0,200), main=prefix, xlab="Position (Mb)", ylab="BlockSize (Mb)", xaxs="i", type="n",
     cex.lab=0.9, cex.axis=0.8, font.axis=5)
grid(nx=10,ny=5,lwd=0.5)

for(i in 1:nrow(df)){
  rect(df[i,stt], 0, df[i,end], df[i,hgt], col="green", border=NA)
}

dev.off()