#Author by 2019/04/01                           KaKs distribution plot
args <- commandArgs(T)                          #Rscript script.R in.kaks out.pdf
library(ggplot2)

df <- read.delim(args[1], sep="\t", header=T)   #Ka\tKs\tColr(purify/neutral/positive)

p <- ggplot(df, aes(x=Ka,y=Ks,colour=factor(Colr)))
p <- p + geom_point(shape=20, size=0.1, alpha=0.8)
p <- p + xlim(0,max(df$Ka)) + ylim(0,max(df$Ks))
p <- p + labs(x="Ka", y="Ks", title="KaKs Distribution", subtitle="")
p <- p + scale_colour_manual(name="orth_type",
                             breaks=c("purify","positive","neutral"),
                             labels=c("conserve","divergent","neutral"),
                             values=c("green","black","red"))
p <- p + theme(plot.title=element_text(hjust=0.5,size=10),
               plot.subtitle=element_text(hjust=0.5,size=10),
               axis.title=element_text(size=10),
               axis.text=element_text(size=6),
               legend.title=element_text(size=10))
ggsave(args[2], p, width=5, height=5)