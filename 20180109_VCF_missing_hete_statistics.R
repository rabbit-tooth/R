#Author by 2018/01/08                     VCF missing/hetero rate statistics | perfect
args <- commandArgs(T)                    #Rscript script.R vcf miss/hete snp/sam > txt/csv
df <- read.delim(args[1], header=F, sep="\t", comment.char="", fill=T, stringsAsFactors = F)

snp <- function(line,type){               #For snp
  content <- 0
  if(type=="miss"){
    for(i in 10:length(line)){
      if(line[i]=="./."){
        content <- content + 1
      }
    }
  }
  if(type=="hete"){
    for(i in 10:length(line)){
      if(substr(line[i],1,1) != substr(line[i],3,3)){
        content <- content + 1
      }
    }
  }
  percentage <- content/(length(line)-9)
}
sample <- function(col,type){             #For sample
  content <- 0
  if(type=="miss"){
    for(i in 2:length(col)){
      if(col[i]=="./."){
        content <- content + 1
      }
    }
  }
  if(type=="hete"){
    for(i in 2:length(col)){
      if(substr(col[i],1,1) != substr(col[i],3,3)){
        content <- content + 1
      }
    }
  }
  percentage <- content/(length(col)-1)
}

if(args[3]=="snp"){
  for(j in 2:nrow(df)){
    marker <- df[j,]
    rate <- snp(marker,args[2])
    cat(paste0(marker[1],"\t",marker[2],"\t",marker[3],"\t",rate,"\n"))
  }
}
if(args[3]=="sam"){
  for(j in 10:ncol(df)){
    sam <- df[,j]
    rate <- sample(sam,args[2])
    cat(paste0(sam[1],",",rate,"\n"))
  }
}