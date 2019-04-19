#Author by 2019/04/17                                         Gene density plot
library(RColorBrewer)
library(ComplexHeatmap)
library(gtrellis)
library(circlize)

chrs <- read.delim("chrs.bed3", head=T, sep="\t")             #bed3:chr\tstart\tend
gene <- read.delim("gene.bed3", head=T, sep="\t")

density <- genomicDensity(gene, window.size=2e6, overlap=T)

colr <- colorRamp2(seq(0,max(density[[4]]),length=11), rev(brewer.pal(11,"RdYlBu")))
cm <- ColorMapping(col_fun=colr)
lgd <- color_mapping_legend(cm, plot=T, title="legend", color_bar="continuous")

pdf(file="density.pdf", width=7, height=7)
gtrellis_layout(chrs, byrow=F, ncol=1, title="Gene Density\n", xlab="Chr Position",
                asist_ticks=F, track_axis=F, border=F, title_fontsize=12,
                legend=lgd, padding=unit(c(0.01,0.01,0.03,0.01),"npc"),
                xpadding=c(0.1,0), track_height=unit(6,"mm"))
add_heatmap_track(density, density[[4]], fill=colr, track=1)
add_track(track=1, clip=T,
  panel_fun=function(){
    chr <- get_cell_meta_data("name")
    grid.text(chr, x=0.01, y=unit(0.5,"npc"), just=c("left","centre"))
    if(chr=="chr7D"){
      grid.lines(get_cell_meta_data("xlim"), unit(c(0,0),"npc"), default.units="native")
    }
  }
)
dev.off()