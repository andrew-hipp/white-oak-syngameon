library(adegenet)
library(phangorn)
library(ggplot2)
library(ape)
library(magrittr)
library(vegan)
library(gridExtra)

do.phylo.map <- FALSE

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dat.spp <- sapply(strsplit(row.names(dat.gen@tab), "|", fixed = T), function(x) x[1])
#dat.spp[dat.spp == 'INDET'] <- 'Quercus macrocarpa'
dat.spp[dat.spp == 'INDET'] <- 'Undetermined: seedling'
dat.spp.num <- as.factor(dat.spp) %>%
                as.integer

dat.gen.tab <- tab(dat.gen, NA.method = 'mean')

dat.dist <- dist(dat.gen.tab)

pdf('../out/upgma.allLoci.pdf', 8.5, 11)
dat.upgma <- upgma(dat.dist)
dat.upgma.mac <- drop.tip(dat.upgma, grep('macrocarpa', dat.upgma$tip.label, invert = T)) %>% ladderize
plot(dat.upgma, cex = 0.3, tip.color = cbbPalette[dat.spp.num])
dev.off()

dat.mds <- metaMDS(dat.gen.tab, 'euclidean', k = 3)
dat.mds$points.frame <- as.data.frame(dat.mds$points)
dat.mds$points.frame$Species <- dat.spp
#dat.mds$points.frame$Source <- NA
#dat.mds$points.frame$Source[grep('CC', row.names(dat.gen@tab))] <- 'MOR-germinated, seedlings'
#dat.mds$points.frame$Source[grep('MOR', row.names(dat.gen@tab))] <- 'Wild collected, trees'

p1 <- ggplot(dat.mds$points.frame, aes(x = MDS1, y = MDS2))
#p <- p + geom_point(aes(color = Species, shape = Source), size = 4)
p1 <- p1 + geom_point(aes(color = Species), size = 4)
p1 <- p1 + scale_colour_manual("Species, field ID", values = cbbPalette)
p1 <- p1 + theme(legend.position = 'none')
p2 <- ggplot(dat.mds$points.frame, aes(x = MDS1, y = MDS3))
#p2 <- p2 + geom_point(aes(color = Species, shape = Source), size = 4)
p2 <- p2 + geom_point(aes(color = Species), size = 4)
p2 <- p2 + scale_colour_manual("Species", values = cbbPalette)
p2 <- p2 + theme(legend.position = 'right')
pdf('../out/mds.plot.pdf', 10, 5)
grid.arrange(p1, p2, layout_matrix = matrix(c(1:2), 1), widths = c(3, 4.5))
dev.off()

if(do.phylo.map) {
  dat.seq.mapping.macro <- dat.seq.mapping[dat.upgma.mac$tip.label, c('lat', 'long')]
  dat.upgma.mac$tip.label <- gsub(' ', '_', dat.upgma.mac$tip.label)
  row.names(dat.seq.mapping.macro) <- gsub(' ', '_', row.names(dat.seq.mapping.macro))
  phylo.to.map(dat.upgma.mac, dat.seq.mapping.macro)
}
