#
# # load("/home/clement/Downloads/extdata/constrastDEA.rda")
# # load("/home/clement/Downloads/extdata/designDEA.rda")
# # load("/home/clement/Downloads/extdata/RawdataDEA.rda")
# library(limma)
# library(DESeq2)
# library(edgeR)
# library(ggplot2)
# contrast <- read.table("/home/clement/Downloads/extdata/constrastDEA",sep = ",", row.names = 1)
# design <- read.table("/home/clement/Downloads/extdata/designDEA",sep = ",", row.names = 1)
# Rawdata <- read.table("/home/clement/Downloads/extdata/RawdataDEA",sep = ",", row.names = 1)
#
# head(contrast)
# head(design)
# head(Rawdata)
#
# y <- DGEList(counts=Rawdata, genes=rownames(Rawdata))
# v <- voom(y, design, plot=FALSE)
#   v <- voom(y, design, plot=TRUE, save.plot = TRUE)
#   dfplot <- data.frame(Mean = v$voom.xy$x, sd = v$voom.xy$y)
#   dfline <- data.frame(x = v$voom.line$x,y = v$voom.line$y)
#
#   ggplot <- ggplot(dfplot, aes(x= Mean,y = sd, label = rownames(dfplot))) +
#     geom_point(data = dfplot,
#                color = "blue", alpha = 0.05) +
#     labs(title = "Scatter plot on voom normalized value",x = v$voom.xy$xlab,
#          y = v$voom.xy$ylab) +
#     geom_line(data = dfline,aes(x=x,y=y))
#
# plot(ggplot)
#
# fit <- lmFit(v, design)
# fit2 <- contrasts.fit(fit,contrast)
# fit2 <- eBayes(fit2)
# res <- topTable(fit2, number=nrow(Rawdata), adjust.method="BH")
# res <- res[order(res$adj.P.Val),]
# rownames(res) <- res$genes
# res$genes  <- NULL
# nsign <- length(which(res$adj.P.Val < 0.05))
# nsignfc <- length(which(res$adj.P.Val < 0.05 & abs(res$logFC) > 1))
# #up <- which(res$adj.P.Val < 0.05 & res$logFC > 0.5)
# up <- which(res$logFC > 1)
# #down <- which(res$adj.P.Val < 0.05 & res$logFC < -0.5)
# down <- which(res$logFC < -1)
# sign <- which(res$adj.P.Val < 0.05)
#
#
#
# # res$AveExpr <- round(res$AveExpr,digits = 0)
# # for (row in 1:nrow(res)){
# # res[,'myAverage'] <-
# # }
#
# pdf(file = "hist")
# ggplot(data = res) + aes(x = `P.Value`) +
#   geom_histogram(fill = "steelblue",breaks = seq(0, 1, length.out = 20))
# dev.off()
#
# pdf(file = "splittedhist")
# # ggplot(data = res) + aes(x = `P.Value`) +
# #   geom_histogram(fill = "steelblue",breaks = seq(0, 1, length.out = 20)) +
# #  #geom_histogram(fill = "steelblue")
# #   facet_grid(~AveExpr)
# # dev.off()
#
#
# #ggplot(data = res) + aes(x = `P.Value`) +
#  # geom_histogram(fill = "steelblue",breaks = seq(0, 1, length.out = 20)) +
#   #geom_histogram(fill = "steelblue")
#   #facet_wrap(~AveExpr)
#
# # ggplot(data = res) + aes(x = `P.Value`) +
# #   geom_histogram(fill = "steelblue",breaks = seq(0, 1, length.out = 20)) +
# #   #geom_histogram(fill = "steelblue")
# #   facet_wrap(~abs(AveExpr))
#
# dev.off()
#
# volcanoplot(fit2, highlight = 40, names = rownames(fit))
# #head(res)
#
# with(res, plot(logFC, -log10(adj.P.Val), pch=20, main="Volcano plot"))
# with(res[up,], points(logFC, -log10(adj.P.Val), pch=20, col="green"))
# with(res[down,], points(logFC, -log10(adj.P.Val), pch=20, col="red"))
#
#
# with(res, plot(logFC, -log10(P.Value), pch=20, main="Volcano plot"))
# with(res[up,], points(logFC, -log10(P.Value), pch=20, col="green"))
# with(res[down,], points(logFC, -log10(adj.P.Value), pch=20, col="red"))
#
# ######## utre methode ############
# ### define interesting genes
# #interesting_genes <- c('Il17s', 'Casz1', 'Rorc', 'Il22')
# #fc_interesting_genes <- c(-1.8, -1.3, -0.9, -0.85)
# #pval_interesting_genes <- log10(c(0.0005, 0.01, 1e-4, 4e-05))
#
# ### define colours (grey, blue, red)
# palette <- c(rgb(0, 0, 0, max = 255, alpha = 65),
#              rgb(0, 0, 255, max = 255, alpha = 125),
#              rgb(255, 0, 0, max = 255, alpha = 125),
#              rgb(24, 249, 6, max = 255, alpha = 125))
#
# point_type = 20
# point_size = 1.5
# library(ggrepel)
# ### plot corpus
# plot <- plot(res$logFC, -log10(res$P.Value),
#      pch = point_type, cex = point_size, col = palette[1],
#      xlab = "Fold Change (log2)", ylab = "-log10(P value)") + abline(h=5,v=c(-1,1), lty=2) +
#   geom_text(aes(label = rownames(res)),size = 3.5)
#
# points(res[up,]$logFC,-log10(res[up,]$P.Value),pch = point_type, cex = point_size, col = palette[2])
# points(res[down,]$logFC,-log10(res[down,]$P.Value),pch = point_type, cex = point_size, col = palette[3])
# points(res[sign,]$logFC,-log10(res[sign,]$P.Value), pch = point_type, cex = point_size,, col = palette[4])
#
# #ggplot <- ggplot(res)
#
#
# aty <- axTicks(2)
# labels <- sapply(aty,function(i)
#   as.expression(bquote(10^ .(i)))
# )
# labels[6] <- 1
# axis(2,at=aty,labels=labels)
#
#
# res$label <- rownames(res)
#
#
# # transformation function for reverse log1p axis
# revlog_trans <- function(base = exp(1)) {
#   trans <- function(x) -log1p(x)
#   inv <- function(x) expm1(-x)
#   scales::trans_new("revlog1p", trans, inv, domain = c(0, Inf))
# }
#
# ggplot <- ggplot(res, aes(x = logFC, y = -log10(P.Value), label = adj.P.Val)) +
#   scale_fill_gradient(low = "lightgray", high = "navy") +
#   scale_color_gradient(low = "lightgray", high = "navy") +
#   #scale_y_continuous(trans = revlog_trans(), expand = c(0.005, 0.005)) +
#   expand_limits(y = c(min(-log10(res$P.Value)), 1)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon",
#                   show.legend = FALSE) +
#   geom_point(data = res,
#              color = "grey", alpha = 0.5) +
#   geom_point(data = subset(res, logFC > 1  & adj.P.Val > 0.05),
#              color = "red", alpha = 0.5) +
#   geom_point(data = subset(res, logFC < -1),
#              color = "blue", alpha = 0.5) +
#   geom_point(data = subset(res, adj.P.Val < 0.05),
#              color = "green", alpha = 0.5) +
#   geom_vline(xintercept = min(-log10(res$P.Value))) +
#   geom_hline(yintercept = min(-log10(res$P.Value))) +
#   geom_hline(yintercept = 5, linetype = "dashed") +
#   geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
#   theme_linedraw() +
#   theme(panel.grid = element_blank()) +
#   xlab("Fold change (log2)") +
#   ylab("-log10(P-Value)") +
#   geom_text_repel(
#     data = subset(res, adj.P.Val < 0.05),
#     aes(label = label),
#     size = 5,
#     box.padding = unit(0.35, "lines"),
#     point.padding = unit(0.3, "lines")
#   )
# plot(ggplot)
#
