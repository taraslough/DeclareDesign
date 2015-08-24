
dir <- tempdir()


library(extrafont)
loadfonts()

pdf(file = paste0(dir, "/experimentr_logo_tmp.pdf"), width = 5.75, height = 1.35)
par(mar = rep(0, 4), omi = rep(0, 4))
plot(0, 0, xlim = c(.03, 1), ylim = c(.1, .9), type = "n", axes = F, ylab = "", xlab = "")
text(0.01, .48, "experimentr", pos = 4, cex = 5, col = rgb(51/255,153/255,255/255), family = "Open Sans")
#top
points(.965,.64, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.64, cex = 1.5, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)

#bottom 
points(.965,.43, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.43, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)
points(.965,.43, cex = 2, col = "white", lwd = 4, pch = 16)
dev.off()
embed_fonts(paste0(dir, "/experimentr_logo_tmp.pdf"), outfile = paste0(dir, "/experimentr_logo.pdf"))
system(paste("open", dir))

svg(file = paste0(dir, "/experimentr_logo.svg"), width = 5.75, height = 1.35)
par(mar = rep(0, 4), omi = rep(0, 4))
plot(0, 0, xlim = c(.03, 1), ylim = c(.1, .9), type = "n", axes = F, ylab = "", xlab = "")
text(0.01, .48, "experimentr", pos = 4, cex = 5, col = rgb(51/255,153/255,255/255), family = "Open Sans")
#top
points(.965,.64, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.64, cex = 1.5, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)

#bottom 
points(.965,.43, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.43, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)
points(.965,.43, cex = 2, col = "white", lwd = 4, pch = 16)
dev.off()
