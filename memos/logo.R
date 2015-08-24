
dir <- tempdir()


library(extrafont)
loadfonts()

pdf(file = paste0(dir, "/experimentr_logo_tmp.pdf"), width = 5.5, height = 0.95)
par(mar = rep(0, 4), omi = rep(0, 4))
plot(0, 0, xlim = c(.03, 1), ylim = c(.1, .9), type = "n", axes = F, ylab = "", xlab = "")
text(-.02, .48, "experimentr", pos = 4, cex = 5, col = rgb(51/255,153/255,255/255), family = "Open Sans")
#top
points(.95,.69, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.95,.69, cex = 1.5, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)

#bottom 
points(.96,.42, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.96,.42, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)
points(.96,.42, cex = 2, col = "white", lwd = 4, pch = 16)
dev.off()
embed_fonts(paste0(dir, "/experimentr_logo_tmp.pdf"), outfile = paste0(dir, "/experimentr_logo.pdf"))
system(paste("open", dir))
