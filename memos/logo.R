
library(extrafont)
loadfonts()

pdf(file = "experimentr_logo_tmp.pdf", width = 5.75, height = 4)
par(mar = rep(0, 4), omi = rep(0, 4))
plot(0, 0, xlim = c(.03, 1), ylim = c(.1, .9), type = "n", axes = F, ylab = "", xlab = "")
text(0.01, .48, "experimentr", pos = 4, cex = 5, col = rgb(51/255,153/255,255/255), family = "Open Sans")
#top
points(.965,.53, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.53, cex = 1.5, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)

#bottom 
points(.965,.46, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.46, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)
points(.965,.46, cex = 2, col = "white", lwd = 4, pch = 16)
dev.off()
embed_fonts("experimentr_logo_tmp.pdf", outfile = "experimentr_logo.pdf")
system("rm experimentr_logo_tmp.pdf")

svg(file = "experimentr_logo.svg", width = 5.75, height = 4)
par(mar = rep(0, 4), omi = rep(0, 4))
plot(0, 0, xlim = c(.03, 1), ylim = c(.1, .9), type = "n", axes = F, ylab = "", xlab = "")
text(0.01, .48, "experimentr", pos = 4, cex = 5, col = rgb(51/255,153/255,255/255), family = "Open Sans")
#top
points(.965,.53, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.53, cex = 1.5, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)

#bottom 
points(.965,.46, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4)
points(.965,.46, cex = 3.2, col = rgb(51/255,153/255,255/255), lwd = 4, pch = 16)
points(.965,.46, cex = 2, col = "white", lwd = 4, pch = 16)
dev.off()
