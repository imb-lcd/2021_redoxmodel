##### read data
#### set working directory
# setwd("D:/Lab_SHC_files/ROS_model/008_afterChiaChouMod/004_fig4")

#### read data
sol <- read.table("ROS_v6_TimeEvolution_ndsolve_4-2_out.txt", header = TRUE, sep = "\t") 

#### convert from character type to numeric type
sol <- apply(sol, MARGIN = 2, FUN = as.numeric)
sol <- as.data.frame(sol)

##### libraries
library("ggplot2")
library("cowplot")
library("viridis")
library("ggrepel")

##### plot themes
gtheme <- theme(
	axis.line = element_line(color = "black"), 
	axis.ticks = element_line(color = "black"), 
	axis.text = element_text(color = "black", size = 8), 
	axis.title = element_text(color = "black", size = 8), 
	legend.key = element_rect(fill = "white"), 
	legend.text = element_text(color = "black", size = 8), 
	legend.title = element_text(color = "black", size = 8), 
	panel.grid.major = element_line(color = "grey"), 
	panel.grid.minor = element_line(color = NA), 
	panel.background = element_rect(fill = "white"), 
	panel.border = element_rect(fill = NA, color = "black"), 
	plot.title = element_text(color = "black", size = 8),
	strip.background = element_rect(colour = "black", fill = "white"),
	strip.text.x = element_text(color = "black", size = 8),
	strip.text.y = element_text(color = "black", size = 8))
	
##### plot data
#### ROS
p1a <- ggplot(data = sol, aes(x = t, y = ros.t., group = x, color = log2(x)))
p1b <- p1a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 2))
p1c <- p1b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "ROS / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p1d <- p1c + gtheme
p1e <- p1d + theme(legend.position = "none")
p1e

leg <- get_legend(p1d)

#### GSH
p2a <- ggplot(data = sol, aes(x = t, y = gsh.t., group = x, color = log2(x)))
p2b <- p2a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 4000))
p2c <- p2b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "GSH / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p2d <- p2c + gtheme
p2e <- p2d + theme(legend.position = "none")
p2e

#### GSSG
p3a <- ggplot(data = sol, aes(x = t, y = gssg.t., group = x, color = log2(x)))
p3b <- p3a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 2000))
p3c <- p3b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "GSSG / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p3d <- p3c + gtheme
p3e <- p3d + theme(legend.position = "none")
p3e

#### NADPH
p4a <- ggplot(data = sol, aes(x = t, y = nadph.t., group = x, color = log2(x)))
p4b <- p4a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 1))
p4c <- p4b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "NADPH / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p4d <- p4c + gtheme
p4e <- p4d + theme(legend.position = "none")
p4e

p4a <- ggplot(data = sol, aes(x = t, y = nadph.t., group = x, color = log2(x)))
p4b2 <- p4a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 20), ylim = c(0, 1))
p4c2 <- p4b2 + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "NADPH / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p4d2 <- p4c2 + gtheme
p4e2 <- p4d2 + theme(legend.position = "none")
p4e2

#### Cys
p5a <- ggplot(data = sol, aes(x = t, y = cys.t., group = x, color = log2(x)))
p5b <- p5a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 1000))
p5c <- p5b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "Cys / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p5d <- p5c + gtheme
p5e <- p5d + theme(legend.position = "none")
p5e

#### Cys2
p6a <- ggplot(data = sol, aes(x = t, y = cys2.t., group = x, color = log2(x)))
p6b <- p6a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 500))
p6c <- p6b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "Cys2 / microM") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p6d <- p6c + gtheme
p6e <- p6d + theme(legend.position = "none")
p6e

##### assemble subplots
ap1a <- plot_grid(p4e, p2e, p4e2, p1e, nrow = 2, align = "hv")#, axis = "tblr")
ap1b <- plot_grid(ap1a, leg, rel_widths = c(8, 2))
ap1b
ggsave2("ROS_v6_TimeEvolution_ndsolve_4-2_plot_1.pdf", width = 5, height = 2.4, units = "in", dpi = 600)

ap1c <- plot_grid(p2e, p3e, p5e, p6e, nrow = 2, align = "hv", axis = "tblr")
ap1d <- plot_grid(ap1c, leg, rel_widths = c(8, 2))
ap1d
ggsave2("ROS_v6_TimeEvolution_ndsolve_4-2_plot_2.pdf", width = 5, height = 2.4, units = "in", dpi = 600)

##### initial rates and redox catastrophe time 
#### maximal d ros / d t
soly <- NA

for(i in 1:length(unique(sol$x))){

	tmp <- sol[which(sol$x == unique(sol$x)[i]), ]
	soly <- rbind(soly, tmp[which.max(tmp$ros..t.), c("t", "ros..t.", "x")])
	rm(tmp)

}

soly <- soly[-1, ]
colnames(soly) <- c("tmax", "ros..t.max", "xmax")

#### d ros / d t and catastrophe time
### initial d ros / d t
sol01 <- sol[which(sol$t == 0), c("t", "ros..t.", "x")] ### very negative ros'[t] due to loss of glc contribution to ros production
sol11 <- sol[which(sol$t == 1), c("t", "ros..t.", "x")] ### the "initial" rate "after glc deprivation" 
solx1 <- cbind(sol01, sol11)
colnames(solx1) <- c("t0", "ros..t.0", "x0", "t1", "ros..t.1", "x1")

solz1 <- cbind(solx1, soly)

### plot
p7a <- ggplot(data = solz1, aes(x = ros..t.1, y = tmax, color = log2(xmax)))
p7b <- p7a + geom_point() + coord_cartesian(ylim = c(0, 100))
p7c <- p7b + scale_x_continuous(name = "Init. dROS / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Tmax / min") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p7d <- p7c + gtheme
p7e <- p7d + theme(legend.position = "none")
p7e

#### d gsh / d t and catastrophe time
### initial d gsh / d t
sol02 <- sol[which(sol$t == 0), c("t", "gsh..t.", "x")] ### very negative gsh'[t] due to loss of glc contribution to gsh production
sol12 <- sol[which(sol$t == 1), c("t", "gsh..t.", "x")] ### the "initial" rate "after glc deprivation" 
solx2 <- cbind(sol02, sol12)
colnames(solx2) <- c("t0", "gsh..t.0", "x0", "t1", "gsh..t.1", "x1")

solz2 <- cbind(solx2, soly)

### plot
p8a <- ggplot(data = solz2, aes(x = gsh..t.1, y = tmax, color = log2(xmax)))
p8b <- p8a + geom_point() + coord_cartesian(ylim = c(0, 100))
p8c <- p8b + scale_x_continuous(name = "Init. dGSH / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Tmax / min") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p8d <- p8c + gtheme
p8e <- p8d + theme(legend.position = "none")
p8e

#### d cys2 / d t and catastrophe time
### initial d cys2 / d t
sol03 <- sol[which(sol$t == 0), c("t", "cys2..t.", "x")] ### very negative cys2'[t] due to loss of glc contribution to cys2 production
sol13 <- sol[which(sol$t == 1), c("t", "cys2..t.", "x")] ### the "initial" rate "after glc deprivation" 
solx3 <- cbind(sol03, sol13)
colnames(solx3) <- c("t0", "cys2..t.0", "x0", "t1", "cys2..t.1", "x1")

solz3 <- cbind(solx3, soly)

### plot
p9a <- ggplot(data = solz3, aes(x = cys2..t.1, y = tmax, color = log2(xmax)))
p9b <- p9a + geom_point() + coord_cartesian(ylim = c(0, 100))
p9c <- p9b + scale_x_continuous(name = "Init. dCys2 / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Tmax / min") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p9d <- p9c + gtheme
p9e <- p9d + theme(legend.position = "none")
p9e

#### d nadph / d t and catastrophe time
### initial d nadph / d t
sol04 <- sol[which(sol$t == 0), c("t", "nadph..t.", "x")] ### very negative nadph'[t] due to loss of glc contribution to nadph production
sol14 <- sol[which(sol$t == 1), c("t", "nadph..t.", "x")] ### the "initial" rate "after glc deprivation" 
solx4 <- cbind(sol04, sol14)
colnames(solx4) <- c("t0", "nadph..t.0", "x0", "t1", "nadph..t.1", "x1")

solz4 <- cbind(solx4, soly)

### plot
p10a <- ggplot(data = solz4, aes(x = nadph..t.1, y = tmax, color = log2(xmax)))
p10b <- p10a + geom_point() + coord_cartesian(ylim = c(0, 100))
p10c <- p10b + scale_x_continuous(name = "Init. dNADPH / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Tmax / min") + scale_color_gradient2(name = "Fold Cys2 & Gln", guide = "legend", high = "darkred", low = "darkblue", mid = "grey60", breaks = seq(-3, 3, 1))
p10d <- p10c + gtheme
p10e <- p10d + theme(legend.position = "none")
p10e

##### assemble subplots
ap1e <- plot_grid(p7e, p8e, p9e, p10e, ncol = 1, align = "hv", axis = "tblr")
ap1e
ggsave2("ROS_v6_TimeEvolution_ndsolve_4-2_plot_3.pdf", width = 2, height = 4.8, units = "in", dpi = 600)
