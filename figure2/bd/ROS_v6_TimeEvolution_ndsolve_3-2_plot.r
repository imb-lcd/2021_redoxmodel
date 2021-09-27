##### read data
#### set working directory
# setwd("D:/Lab_SHC_files/ROS_model/009_afterDiscussionLCD/Fig1")

#### read data
sol <- read.table("ROS_v6_TimeEvolution_ndsolve_3-2_out.txt", header = TRUE, sep = "\t") 

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
p1a <- ggplot(data = sol, aes(x = t, y = ros.t., group = glc, color = glc))
p1b <- p1a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 2))
p1c <- p1b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "ROS / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p1d <- p1c + gtheme
p1e <- p1d + theme(legend.position = "none")
p1e

leg <- get_legend(p1d)

#### GSH
p2a <- ggplot(data = sol, aes(x = t, y = gsh.t., group = glc, color = glc))
p2b <- p2a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 4000))
p2c <- p2b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "GSH / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p2d <- p2c + gtheme
p2e <- p2d + theme(legend.position = "none")
p2e

#### GSSG
p3a <- ggplot(data = sol, aes(x = t, y = gssg.t., group = glc, color = glc))
p3b <- p3a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 2000))
p3c <- p3b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "GSSG / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p3d <- p3c + gtheme
p3e <- p3d + theme(legend.position = "none")
p3e

#### NADPH
p4a <- ggplot(data = sol, aes(x = t, y = nadph.t., group = glc, color = glc))
p4b <- p4a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 1))
p4c <- p4b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "NADPH / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p4d <- p4c + gtheme
p4e <- p4d + theme(legend.position = "none")
p4e

p4a <- ggplot(data = sol, aes(x = t, y = nadph.t., group = glc, color = glc))
p4b2 <- p4a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 20), ylim = c(0, 1))
p4c2 <- p4b2 + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "NADPH / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p4d2 <- p4c2 + gtheme
p4e2 <- p4d2 + theme(legend.position = "none")
p4e2

#### Cys
p5a <- ggplot(data = sol, aes(x = t, y = cys.t., group = glc, color = glc))
p5b <- p5a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 1000))
p5c <- p5b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "Cys / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p5d <- p5c + gtheme
p5e <- p5d + theme(legend.position = "none")
p5e

#### Cys2
p6a <- ggplot(data = sol, aes(x = t, y = cys2.t., group = glc, color = glc))
p6b <- p6a + geom_point(shape = 1) + geom_path() + coord_cartesian(xlim = c(0, 200), ylim = c(0, 250))
p6c <- p6b + scale_x_continuous(expand = c(0, 0), name = "Time / min") + scale_y_continuous(expand = c(0, 0), name = "Cys2 / microM") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p6d <- p6c + gtheme
p6e <- p6d + theme(legend.position = "none")
p6e

#### initial rates and catastrophe time
### initial rates
sol0 <- sol[which(sol$t == 0), "ros..t."] ### very negative ros'[t] due to loss of glc contribution to ros production
sol1 <- sol[which(sol$t == 1), "ros..t."] ### the "initial" rate "after glc deprivation" 
sol2 <- sol[which(sol$t == 1), "gsh..t."]
sol3 <- sol[which(sol$t == 1), "nadph..t."]
sol4 <- sol[which(sol$t == 1), "cys2..t."]
solx <- cbind(sol0, sol1, sol2, sol3, sol4)
colnames(solx) <- c("ros..t.0", "ros..t.1", "gsh..t.1", "nadph..t.1", "cys2..t.1")

### maximal d ros / d t
soly <- NA

for(i in 1:length(unique(sol$glc))){

	tmp <- sol[which(sol$glc == unique(sol$glc)[i]), ]
	soly <- rbind(soly, tmp[which.max(tmp$ros..t.), c("t", "ros..t.", "glc")])
	rm(tmp)

}

soly <- soly[-1, ]
colnames(soly) <- c("tmax", "ros..t.max", "glc")
solz <- cbind(solx, soly)

### plot
p7a <- ggplot(data = solz, aes(x = ros..t.1, y = tmax, color = glc))
p7b <- p7a + geom_point(size = 2) + coord_cartesian(xlim = c(0.00015, 0.00019), ylim = c(0, 100))
p7c <- p7b + scale_x_continuous(expand = c(0, 0), name = "Init. dROS / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Time Max dROS / dt / (microM / min)") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p7d <- p7c + gtheme
p7e <- p7d + theme(legend.position = "none")
p7e

p8a <- ggplot(data = solz, aes(x = gsh..t.1, y = tmax, color = glc))
p8b <- p8a + geom_point(size = 2) + coord_cartesian(xlim = c(-215, -175), ylim = c(0, 100))
p8c <- p8b + scale_x_continuous(expand = c(0, 0), name = "Init. dGSH / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Time Max dROS / dt / (microM / min)") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p8d <- p8c + gtheme
p8e <- p8d + theme(legend.position = "none")
p8e

p9a <- ggplot(data = solz, aes(x = nadph..t.1, y = tmax, color = glc))
p9b <- p9a + geom_point(size = 2) + coord_cartesian(xlim = c(-0.025, -0.021), ylim = c(0, 100))
p9c <- p9b + scale_x_continuous(expand = c(0, 0), name = "Init. dNADPH / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Time Max dROS / dt / (microM / min)") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p9d <- p9c + gtheme
p9e <- p9d + theme(legend.position = "none")
p9e

p10a <- ggplot(data = solz, aes(x = cys2..t.1, y = tmax, color = glc))
p10b <- p10a + geom_point(size = 2) + coord_cartesian(xlim = c(5, 25), ylim = c(0, 100))
p10c <- p10b + scale_x_continuous(expand = c(0, 0), name = "Init. dCys2 / dt / (microM / min)") + scale_y_continuous(expand = c(0, 0), name = "Time Max dROS / dt / (microM / min)") + scale_color_continuous(name = "Glc / microM", guide = "legend")
p10d <- p10c + gtheme
p10e <- p10d + theme(legend.position = "none")
p10e

leg2 <- get_legend(p10d)

##### assemble subplots
ap1a <- plot_grid(p4e, p2e, p4e2, p1e, nrow = 2, align = "hv")#, axis = "tblr")
ap1b <- plot_grid(ap1a, leg, rel_widths = c(8, 2))
ap1b
ggsave2("ROS_v6_TimeEvolution_ndsolve_3-2_plot_1.pdf", width = 5, height = 2.4, units = "in", dpi = 600)

ap1c <- plot_grid(p2e, p3e, p5e, p6e, nrow = 2, align = "hv", axis = "tblr")
ap1d <- plot_grid(ap1c, leg, rel_widths = c(8, 2))
ap1d
ggsave2("ROS_v6_TimeEvolution_ndsolve_3-2_plot_2.pdf", width = 5, height = 2.4, units = "in", dpi = 600)

ap1e <- plot_grid(p7e, p8e, p9e, p10e, ncol = 1)
ap1f <- plot_grid(ap1e, leg2, ncol = 2, rel_widths = c(2, 1))
ggsave2("ROS_v6_TimeEvolution_ndsolve_3-2_plot_3.pdf", width = 3, height = 4.8, units = "in", dpi = 600)
