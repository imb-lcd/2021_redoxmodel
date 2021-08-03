##### read data
#### set working directory
setwd("D:/Lab_SHC_files/ROS_model/008_afterChiaChouMod/003_fig3")

#### read data
sol1 <- read.table("ROS_v5_BifurcationCaRosFeedback_solve_1_out.txt", header = TRUE, sep = "\t") 
sol2 <- read.table("ROS_v5_BifurcationCys2NadphFeedback_solve_1_out.txt", header = TRUE, sep = "\t") 
sol3 <- read.table("ROS_v5_BifurcationGshRosFeedback_solve_1_out.txt", header = TRUE, sep = "\t") 
sol4 <- read.table("ROS_v5_BifurcationRtkRosFeedback_solve_1_out.txt", header = TRUE, sep = "\t") 

#### make numeric key data
sol1$gshmax <- as.numeric(sol1$gshmax)
sol1$gshmin <- as.numeric(sol1$gshmin)
sol1$rosmax <- as.numeric(sol1$rosmax)
sol1$rosmin <- as.numeric(sol1$rosmin)
sol2$gshmax <- as.numeric(sol2$gshmax)
sol2$gshmin <- as.numeric(sol2$gshmin)
sol2$rosmax <- as.numeric(sol2$rosmax)
sol2$rosmin <- as.numeric(sol2$rosmin)
sol3$gshmax <- as.numeric(sol3$gshmax)
sol3$gshmin <- as.numeric(sol3$gshmin)
sol3$rosmax <- as.numeric(sol3$rosmax)
sol3$rosmin <- as.numeric(sol3$rosmin)
sol4$gshmax <- as.numeric(sol4$gshmax)
sol4$gshmin <- as.numeric(sol4$gshmin)
sol4$rosmax <- as.numeric(sol4$rosmax)
sol4$rosmin <- as.numeric(sol4$rosmin)

##### libraries
library("ggplot2")
library("cowplot")
library("viridis")

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
	panel.grid.minor = element_line(color = "grey"), 
	panel.background = element_rect(fill = "white"), 
	panel.border = element_rect(fill = NA, color = "black"), 
	plot.title = element_text(color = "black", size = 8, hjust = 0.5),
	strip.background = element_rect(colour = "black", fill = "white"),
	strip.text.x = element_text(color = "black", size = 8),
	strip.text.y = element_text(color = "black", size = 8))

#### number of stable state
p1a <- ggplot(data = sol1, aes(x = foldkCaInRos, y = foldRos, fill = nStableState)) 
p1b <- p1a + geom_tile()
p1c <- p1b + scale_x_continuous(name = "foldkCaInRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_gradient(name = "# Stable state", guide = "legend", limits = c(0, 3))
p1d <- p1c + gtheme + labs(title = "Stability Regimes")
p1e <- p1d + theme(legend.position = "none")
p1e

leg <- get_legend(p1d)

p2a <- ggplot(data = sol2, aes(x = foldkNadphOxCys2, y = foldkCys2Red, fill = nStableState)) 
p2b <- p2a + geom_tile()
p2c <- p2b + scale_x_continuous(name = "foldkNadphOxCys2", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldkCys2Red") + scale_fill_gradient(name = "# Stable state", guide = "legend", limits = c(0, 3))
p2d <- p2c + gtheme + labs(title = "Stability Regimes")
p2e <- p2d + theme(legend.position = "none")
p2e

p3a <- ggplot(data = sol3, aes(x = foldkGshOx, y = foldkRosDeg, fill = nStableState)) 
p3b <- p3a + geom_tile()
p3c <- p3b + scale_x_continuous(name = "foldkGshOx", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldkRosDeg") + scale_fill_gradient(name = "# Stable state", guide = "legend", limits = c(0, 3))
p3d <- p3c + gtheme + labs(title = "Stability Regimes")
p3e <- p3d + theme(legend.position = "none")
p3e

p4a <- ggplot(data = sol4, aes(x = foldkPptaseOxRos, y = foldRos, fill = nStableState)) 
p4b <- p4a + geom_tile()
p4c <- p4b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_gradient(name = "# Stable state", guide = "legend", limits = c(0, 3))
p4d <- p4c + gtheme + labs(title = "Stability Regimes")
p4e <- p4d + theme(legend.position = "none")
p4e

#### assemble subplots
ap1a <- plot_grid(p1e, p2e, p3e, p4e, nrow = 2)
ap1b <- plot_grid(ap1a, leg, ncol = 2, rel_widths = c(8, 2))
ap1b

ggsave2("ROS_v5_Bifurcation4Feedbacks_1.pdf", width = 4.5, height = 3.6, units = "in")
