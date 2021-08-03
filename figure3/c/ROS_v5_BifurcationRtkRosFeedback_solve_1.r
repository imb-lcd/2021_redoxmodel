##### read data
#### set working directory
setwd("D:/Lab_SHC_files/ROS_model/008_afterChiaChouMod/003_fig3")

#### read data
sol <- read.table("ROS_v5_BifurcationRtkRosFeedback_solve_1_out.txt", header = TRUE, sep = "\t") 

#### make numeric key data
sol$gshmax <- as.numeric(sol$gshmax)
sol$gshmin <- as.numeric(sol$gshmin)
sol$rosmax <- as.numeric(sol$rosmax)
sol$rosmin <- as.numeric(sol$rosmin)

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

##### plot
#### high GSH, low ROS
### GSH
p1a <- ggplot(data = sol, aes(x = foldkPptaseOxRos, y = foldRos, fill = gshmax)) 
p1b <- p1a + geom_tile()
p1c <- p1b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_viridis(name = "GSH / microM", limits = c(0, 4000))
p1d <- p1c + gtheme + labs(title = "Max(GSH)")
p1e <- p1d + theme(legend.position = "none")
p1e

leg1 <- get_legend(p1d)

### ROS
p2a <- ggplot(data = sol, aes(x = foldkPptaseOxRos, y = foldRos, fill = rosmin)) 
p2b <- p2a + geom_tile()
p2c <- p2b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_viridis(name = "ROS / microM", limits = c(0, 2), option = "plasma")
p2d <- p2c + gtheme + labs(title = "Min(ROS)")
p2e <- p2d + theme(legend.position = "none")
p2e

leg2 <- get_legend(p2d)

#### low GSH, high ROS
### GSH
p3a <- ggplot(data = sol, aes(x = foldkPptaseOxRos, y = foldRos, fill = gshmin)) 
p3b <- p3a + geom_tile()
p3c <- p3b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_viridis(name = "GSH / microM", limits = c(0, 4000))
p3d <- p3c + gtheme + labs(title = "Min(GSH)")
p3e <- p3d + theme(legend.position = "none")
p3e

### ROS
p4a <- ggplot(data = sol, aes(x = foldkPptaseOxRos, y = foldRos, fill = rosmax)) 
p4b <- p4a + geom_tile()
p4c <- p4b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_viridis(name = "ROS / microM", limits = c(0, 2), option = "plasma")
p4d <- p4c + gtheme + labs(title = "Max(ROS)")
p4e <- p4d + theme(legend.position = "none")
p4e

#### number of stable state
p5a <- ggplot(data = sol, aes(x = foldkPptaseOxRos, y = foldRos, fill = nStableState)) 
p5b <- p5a + geom_tile()
p5c <- p5b + scale_x_continuous(name = "foldkPptaseOxRos", expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name = "foldRos") + scale_fill_gradient(name = "# Stable state", guide = "legend", limits = c(0, 3))
p5d <- p5c + gtheme + labs(title = "Stability Regimes")
p5e <- p5d + theme(legend.position = "none")
p5e

leg3 <- get_legend(p5d)

#### assemble subplots
ap1a <- plot_grid(p1e, p2e, p5e, nrow = 1)
ap1b <- plot_grid(leg1, leg2, NULL, leg3)
ap1c <- plot_grid(p3e, p4e, ap1b, nrow = 1)
ap1d <- plot_grid(ap1a, ap1c, ncol = 1)
ap1d

ggsave2("ROS_v5_BifurcationRtkRosFeedback_solve_1.pdf", width = 5.4, height = 3.6, units = "in")
