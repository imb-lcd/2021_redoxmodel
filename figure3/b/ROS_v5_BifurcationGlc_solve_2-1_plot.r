##### read data
#### set working directory
setwd("D:/Lab_SHC_files/ROS_model/008_afterChiaChouMod/003_fig3")

#### read data
sol <- read.table("ROS_v5_BifurcationGlc_solve_2-1_out.txt", header = TRUE, sep = "\t") 

#### convert from character type to numeric type
sol <- apply(sol, MARGIN = 2, FUN = as.numeric)
sol <- as.data.frame(sol)

#### compute rates
sol$gshsynrate <- sol$kGshPro * sol$cysmax * sol$glumax
sol$gshregrate <- 2 * sol$kGshRed * sol$gssgmin * sol$nadphmax

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
#### Glu
p1a <- ggplot(data = sol[3:101, ], aes(x = glc, y = glumax))
p1b1 <- p1a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 4000))
p1b2 <- p1b1 + geom_point(data = sol[3:16, ], aes(y = glumd), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = glumd), color = "grey40")
p1b3 <- p1b2 + geom_point(data = sol[1:16, ], aes(y = glumax), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = glumax), color = "red")
p1c <- p1b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "Glu / microM")
p1d <- p1c + gtheme + theme(legend.position = "none")
p1d

#### Cys2
p2a <- ggplot(data = sol[3:101, ], aes(x = glc, y = cys2min))
p2b1 <- p2a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 100))
p2b2 <- p2b1 + geom_point(data = sol[3:16, ], aes(y = cys2md), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = cys2md), color = "grey40")
p2b3 <- p2b2 + geom_point(data = sol[1:16, ], aes(y = cys2max), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = cys2max), color = "red")
p2c <- p2b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "Cys2 / microM")
p2d <- p2c + gtheme + theme(legend.position = "none") 
p2d
  
#### NADPH
p3a <- ggplot(data = sol[3:101, ], aes(x = glc, y = nadphmax))
p3b1 <- p3a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 1))
p3b2 <- p3b1 + geom_point(data = sol[3:16, ], aes(y = nadphmd), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = nadphmd), color = "grey40")
p3b3 <- p3b2 + geom_point(data = sol[1:16, ], aes(y = nadphmin), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = nadphmin), color = "red")
p3c <- p3b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "NADPH / microM")
p3d <- p3c + gtheme + theme(legend.position = "none") 
p3d

#### GSH
p4a <- ggplot(data = sol[3:101, ], aes(x = glc, y = gshmax))
p4b1 <- p4a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 4000))
p4b2 <- p4b1 + geom_point(data = sol[3:16, ], aes(y = gshmd), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = gshmd), color = "grey40")
p4b3 <- p4b2 + geom_point(data = sol[1:16, ], aes(y = gshmin), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = gshmin), color = "red")
p4c <- p4b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "GSH / microM")
p4d <- p4c + gtheme + theme(legend.position = "none") 
p4d

#### GSH + GSSG
p5a <- ggplot(data = sol[3:101, ], aes(x = glc, y = (gshmax + 2 * gssgmin)))
p5b1 <- p5a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 4000))
p5b2 <- p5b1 + geom_point(data = sol[3:16, ], aes(y = gshmd + 2 * gssgmd), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = gshmd + 2 * gssgmd), color = "grey40")
p5b3 <- p5b2 + geom_point(data = sol[1:16, ], aes(y = gshmin + 2 * gssgmax), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = gshmin + 2 * gssgmax), color = "red")
p5c <- p5b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "GSH + GSSG / microM")
p5d <- p5c + gtheme + theme(legend.position = "none") 
p5d

#### ROS
p6a <- ggplot(data = sol[3:101, ], aes(x = glc, y = rosmin))
p6b1 <- p6a + geom_point(shape = 1) + geom_path() + coord_cartesian(ylim = c(0, 2))
p6b2 <- p6b1 + geom_point(data = sol[3:16, ], aes(y = rosmd), shape = 1, color = "grey40") + geom_path(data = sol[3:16, ], aes(y = rosmd), color = "grey40")
p6b3 <- p6b2 + geom_point(data = sol[1:16, ], aes(y = rosmax), shape = 1, color = "red") + geom_path(data = sol[1:16, ], aes(y = rosmax), color = "red")
p6c <- p6b3 + scale_x_continuous(expan = c(0, 0), name = "Glc / microM") + scale_y_continuous(expan = c(0, 0), name = "ROS / microM")
p6d <- p6c + gtheme + theme(legend.position = "none") 
p6d

##### assemble subplots
ap1a <- plot_grid(p4d, p6d, ncol = 1, align = "hv", axis = "tblr")

ggsave2("ROS_v5_BifurcationGlc_solve_2-1.pdf", width = 2, height = 2.4, units = "in", dpi = 600)
