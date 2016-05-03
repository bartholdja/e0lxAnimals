x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

plot_tau_vs_e0 <-
  ggplot(out, aes(x = e0, y = tau)) +
  # main
  geom_point(alpha = 0.9, size = 1.8, aes(colour = factor(class))) +
  labs(colour = "Class")+
  # linear model
  stat_smooth_eqn_lab(geom = "text", method = "lm",
                      xpos = 80, ypos = 20,
                      hjust = 0, parse = TRUE, family = font, size = 8)  +
  geom_smooth(method = "lm", colour = "darkgrey", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 140, 20)) +
  scale_y_continuous(name = expression(tau))

ExportPDF(plot_tau_vs_e0, "./out/tau_vs_e0.pdf",
          .width = 25, .height = 20)


plot_lbar_vs_e0sim <-
  ggplot(outsim, aes(x = e0, y = lbar)) +
  # main
  geom_point(alpha = 0.5, size = 1.8) +
  # linear model
  #stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                    xpos = 80, ypos = 0.2,
  #                    hjust = 0, parse = TRUE, family = font, size = 8)  +
  #geom_smooth(method = "lm", colour = "darkgrey", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 275, 25)) +
  scale_y_continuous(name = expression(bar(l))) +
  theme(axis.title.y = element_text(angle = 0))

ExportPDF(plot_lbar_vs_e0sim, "./out/lbar_vs_e0_sim.pdf",
          .width = 25, .height = 20)


# Simulation
plot_tau_vs_e0sim <-
  ggplot(outsim, aes(x = e0, y = tau)) +
  # main
  ggtitle(expression(paste("40,000 simulated Siler trajectories, 0 <= ", theta," <= 1"))) +
  geom_point(alpha = 0.5, size = 1.8) +
  # linear model
  stat_smooth_eqn_lab(geom = "text", method = "lm",
                      xpos =150, ypos = 15,
                      hjust = 0, parse = TRUE, family = font, size = 8)  +
  geom_smooth(method = "lm", colour = "darkred", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 275, 25)) +
  scale_y_continuous(name = expression(tau))

ExportPDF(plot_tau_vs_e0sim, "./out/tau_vs_e0_sim.pdf",
          .width = 25, .height = 20)
# zoomed in
plot_tau_vs_e0simZoom <-
  ggplot(outsim, aes(x = e0, y = tau)) +
  coord_cartesian(xlim=c(0, 80), ylim = c(0, 50)) +
  # main
  geom_point(alpha = 0.5, size = 1.8) +
  # linear model
  stat_smooth_eqn_lab(geom = "text", method = "lm",
                      xpos =55, ypos = 15,
                      hjust = 0, parse = TRUE, family = font, size = 8)  +
  geom_smooth(method = "lm", colour = "darkred", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10)) +
  scale_y_continuous(name = expression(tau))

ExportPDF(plot_tau_vs_e0simZoom, "./out/tau_vs_e0_simZoom.pdf",
          .width = 25, .height = 20)


plot_lbar_vs_e0sim <-
  ggplot(outsim, aes(x = e0, y = lbar)) +
  # main
  ggtitle(expression(paste("40,000 simulated Siler trajectories, 0 <= ", theta," <= 1"))) +
  geom_point(alpha = 0.5, size = 1.8) +
  # linear model
  #stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                    xpos = 80, ypos = 0.2,
  #                    hjust = 0, parse = TRUE, family = font, size = 8)  +
  #geom_smooth(method = "lm", colour = "darkgrey", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 275, 25)) +
  scale_y_continuous(name = expression(bar(l))) +
  theme(axis.title.y = element_text(angle = 0))

ExportPDF(plot_lbar_vs_e0sim, "./out/lbar_vs_e0_sim.pdf",
          .width = 25, .height = 20)

plot_Gini_vs_e0sim <-
  ggplot(outsim, aes(x = e0, y = Gini)) +
  # main
  ggtitle(expression(paste("40,000 simulated Siler trajectories, 0 <= ", theta," <= 1"))) +
  geom_point(alpha = 0.5, size = 1.8) +
  # linear model
  #stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                      xpos =150, ypos = 0.3,
  #                    hjust = 0, parse = TRUE, family = font, size = 8)  +
  #geom_smooth(method = "lm", colour = "darkred", size = 1.2) +
  # scale
  scale_x_continuous(name = x_lab, breaks = seq(0, 275, 25)) +
  scale_y_continuous(name = "Gini coefficient")

ExportPDF(plot_Gini_vs_e0sim, "./out/Gini_vs_e0_sim.pdf",
          .width = 25, .height = 20)
