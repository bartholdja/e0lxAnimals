x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

#colMan <- gg_color_hue(n = 8)
#colMan <- sample(colMan)
colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF")

lbar_vs_e0 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar), data = out
             , alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("All observations, n = 105")
  

ExportPDF(list(lbar_vs_e0), "./out/lbar_vs_e0.pdf",
          .width = 25, .height = 20)

# class more than 3
indMore4ObsClass <- names(table(out$class))[which(table(out$class) >= 4)]

lbar_vs_e0_class <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar, colour = factor(class)), 
             data = filter(out, class %in% indMore4ObsClass), 
                           alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("> 3 observations per class, n = 81")

ExportPDF(list(lbar_vs_e0_class), "./out/lbar_vs_e0_class.pdf",
          .width = 25, .height = 20)

# class more than 6
indMore6ObsClass <- names(table(out$class))[which(table(out$class) >= 7)]

lbar_vs_e0_class6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar, colour = factor(class)), 
             data = filter(out, class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[c(1:2, 4:5)]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("> 6 observations per class, n = 77")

ExportPDF(list(lbar_vs_e0_class6), "./out/lbar_vs_e0_class6.pdf",
          .width = 25, .height = 20)


# order, more than 3
indMore4ObsOrder <- names(table(out$order))[which(table(out$order) >= 4)]
lbar_vs_e0_order <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar, colour = factor(order)), 
             data = filter(out, order %in% indMore4ObsOrder), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:8]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("> 3 observations per order, n = 53")

ExportPDF(list(lbar_vs_e0_order), "./out/lbar_vs_e0_order.pdf",
          .width = 25, .height = 20)

# order more than 6
indMore6ObsOrder <- names(table(out$order))[which(table(out$order) >= 7)]
lbar_vs_e0_order6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar, colour = factor(order)), 
             data = filter(out, order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[c(2:3,5, 8)]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("> 6 observations per order, n = 34")

ExportPDF(list(lbar_vs_e0_order6), "./out/lbar_vs_e0_order6.pdf",
          .width = 25, .height = 20)

# family, more than 3
indMore4ObsFamily <- names(table(out$family))[which(table(out$family) >= 4)]

lbar_vs_e0_family <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = lbar, colour = factor(family)), 
             data = filter(out, family %in% indMore4ObsFamily), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Family") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(name = expression(bar(l)), breaks = seq(0.2, 1, 0.1) , limits = c(0.2, 1)) +
  ggtitle("> 3 observations per family, n = 21")

ExportPDF(list(lbar_vs_e0_family), "./out/lbar_vs_e0_family.pdf",
          .width = 25, .height = 20)

ExportPDF(list(lbar_vs_e0, lbar_vs_e0_class,lbar_vs_e0_class6, lbar_vs_e0_order, lbar_vs_e0_order6, lbar_vs_e0_family), "./out/lbar_vs_e0_summary.pdf",
          .width = 25, .height = 20)


### plot gini thing instead

x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

#colMan <- gg_color_hue(n = 8)
#colMan <- sample(colMan)
colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF")

out %>%
  mutate(shape =1 - 2*(1-lbar)) -> out
out1 %>%
  mutate(shape =1 - 2*(1-lbar)) -> out1

shape_vs_e0 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape), data = out
             , alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_label_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations, n = 105, plus 6 human populations")


ExportPDF(list(shape_vs_e0), "./out/shape_vs_e0.pdf",
          .width = 25, .height = 20)

# class more than 3
indMore4ObsClass <- names(table(out$class))[which(table(out$class) >= 4)]

shape_vs_e0_class <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape, colour = factor(class)), 
             data = filter(out, class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_text_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 observations per class, n = 81, plus 6 human populations")

ExportPDF(list(shape_vs_e0_class), "./out/shape_vs_e0_class.pdf",
          .width = 25, .height = 20)

# class more than 6
indMore6ObsClass <- names(table(out$class))[which(table(out$class) >= 7)]

shape_vs_e0_class6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape, colour = factor(class)), 
             data = filter(out, class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_text_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[c(1:2, 4:5)]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 observations per class, n = 77, plus 6 human populations")

ExportPDF(list(shape_vs_e0_class6), "./out/shape_vs_e0_class6.pdf",
          .width = 25, .height = 20)


# order, more than 3
indMore4ObsOrder <- names(table(out$order))[which(table(out$order) >= 4)]
shape_vs_e0_order <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape, colour = factor(order)), 
             data = filter(out, order %in% indMore4ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_text_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:8]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 observations per order, n = 53, plus 6 human populations")

ExportPDF(list(shape_vs_e0_order), "./out/shape_vs_e0_order.pdf",
          .width = 25, .height = 20)

# order more than 6
indMore6ObsOrder <- names(table(out$order))[which(table(out$order) >= 7)]
shape_vs_e0_order6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape, colour = factor(order)), 
             data = filter(out, order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_text_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[c(2:3,5, 8)]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 observations per order, n = 34, plus 6 human populations")

ExportPDF(list(shape_vs_e0_order6), "./out/shape_vs_e0_order6.pdf",
          .width = 25, .height = 20)

# family, more than 3
indMore4ObsFamily <- names(table(out$family))[which(table(out$family) >= 4)]

shape_vs_e0_family <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape, colour = factor(family)), 
             data = filter(out, family %in% indMore4ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_point(mapping = aes(x = e0, y = shape), data = out1
             , alpha = 0.9, size = 3.8, shape = 17, color = "red") +
  geom_text_repel(mapping = aes(x = e0, y = shape), data = out1, label = out1$pop) +
  labs(colour = "Family") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 observations per family, n = 21, plus 6 human populations")

ExportPDF(list(shape_vs_e0_family), "./out/shape_vs_e0_family.pdf",
          .width = 25, .height = 20)

ExportPDF(list(shape_vs_e0, shape_vs_e0_class,shape_vs_e0_class6, shape_vs_e0_order, shape_vs_e0_order6, shape_vs_e0_family), "./out/shape_vs_e0_summary.pdf",
          .width = 25, .height = 20)
