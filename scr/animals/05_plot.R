### plot gini thing instead
rm(out)
out <- readRDS("data/outAll.rds")

x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

#colMan <- gg_color_hue(n = 8)
#colMan <- sample(colMan)
colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF", "#89fdc5")

labHumans <- out %>% filter(SpeciesAccepted == "Homo_sapiens")  %>% select(CommonName)  %>%
  unlist()

# all data points
shape_vs_e0_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0), data = out,
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = e0, y = shape0), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens"), label = labHumans) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations incl. repeated obs. on same species, n = 202, plus 6 human populations")

shape_vs_e0 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0), data = (filter(out, singleObs == 1)),
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = e0, y = shape0), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens" & singleObs == 1), label = "Modern Japan") +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations excl. repeated obs. on same species, n = 107, plus 1 human population")


# class more than 3
outSingleObs <- filter(out, singleObs == 1)
indMore4ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 4)]

shape_vs_e0_class_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(out, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                   data = filter(out, Class %in% indMore4ObsClass & SpeciesAccepted == "Homo_sapiens"),
                   label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 165, plus 6 human populations")

shape_vs_e0_class <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 88, plus 1 human population")


# class more than 6
indMore6ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 7)]

shape_vs_e0_class6_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(out, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Class %in% indMore6ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, incl. repeated obs. on species, n = 161, plus 6 human populations")

shape_vs_e0_class6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, excl. repeated obs. on species, n = 83, plus 1 human population")


# order, more than 3
indMore3ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 4)]

shape_vs_e0_order_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(out, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Order %in% indMore3ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, incl. repeated obs. on species, n = 107, plus 6 human populations")

shape_vs_e0_order <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, excl. repeated obs. on species, n = 61, plus 1 human population")


# order more than 6
indMore6ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 7)]

shape_vs_e0_order6_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(out, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Order %in% indMore6ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, incl. repeated obs. on species, n = 67, plus 6 human populations")

shape_vs_e0_order6 <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, excl. repeated obs. on species, n = 37, plus 1 human population")


# family, more than 3
indMore3ObsFamily <- names(table(outSingleObs$Family))[which(table(outSingleObs$Family) >= 4)]
indMore3ObsFamily <- c(indMore3ObsFamily, "Hominidae")

shape_vs_e0_family_All <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Family)), 
             data = filter(out, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Family %in% indMore3ObsFamily & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Family") +
  scale_color_manual(values = colMan[-4]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, incl. repeated obs. on species, n = 47, plus 6 human populations")

shape_vs_e0_family <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Family)), 
             data = filter(outSingleObs, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Family") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, excl. repeated obs. on species, n = 26, plus 1 human population")


ExportPDF(list(shape_vs_e0, shape_vs_e0_class,shape_vs_e0_class6, 
               shape_vs_e0_order, shape_vs_e0_order6, 
               shape_vs_e0_family), "./out/shape0_vs_e0_summary_SingleObs_Birth.pdf",
          .width = 25, .height = 20)

ExportPDF(list(shape_vs_e0_All, shape_vs_e0_class_All,shape_vs_e0_class6_All, shape_vs_e0_order_All,
               shape_vs_e0_order6_All, shape_vs_e0_family_All), "./out/shape0_vs_e0_summary_All_Birth.pdf",
          .width = 25, .height = 20)
###############################################################################################
###############################################################################################
# without humans

### plot gini thing instead
rm(out, outSingleObs)
out <- readRDS("data/outAll.rds")

x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF", "#89fdc5")
 
out <- filter(out, SpeciesAccepted != "Homo_sapiens")
outSingleObs <- filter(out, singleObs == 1)
outSingleObs <- filter(outSingleObs, SpeciesAccepted != "Homo_sapiens")

# all data points
shape_vs_e0_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0), data = out,
             alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations incl. repeated obs. on same species, n = 202")

shape_vs_e0_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0), data = (filter(out, singleObs == 1)),
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = e0, y = shape0), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens" & singleObs == 1), label = "Modern Japan") +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations excl. repeated obs. on same species, n = 107")

# class more than 3
indMore4ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 4)]

shape_vs_e0_class_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(out, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Class %in% indMore4ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 165")

shape_vs_e0_class_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, excl. repeated obs. on species, n = 87")


# class more than 6
indMore6ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 7)]

shape_vs_e0_class6_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(out, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Class %in% indMore6ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, incl. repeated obs. on species, n = 161")

shape_vs_e0_class6_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, excl. repeated obs. on species, n = 83")


# order, more than 3
indMore3ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 4)]

shape_vs_e0_order_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(out, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Order %in% indMore3ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, incl. repeated obs. on species, n = 107")

shape_vs_e0_order_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, excl. repeated obs. on species, n = 61")

# order more than 6
indMore6ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 7)]

shape_vs_e0_order6_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(out, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Order %in% indMore6ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, incl. repeated obs. on species, n = 67")

shape_vs_e0_order6_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, excl. repeated obs. on species, n = 37")

# family, more than 3
indMore3ObsFamily <- names(table(outSingleObs$Family))[which(table(outSingleObs$Family) >= 4)]

shape_vs_e0_family_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Family)), 
             data = filter(out, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(out, Family %in% indMore3ObsFamily & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Family") +
  scale_color_manual(values = colMan[-4]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, incl. repeated obs. on species, n = 47")

shape_vs_e0_family_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = e0, y = shape0, colour = factor(Family)), 
             data = filter(outSingleObs, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = e0, y = shape0), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Family") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 50, 10), limits = c(0, 50)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, excl. repeated obs. on species, n = 26")


ExportPDF(list(shape_vs_e0_noHum, shape_vs_e0_class_noHum,shape_vs_e0_class6_noHum, 
               shape_vs_e0_order_noHum, shape_vs_e0_order6_noHum, 
               shape_vs_e0_family_noHum), "./out/shape0_vs_e0_summary_SingleObs_noHum_Birth.pdf",
          .width = 25, .height = 20)

ExportPDF(list(shape_vs_e0_All_noHum, shape_vs_e0_class_All_noHum,shape_vs_e0_class6_All_noHum,
               shape_vs_e0_order_All_noHum,
               shape_vs_e0_order6_All_noHum, shape_vs_e0_family_All_noHum), 
          "./out/shape0_vs_e0_summary_All_noHum_Birth.pdf",
          .width = 25, .height = 20)
###############################################################################################
###############################################################################################
# from maturity
### plot gini thing instead
rm(out, outSingleObs)
out <- readRDS("data/outAll.rds")
out <- filter(out, !is.na(eMat))

x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

#colMan <- gg_color_hue(n = 8)
#colMan <- sample(colMan)
colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF", "#89fdc5")

labHumans <- out %>% filter(SpeciesAccepted == "Homo_sapiens")  %>% select(CommonName)  %>%
  unlist()

# all data points
shape_vs_eMat_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat), data = out,
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = eMat, y = shapeMat), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens"), label = labHumans) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations incl. repeated obs. on same species, n = 163, plus 6 human populations")

shape_vs_eMat <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat), data = (filter(out, singleObs == 1)),
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = eMat, y = shapeMat), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens" & singleObs == 1), label = "Modern Japan") +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations excl. repeated obs. on same species, n = 91, plus 1 human population")


# class more than 3
outSingleObs <- filter(out, singleObs == 1)
indMore4ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 4)]

shape_vs_eMat_class_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(out, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Class %in% indMore4ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 128, plus 6 human populations")

shape_vs_eMat_class <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 72, plus 1 human population")


# class more than 6
indMore6ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 7)]

shape_vs_eMat_class6_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(out, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Class %in% indMore6ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, incl. repeated obs. on species, n = 127, plus 6 human populations")

shape_vs_eMat_class6 <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, excl. repeated obs. on species, n = 68, plus 1 human population")


# order, more than 3
indMore3ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 4)]

shape_vs_eMat_order_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(out, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Order %in% indMore3ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, incl. repeated obs. on species, n = 89, plus 6 human populations")

shape_vs_eMat_order <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, excl. repeated obs. on species, n = 51, plus 1 human population")


# order more than 6
indMore6ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 7)]

shape_vs_eMat_order6_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(out, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Order %in% indMore6ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, incl. repeated obs. on species, n = 57, plus 6 human populations")

shape_vs_eMat_order6 <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, excl. repeated obs. on species, n = 33, plus 1 human population")


# family, more than 3
indMore3ObsFamily <- names(table(outSingleObs$Family))[which(table(outSingleObs$Family) >= 4)]
indMore3ObsFamily <- c(indMore3ObsFamily, "Hominidae")

shape_vs_eMat_family_All <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Family)), 
             data = filter(out, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Family %in% indMore3ObsFamily & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Family") +
  scale_color_manual(values = colMan[-4]) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, incl. repeated obs. on species, n = 37, plus 6 human populations")

shape_vs_eMat_family <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Family)), 
             data = filter(outSingleObs, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Family") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, excl. repeated obs. on species, n = 22, plus 1 human population")


ExportPDF(list(shape_vs_eMat, shape_vs_eMat_class,shape_vs_eMat_class6, 
               shape_vs_eMat_order, shape_vs_eMat_order6, 
               shape_vs_eMat_family), "./out/shapeMat_vs_eMat_summary_SingleObs_Maturity.pdf",
          .width = 25, .height = 20)

ExportPDF(list(shape_vs_eMat_All, shape_vs_eMat_class_All,shape_vs_eMat_class6_All, shape_vs_eMat_order_All,
               shape_vs_eMat_order6_All, shape_vs_eMat_family_All), "./out/shapeMat_vs_eMat_summary_All_Maturity.pdf",
          .width = 25, .height = 20)
###############################################################################################
###############################################################################################
# without humans

### plot gini thing instead
rm(out, outSingleObs)
out <- readRDS("data/outAll.rds")
out <- filter(out, !is.na(eMat))

x_lab    <- expression(italic(e[0]))
font     <- "Palatino"

colMan <- c("#C77CFF", "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600", "#F8766D", "#00BE67", "#00A9FF", "#89fdc5")

out <- filter(out, SpeciesAccepted != "Homo_sapiens")
outSingleObs <- filter(out, singleObs == 1)
outSingleObs <- filter(outSingleObs, SpeciesAccepted != "Homo_sapiens")

# all data points
shape_vs_eMat_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat), data = out,
             alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations incl. repeated obs. on same species, n = 163")

shape_vs_eMat_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat), data = (filter(out, singleObs == 1)),
             alpha = 0.9, size = 3.8) +
  geom_label_repel(mapping = aes(x = eMat, y = shapeMat), 
                   data = filter(out, SpeciesAccepted == "Homo_sapiens" & singleObs == 1), label = "Modern Japan") +
  labs(colour = "Class") +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("All observations excl. repeated obs. on same species, n = 91")

# class more than 3
indMore4ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 4)]

shape_vs_eMat_class_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(out, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Class %in% indMore4ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, incl. repeated obs. on species, n = 128")

shape_vs_eMat_class_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore4ObsClass), 
             alpha = 0.9, size = 3.8) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per class, excl. repeated obs. on species, n = 72")


# class more than 6
indMore6ObsClass <- names(table(outSingleObs$Class))[which(table(outSingleObs$Class) >= 7)]

shape_vs_eMat_class6_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(out, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Class %in% indMore6ObsClass & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, incl. repeated obs. on species, n = 121")

shape_vs_eMat_class6_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Class)), 
             data = filter(outSingleObs, Class %in% indMore6ObsClass), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Class") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per class, excl. repeated obs. on species, n = 68")


# order, more than 3
indMore3ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 4)]

shape_vs_eMat_order_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(out, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Order %in% indMore3ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, incl. repeated obs. on species, n = 89")

shape_vs_eMat_order_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore3ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per order, excl. repeated obs. on species, n = 51")

# order more than 6
indMore6ObsOrder <- names(table(outSingleObs$Order))[which(table(outSingleObs$Order) >= 7)]

shape_vs_eMat_order6_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(out, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Order %in% indMore6ObsOrder & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, incl. repeated obs. on species, n = 35")

shape_vs_eMat_order6_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Order)), 
             data = filter(outSingleObs, Order %in% indMore6ObsOrder), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Order") +
  scale_color_manual(values = colMan[1:5]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 6 species observations per order, excl. repeated obs. on species, n = 22")

# family, more than 3
indMore3ObsFamily <- names(table(outSingleObs$Family))[which(table(outSingleObs$Family) >= 4)]

shape_vs_eMat_family_All_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Family)), 
             data = filter(out, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(out, Family %in% indMore3ObsFamily & SpeciesAccepted == "Homo_sapiens"),
                  label = labHumans) +
  labs(colour = "Family") +
  scale_color_manual(values = colMan[-4]) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, incl. repeated obs. on species, n = 36")

shape_vs_eMat_family_noHum <-
  ggplot() +
  geom_point(mapping = aes(x = eMat, y = shapeMat, colour = factor(Family)), 
             data = filter(outSingleObs, Family %in% indMore3ObsFamily), 
             alpha = 0.9, size = 3.8) +
  geom_text_repel(mapping = aes(x = eMat, y = shapeMat), 
                  data = filter(outSingleObs, SpeciesAccepted == "Homo_sapiens"),
                  label = "Modern Japan") +
  labs(colour = "Family") +
  scale_color_manual(values = c(colMan, "darkbrown")) +
  scale_x_continuous(name = x_lab, breaks = c(0, 50), limits = c(0, 60)) +
  scale_y_continuous(name = "shape measure") +
  ggtitle("> 3 species observations per family, excl. repeated obs. on species, n = 21")


ExportPDF(list(shape_vs_eMat_noHum, shape_vs_eMat_class_noHum,shape_vs_eMat_class6_noHum, 
               shape_vs_eMat_order_noHum, shape_vs_eMat_order6_noHum, 
               shape_vs_eMat_family_noHum), "./out/shapeMat_vs_eMat_summary_SingleObs_noHum_Maturity.pdf",
          .width = 25, .height = 20)

ExportPDF(list(shape_vs_eMat_All_noHum, shape_vs_eMat_class_All_noHum,shape_vs_eMat_class6_All_noHum,
               shape_vs_eMat_order_All_noHum,
               shape_vs_eMat_order6_All_noHum, shape_vs_eMat_family_All_noHum), 
          "./out/shapeMat_vs_eMat_summary_All_noHum_Maturity.pdf",
          .width = 25, .height = 20)