#####################
## Produce PMP 
## plot
####################

library(tidyverse)
library(ggplot)

pmp <- data.frame(PMP = paste("PMP", 1:13),
                  ord = 1:13,
                  President = c(rep(1,4), 2, 3, 3, 1, 1, 2, 2, 3, 3),
                  LowerHouse = c(3, 3, rep(2, 5), rep(1, 6)),
                  UpperHouse = c(3, 0, 2, 0, 2, 2, 0, 1, 0, 1, 0, 1, 0))


pmp %>%
  gather(key=Branch, value = value, President:UpperHouse) %>%
  mutate(PMP = reorder(PMP, ord)) %>%
  mutate(ord_branch = rep(c(3, 1, 2), each=13)) %>%
  mutate(Branch = reorder(Branch, ord_branch)) %>%
  ggplot(aes(x=PMP, y = value, fill = Branch)) +
  scale_fill_grey(start = 0, end = 0.75) +
  geom_bar(stat='identity', position = "fill") +
  geom_text(aes(x=c(1), y = c(0.1), label = c("Weak")), col="white") +
  geom_text(aes(x=c(1), y = c(0.8), label = c("Except\nBudget")), col="white") +
  geom_text(aes(x=c(13), y = c(0.3), label = c("Proactive")), col="white") +
  geom_text(aes(x=c(13), y = c(0.85), label = c("Limited")), col="white") +
  geom_text(aes(x=c(5), y = c(0.2), label = c("Agenda\nSetting")), col="white") +
  geom_text(aes(x=c(5), y = c(0.5), label = c("Only\nBudget")), col="white") +
  geom_text(aes(x=c(5), y = c(0.8), label = c("Only\nBudget")), col="white") +
  geom_text(aes(x=c(8), y = c(0.2), label = c("Weak")), col="white") +
  geom_text(aes(x=c(8), y = c(0.5), label = c("Limited")), col="white") +
  geom_text(aes(x=c(8), y = c(0.8), label = c("Limited")), col="white") +
  xlab("Legislature-favoring <--- Parity ---> Executive-favoring") + 
  ylab("Share of Strength") +
  theme_bw()

