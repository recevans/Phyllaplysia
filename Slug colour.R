# LIBRARY #####################################################################
library(car)
library(tidyverse)
library(lubridate)

# THEME #######################################################################
theme_326 <- theme_classic() + 
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, colour = "black"), 
    axis.text.x = element_text(margin = margin(t = 10, unit = "pt")), 
    axis.text.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 14), 
    strip.text = element_text(size = 14))

# DATA FRAMES #################################################################
colour <- read.csv("Slug colour.csv") %>%
  mutate(mean_deltaE = rowMeans(.[, 18:27])) %>%
  mutate(date = as.character(as.factor(date))) %>%
  group_by(substrate, slug, date)

zostera <- colour %>%
  filter(., substrate == "Zostera")

fucus <- colour %>%
  filter(., substrate == "Fucus")

pyropia <- colour %>%
  filter(., substrate == "Pyropia")

ulva <- colour %>%
  filter(., substrate == "Ulva")

# ANALYSES ####################################################################
## Overall ====================================================================
### deltaE --------------------------------------------------------------------
dE.anova <- aov(mean_deltaE ~ substrate + date, data = colour)
Anova(dE.anova, type = "III")
summary(dE.anova)
TukeyHSD(dE.anova)

### L -------------------------------------------------------------------------
L.anova <- aov(L ~ substrate + date, data = colour)
Anova(L.anova, type = "III")
summary(L.anova)
TukeyHSD(L.anova)

### a -------------------------------------------------------------------------
a.anova <- aov(a ~ substrate + date, data = colour)
Anova(a.anova, type = "III")
summary(a.anova)
TukeyHSD(a.anova)

### b -------------------------------------------------------------------------
b.anova <- aov(b ~ substrate + date, data = colour)
Anova(b.anova, type = "III")
summary(b.anova)
TukeyHSD(b.anova)

## Zostera ====================================================================
### deltaE --------------------------------------------------------------------
z.dE.anova <- aov(mean_deltaE ~ date, data = zostera)
Anova(z.dE.anova, type = "III")
summary(z.dE.anova)
TukeyHSD(z.dE.anova)

### L -------------------------------------------------------------------------
z.L.anova <- aov(L ~ date, data = zostera)
Anova(z.L.anova, type = "III")
summary(z.L.anova)
TukeyHSD(z.L.anova)

### a -------------------------------------------------------------------------
z.a.anova <- aov(a ~ date, data = zostera)
Anova(z.a.anova, type = "III")
summary(z.a.anova)
TukeyHSD(z.a.anova)

### b -------------------------------------------------------------------------
z.b.anova <- aov(b ~ date, data = zostera)
Anova(z.b.anova, type = "III")
summary(z.b.anova)
TukeyHSD(z.b.anova)

## Fucus ======================================================================
### deltaE --------------------------------------------------------------------
f.dE.anova <- aov(mean_deltaE ~ date, data = fucus)
Anova(f.dE.anova, type = "III")
summary(f.dE.anova)
TukeyHSD(f.dE.anova)

### L -------------------------------------------------------------------------
f.L.anova <- aov(L ~ date, data = fucus)
Anova(f.L.anova, type = "III")
summary(f.L.anova)
TukeyHSD(f.L.anova)

### a -------------------------------------------------------------------------
f.a.anova <- aov(a ~ date, data = fucus)
Anova(f.a.anova, type = "III")
summary(f.a.anova)
TukeyHSD(f.a.anova)

### b -------------------------------------------------------------------------
f.b.anova <- aov(b ~ date, data = fucus)
Anova(f.b.anova, type = "III")
summary(f.b.anova)
TukeyHSD(f.b.anova)

## Pyropia ====================================================================
### deltaE --------------------------------------------------------------------
p.dE.anova <- aov(mean_deltaE ~ date, data = pyropia)
Anova(p.dE.anova, type = "III")
summary(p.dE.anova)
TukeyHSD(p.dE.anova)

### L -------------------------------------------------------------------------
p.L.anova <- aov(L ~ date, data = pyropia)
Anova(p.L.anova, type = "III")
summary(p.L.anova)
TukeyHSD(p.L.anova)

### a -------------------------------------------------------------------------
p.a.anova <- aov(a ~ date, data = pyropia)
Anova(p.a.anova, type = "III")
summary(p.a.anova)
TukeyHSD(p.a.anova)

### b -------------------------------------------------------------------------
p.b.anova <- aov(b ~ date, data = pyropia)
Anova(p.b.anova, type = "III")
summary(p.b.anova)
TukeyHSD(p.b.anova)

## Ulva =======================================================================
### deltaE --------------------------------------------------------------------
u.dE.anova <- aov(mean_deltaE ~ date, data = ulva)
Anova(u.dE.anova, type = "III")
summary(u.dE.anova)
TukeyHSD(u.dE.anova)

### L -------------------------------------------------------------------------
u.L.anova <- aov(L ~ date, data = ulva)
Anova(u.L.anova, type = "III")
summary(u.L.anova)
TukeyHSD(u.L.anova)

### a -------------------------------------------------------------------------
u.a.anova <- aov(a ~ date, data = ulva)
Anova(u.a.anova, type = "III")
summary(u.a.anova)
TukeyHSD(u.a.anova)

### b -------------------------------------------------------------------------
u.b.anova <- aov(b ~ date, data = ulva)
Anova(u.b.anova, type = "III")
summary(u.b.anova)
TukeyHSD(u.b.anova)

# VISUALISATION ###############################################################
roc.plot <- colour %>%
  ggplot(aes(x = date, y = mean_deltaE, colour = slug)) +
  geom_line() +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "Overall rate of change plot", x = "Date", y = "Mean deltaE")
roc.plot

## Zostera ====================================================================
z.L.plot <- zostera %>%
  ggplot(aes(x = date, y = L)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "L values in Zostera", x = "Date", y = "L")
z.L.plot

z.a.plot <- zostera %>%
  ggplot(aes(x = date, y = a)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "a values in Zostera", x = "Date", y = "a")
z.a.plot

z.b.plot <- zostera %>%
  ggplot(aes(x = date, y = b)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "b values in Zostera", x = "Date", y = "b")
z.b.plot

## Fucus ======================================================================
f.L.plot <- fucus %>%
  ggplot(aes(x = date, y = L)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "L values in Fucus", x = "Date", y = "L")
f.L.plot

f.a.plot <- fucus %>%
  ggplot(aes(x = date, y = a)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "a values in Fucus", x = "Date", y = "a")
f.a.plot

f.b.plot <- fucus %>%
  ggplot(aes(x = date, y = b)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "b values in Fucus", x = "Date", y = "b")
f.b.plot

## Pyropia ====================================================================
p.L.plot <- pyropia %>%
  ggplot(aes(x = date, y = L)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "L values in Pyropia", x = "Date", y = "L")
p.L.plot

p.a.plot <- pyropia %>%
  ggplot(aes(x = date, y = a)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "a values in Pyropia", x = "Date", y = "a")
p.a.plot

p.b.plot <- pyropia %>%
  ggplot(aes(x = date, y = b)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "b values in Pyropia", x = "Date", y = "b")
p.b.plot

## Ulva =======================================================================
u.L.plot <- ulva %>%
  ggplot(aes(x = date, y = L)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "L values in Ulva", x = "Date", y = "L")
u.L.plot

u.a.plot <- ulva %>%
  ggplot(aes(x = date, y = a)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "a values in Ulva", x = "Date", y = "a")
u.a.plot

u.b.plot <- ulva %>%
  ggplot(aes(x = date, y = b)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap("slug") +
  theme_326 +
  labs(title = "b values in Ulva", x = "Date", y = "b")
u.b.plot