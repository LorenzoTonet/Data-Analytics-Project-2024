
library(patchwork)
library(ggplot2)
library(ggExtra)
library(moments)
library(sf)
library(rnaturalearth)
library(dplyr)
library(corrplot)
library(MASS)

set.seed(1235)

verde_acqua = "#64F9C7"
giallo = "#FFFF00"
arancione = "#FFB600"
verde_acqua_scurito = "#3da07f"
arancione_scurito = "#D69A01"
black ="#000000"

col_map = c("1" = "#64F9C7", "2" = "#FFFF00", "3" = "#FFB600", "4" = "#ee6055", "5" = "#57f054", "6" = "#c73596", "7" = "#a26769", "8" = "#7ca5b8")

data = read.csv("life expectation.csv", header = TRUE)
original_data = data
summary(data)

data$COUNTRY = factor(data$COUNTRY)

data$REGION = factor(data$REGION)



country_na_counts = original_data %>%
  group_by(COUNTRY) %>%
  summarise(across(everything(), ~sum(is.na(.))))


country_na_counts = original_data %>%
  group_by(original_data$COUNTRY) %>%
  summarise(Total_NA = sum(rowSums(is.na(across(-COUNTRY)))))

country_na_counts = country_na_counts[order(country_na_counts$Total_NA, decreasing=TRUE), ]

sistema = function(x) gsub("\xa0", " ", x, useBytes = TRUE)
leva_parentesi = function(x) gsub("\\(.*?\\)", "", x)
data$COUNTRY = as.vector(sapply(data$COUNTRY, sistema))
data$COUNTRY[data$COUNTRY == "C\xf4te d'Ivoire" ] = "Côte d'Ivoire"
data$COUNTRY = as.vector(sapply(data$COUNTRY, leva_parentesi))

world = ne_countries(scale = "medium", returnclass = "sf")
countries_regions = data.frame(
  country = data$COUNTRY, 
  region = data$REGION
)
world_regions = world %>%
  left_join(countries_regions, by = c("name_long" = "country"))

ggplot(data = world_regions) +
  geom_sf(aes(fill = region), color = "white", lwd = 0.2) +
  scale_fill_manual(values = col_map) +
  labs(title = "Regioni", fill = "Legenda")

ggplot(data, aes(x = REGION)) + 
  geom_bar(fill = giallo, color = "black") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  ggtitle("REGION COUNT")

p1 =ggplot(data, aes(x = LIFEEXP, y = ..density..)) + 
  geom_histogram(bins = sturges_k + 1 , fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  ggtitle("Life expectancy histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)+
  xlim(c(40, 80))

p2 = ggplot(data, aes(x = LIFEEXP)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Life expectancy (years)") +
  xlim(c(40, 80))

p1 + p2 + plot_layout(heights = c(6, 1))

skewness(data$LIFEEXP)

p1 = ggplot(data, aes(x = ILLITERATE, y = ..density..)) + 
  geom_histogram(bins = sturges_k +1, fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  ggtitle("Illiterate histogram") +
  xlim(c(0, 80)) +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = ILLITERATE)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Illiterate rate (%)") +
  xlim(c(0, 80))

p1 + p2 + plot_layout(heights = c(6, 1))


skewness(data$ILLITERATE, na.rm = T)


p1 = ggplot(data, aes(x = POP, y = after_stat(density))) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  ggtitle("Population histogram") +
  xlim(c(0, 250)) +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = POP)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Population (millions)") +
  xlim(c(0, 250))

p1 + p2 + plot_layout(heights = c(6, 1))

p1 = ggplot(data, aes(x = FERTILITY, y = ..density..)) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  ggtitle("Fertility histogram") +
  xlim(c(0, 10)) +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = FERTILITY)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Fertility rate (birth per woman)") +
  xlim(c(0, 10))

p1 + p2 + plot_layout(heights = c(6, 1))

skewness(data$FERTILITY, na.rm = T)


p1 = ggplot(data, aes(x = PRIVATEHEALTH, y = ..density..)) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  xlim(c(0, 9)) +
  ggtitle("Private health expediture histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = PRIVATEHEALTH)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(0, 9)) +
  xlab("Private health expediture (% of GDP)") 

p1 + p2 + plot_layout(heights = c(6, 1))

p1 = ggplot(data, aes(x = PRIVATEHEALTH, y = ..density..)) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  xlim(c(0, 6)) +
  ggtitle("Private health expediture histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = PRIVATEHEALTH)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(0, 6)) +
  xlab("Private health expediture (% of GDP)") 

p1 + p2 + plot_layout(heights = c(6, 1))

qqnorm(data$PRIVATEHEALTH[data$PRIVATEHEALTH < 8], col = arancione)
qqline(data$PRIVATEHEALTH[data$PRIVATEHEALTH < 8], col = verde_acqua)

plot(ecdf(data$PRIVATEHEALTH[data$PRIVATEHEALTH < 8]), col = arancione, main = "Funzione di ripartizione empirica")
m = mean(na.omit(data$PRIVATEHEALTH[data$PRIVATEHEALTH < 8]))
s = sd(na.omit(data$PRIVATEHEALTH[data$PRIVATEHEALTH < 8]))
curve(pnorm(x, mean = m, sd = s), add = TRUE, col = verde_acqua, lwd = 2)

p1 = ggplot(data, aes(x = PUBLICEDUCATION, y = ..density..)) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  xlim(c(0, 15)) +
  ggtitle("Public education histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = PUBLICEDUCATION)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(0, 15)) +
  xlab("Public expenditure on education (% of GDP)") 

p1 + p2 + plot_layout(heights = c(6, 1))

qqnorm(data$PUBLICEDUCATION[data$PUBLICEDUCATION < 10], col = arancione)
qqline(data$PUBLICEDUCATION[data$PUBLICEDUCATION < 10], col = verde_acqua)

plot(ecdf(data$PUBLICEDUCATION[data$PUBLICEDUCATION < 10]), col = arancione, main = "Funzione di ripartizione empirica")
m = mean(na.omit(data$PUBLICEDUCATION[data$PUBLICEDUCATION < 10]))
s = sd(na.omit(data$PUBLICEDUCATION[data$PUBLICEDUCATION < 10]))
curve(pnorm(x, mean = m, sd = s), add = TRUE, col = verde_acqua, lwd = 2)

p1 = ggplot(data, aes(x = HEALTHEXPEND, y = ..density..)) + 
  geom_histogram(fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  xlim(c(0, 6500)) +
  ggtitle("Health expenditure per capita histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = HEALTHEXPEND)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(0, 6500)) +
  xlab(" Health expenditure per capita (PPP in USD)") 

p1 + p2 + plot_layout(heights = c(6, 1))

skewness(data$HEALTHEXPEND, na.rm = T)

p1 = ggplot(data, aes(x = BIRTHATTEND, y = ..density..)) + 
  geom_histogram(bins = 11,fill = giallo, color = "black", boundary = 0) +
  xlab("") +
  xlim(c(0,100)) +
  ggtitle("Birth attended by skilled health personnel histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(data, aes(x = BIRTHATTEND)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(0, 100)) +
  xlab("% of birth attended by skilled health personnel") 

p1 + p2 + plot_layout(heights = c(6, 1))

p_na = sum(is.na(data$PHYSICIAN))/length(data$PHYSICIAN)

 
ph = na.omit(data$PHYSICIAN)

p1 = ggplot(NULL, aes(x = ph, y = after_stat(density))) + 
  geom_histogram(bins = 25, fill = giallo, color = "black") +
  xlab("") +
  ggtitle("Physician histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = ph)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Physician rate (%)") 

p1 + p2 + plot_layout(heights = c(6, 1))


p_na = sum(is.na(data$SMOKING))/length(data$SMOKING)

sm = na.omit(data$SMOKING)

idx = skewness(sm)

p1 = ggplot(NULL, aes(x = sm, y = after_stat(density))) + 
  geom_histogram(bins = 20, fill = giallo, color = "black") +
  xlab("") +
  ggtitle("Smoking histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = sm)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Smoking rate (%)") 

p1 + p2 + plot_layout(heights = c(6, 1))

p_na = sum(is.na(data$RESEARCHERS))/length(data$RESEARCHERS)


rs = na.omit(data$RESEARCHERS)

p1 = ggplot(NULL, aes(x = rs, y = after_stat(density))) + 
  geom_histogram(bins = 11, fill = giallo, color = "black") +
  xlab("") +
  ggtitle("Researchers histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = rs)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Researchers rate (%)") 

p1 + p2 + plot_layout(heights = c(6, 1))


rsl = rs[rs<5000]
p1 = ggplot(NULL, aes(x = rsl, y = after_stat(density))) + 
  geom_histogram(bins = 30, fill = giallo, color = "black") +
  xlab("") +
  ggtitle("Researchers <5000 histogram") +
  xlim(c(0, 5000)) + 
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = rsl)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("Researchers <5000 rate (%)") +
  xlim(c(0, 5000))

p1 + p2 + plot_layout(heights = c(6, 1))

p_na = sum(is.na(data$GDP))/length(data$GDP)

c_na = sum(is.na(data$GDP))
c_na

gdp = na.omit(data$GDP)

p1 = ggplot(NULL, aes(x = gdp, y = after_stat(density))) + 
  geom_histogram(bins = 11, fill = giallo, color = "black") +
  xlab("") +
  ggtitle("GDP histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = gdp)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("GDP rate (%)") 

p1 + p2 + plot_layout(heights = c(6, 1))

gdpl = gdp[gdp<250]

p1 = ggplot(NULL, aes(x = gdpl, y = after_stat(density))) + 
  geom_histogram(bins = 25, fill = giallo, color = "black") +
  xlab("") +
  xlim(c(0, 250)) + 
  ggtitle("GDP 0-250 histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = gdpl)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlab("GDP 0 - 250 rate (%)") + 
  xlim(c(0, 250))

p1 + p2 + plot_layout(heights = c(6, 1))


p_na = sum(is.na(data$FEMALEBOSS))/length(data$FEMALEBOSS)
p_na

fb = na.omit(data$FEMALEBOSS)
summary(fb)

idx = skewness(fb)

p1 = ggplot(NULL, aes(x = fb, y = after_stat(density))) + 
  geom_histogram(bins = 9, fill = giallo, color = "black") +
  xlab("") +
  xlim(c(-2, 62)) +
  ggtitle("FemaleBoss histogram") +
  geom_density(fill = verde_acqua, color = verde_acqua, alpha = 0.25, lwd = 2)

p2 = ggplot(NULL, aes(x = fb)) +
  geom_boxplot(fill = arancione, color = "black") +
  xlim(c(-2, 62)) +
  xlab("FemaleBoss rate (%)") 

p1 + p2 + plot_layout(heights = c(6, 1))

 
  
data = data[,  !(names(data) %in% c("COUNTRY", "SMOKING", "RESEARCHERS", "FEMALEBOSS"))]

numeric_vars = data[, sapply(data, is.numeric)]
cm = cor(numeric_vars, method = "pearson", use = "pairwise.complete.obs")

corrplot(cm,tl.col = 'black', method = 'square', col = colorRampPalette(c(verde_acqua_scurito, "white", arancione_scurito))(200))
correlations = sapply(numeric_vars, function(x) cor(numeric_vars$LIFEEXP, x, use = "pairwise.complete.obs", method = "pearson"))

#togliamo lifeexp
correlations = correlations[-1]
correlations = sort(correlations)

df = data.frame(x = factor(names(correlations), levels = names(correlations)), y = correlations)
ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = verde_acqua, color = "black") +
  geom_text(aes(label = round(y, 2), y = y + 0.13), vjust = 1, size = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Correlation with life expectancy")

rg = data$REGION

ggplot(NULL, aes(x = rg, y = data$LIFEEXP, fill = rg)) + 
  geom_violin(width = 1.6) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  scale_fill_manual(values  = col_map) +
  labs(title = "Regioni", fill = "Legenda", x = "Regione", y = "Life Expectation")

test = aov(data$LIFEEXP~rg)

ggplot(data, aes(x=ILLITERATE, y=LIFEEXP)) + 
  geom_point()

model = lm(LIFEEXP ~ ILLITERATE, data = data)
ggplot(data, aes(x=ILLITERATE, y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, fill = verde_acqua, color = arancione)

rs1 = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs1, aes(x = Fitted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = arancione, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

rs2 = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs2, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(color = arancione) +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

ggplot(data, aes(x=log(HEALTHEXPEND), y=LIFEEXP)) + 
  geom_point()

corr_1 = cor(data$HEALTHEXPEND, data$LIFEEXP, use = "pairwise.complete.obs")
corr_2 = cor(log(data$HEALTHEXPEND), data$LIFEEXP, use = "pairwise.complete.obs")

g1 = ggplot(data, aes(x=HEALTHEXPEND, y=LIFEEXP, color=REGION)) + 
  geom_point() +
  scale_color_manual(values = col_map) +
  theme(legend.position = "none")
g2 = ggplot(data, aes(x=log(HEALTHEXPEND), y=LIFEEXP, color=REGION)) + 
  geom_point() +
  scale_color_manual(values = col_map)+
  ylab("")
g1 + g2 + plot_layout()

ggplot(data, aes(x=log(HEALTHEXPEND), y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(aes(colour=REGION), se=F) +
  scale_color_manual(values = col_map) +
  facet_wrap(~ REGION)

model = lm(LIFEEXP ~ log(HEALTHEXPEND), data = data)
intercept = coef(model)[1]
slope = coef(model)[2]

ggplot(data, aes(x=log(HEALTHEXPEND), y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, fill = verde_acqua, color = arancione)

rs = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs, aes(x = Fitted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = arancione, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

rs = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(col=arancione) +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

ggplot(data, aes(x=FERTILITY, y=LIFEEXP)) + 
  geom_point()

ggplot(data, aes(x=FERTILITY, y=LIFEEXP, col = REGION)) + 
  geom_point() +
  scale_color_manual(values = col_map)

ggplot(data, aes(x=FERTILITY, y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(aes(colour=REGION), se=F) +
  scale_color_manual(values = col_map) +
  facet_wrap(~ REGION) 
  
model = lm(LIFEEXP ~ FERTILITY, data = data)
ggplot(data, aes(x=FERTILITY, y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, fill = verde_acqua, color = arancione)

rs = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs, aes(x = Fitted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, col =arancione, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

rs = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(col=arancione) +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

rs = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs, aes(x = Fitted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

rs = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(color = arancione) +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

ggplot(data, aes(x=BIRTHATTEND, y=LIFEEXP)) + 
  geom_point()

ggplot(data, aes(x=BIRTHATTEND, y=LIFEEXP, col = REGION)) + 
  geom_point()+
  scale_color_manual(values = col_map)

model = lm(LIFEEXP ~ BIRTHATTEND, data = data)
ggplot(data, aes(x=BIRTHATTEND, y=LIFEEXP)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, fill = verde_acqua, color = arancione)

rs = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs, aes(x = Fitted, y = Residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = arancione, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

rs = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(col=arancione) +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

ggplot(data, aes(x=GDP, y=LIFEEXP)) + 
  geom_point()

x = data$GDP[data$GDP>70 & data$GDP<4000]
y = data$LIFEEXP[data$GDP>70 & data$GDP<4000]
ggplot(NULL, aes(x=x, y=y)) + 
  geom_point() +
  labs(title = "GDP into LIFEEXP", x = "GDP", y = "LIFEEXP")

ggplot(data, aes(x=GDP, y=LIFEEXP, color=REGION)) + 
  geom_point() +
  scale_color_manual(values = col_map)


ggplot(data, aes(x=PHYSICIAN, y=LIFEEXP)) + 
  geom_point()

ggplot(data, aes(x=PHYSICIAN, y=LIFEEXP)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE, fill = verde_acqua, color = arancione) +
  labs(title = "y ~ x")

model = lm(LIFEEXP~PHYSICIAN, data)
summary(model)

par(mfrow = c(2, 2))
rs1 = data.frame(Fitted = fitted(model), Residuals = residuals(model))
ggplot(rs1, aes(x = Fitted, y = Residuals)) +
  geom_point(col=black) + 
  geom_hline(yintercept = 0, col = verde_acqua, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") 

rs2 = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs2, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(color = arancione) +
  labs(title = "QQ SResiduals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") 


ggplot(data, aes(x=log(PHYSICIAN), y=LIFEEXP)) + 
  geom_point(col=black)

ggplot(data, aes(x=log(PHYSICIAN), y=LIFEEXP)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE, fill = verde_acqua, color = arancione)+
  labs(title = "y ~ x")

res = cor(log(data$PHYSICIAN), data$LIFEEXP, method = "pearson", use = "pairwise.complete.obs")

model = lm(LIFEEXP~log(PHYSICIAN) , data)
summary(model)

model = lm(LIFEEXP~PHYSICIAN + I(PHYSICIAN^2) , data)
summary(model)

ggplot(data, aes(x=PHYSICIAN, y=LIFEEXP)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),  se=TRUE, fill = verde_acqua, color = arancione)+
  labs(title = "y ~ x + x^2")

rs1 = data.frame(StandardizedResiduals = rstandard(model))
ggplot(rs1, aes(sample = StandardizedResiduals)) +
  stat_qq() + 
  stat_qq_line(color = arancione) +
  labs(title = "QQ SResiduals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") 

model <- lm(LIFEEXP ~ FERTILITY + log(PHYSICIAN) + log(HEALTHEXPEND), data = data)
summary(model)

model <- lm(LIFEEXP ~ FERTILITY + log(PHYSICIAN) + log(HEALTHEXPEND) + REGION, data = data)
summary(model)
AIC(model)

clean_data = na.omit(data)
nrow(clean_data)

null_model = lm(LIFEEXP ~ 1, clean_data)

full_model = lm(LIFEEXP ~ ., clean_data)

best_model = stepAIC(null_model, trace = 0, scope = list(lower = null_model, upper = full_model), direction = "both")

summary(best_model)
AIC(best_model)



model = lm(LIFEEXP ~ REGION + BIRTHATTEND + PUBLICEDUCATION + 
             ILLITERATE + FERTILITY + HEALTHEXPEND + log(PHYSICIAN) + log(HEALTHEXPEND), clean_data)

summary(model)
AIC(model)

model = lm(LIFEEXP ~ REGION + BIRTHATTEND + PUBLICEDUCATION + 
    ILLITERATE + FERTILITY + log(PHYSICIAN) + log(HEALTHEXPEND) +  I(PHYSICIAN^3), clean_data)

summary(model)
AIC(model)

t = read.csv("life expectation.csv", header = TRUE)

clean_t = na.omit(t)
nrow(clean_t)
clean_t = clean_t[, !(names(clean_t) %in% c("COUNTRY"))]
null_model = lm(LIFEEXP ~ 1, clean_t)

full_model = lm(LIFEEXP ~ ., clean_t)

best_model = stepAIC(null_model, scope = list(lower = null_model, upper = full_model), direction = "both", trace = 0)

summary(best_model)

pairs(numeric_vars, na.action = na.omit)

ddd = original_data[c("COUNTRY","BIRTHATTEND", "PUBLICEDUCATION", "ILLITERATE", "FERTILITY", "HEALTHEXPEND", "PHYSICIAN", "GDP", "LIFEEXP")]
ddd = na.omit(ddd)

data_pca = ddd[, c("BIRTHATTEND", "PUBLICEDUCATION", "ILLITERATE", "FERTILITY", "HEALTHEXPEND", "PHYSICIAN", "GDP", "LIFEEXP")]

pca = prcomp(data_pca, scale = TRUE)
# barplot della % di varianza spiegata dalle componenti principali con ggplot
varianze = pca$sdev^2
varianze_spieg = varianze/sum(varianze)
varianze_spieg = round(varianze_spieg*100, 2)
ggplot(NULL, aes(x = c(1:8), y = varianze_spieg)) + 
  geom_bar(stat = "identity", fill = verde_acqua, col = "black") +
  labs(title = "Varianza spiegata dalle componenti principali", x = "Componenti principali", y = "Varianza spiegata (%)")+
  geom_point(aes(x = c(1:8), y = varianze_spieg), col = arancione, size = 3)+
  geom_line(aes(x = c(1:8), y = varianze_spieg), col = arancione, size = 1) +
  geom_text(aes(x = c(1:8), y = varianze_spieg), label = varianze_spieg, vjust = -0.5, col = "black") 


#plot delle prime due componenti principali
data_pca = as.data.frame(pca$x)
data_pca$LIFEEXP = clean_data$LIFEEXP
ggplot(data_pca, aes(x = PC1, y = PC2, col = LIFEEXP)) + 
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA", x = "PC1", y = "PC2")

#tentativo di clustering
data_pca$COUNTRY = ddd$COUNTRY

ggplot(aes(x = PC1, y = PC2), data = data_pca) + 
  geom_point() +
  scale_color_manual(values = col_map) +
  labs(title = "Clustering", x = "PC1", y = "PC2")

kmeans = kmeans(data_pca[, c("PC1", "PC2")], centers = 8)
data_pca$cluster = as.factor(kmeans$cluster)
ggplot(aes(x = PC1, y = PC2, col = cluster), data = data_pca) + 
  geom_point() +
  scale_color_manual(values = col_map) +
  labs(title = "Clustering", x = "PC1", y = "PC2")+
  geom_text(aes(label = cluster), vjust = -0.5)

sistema = function(x) gsub("\xa0", " ", x, useBytes = TRUE)
leva_parentesi = function(x) gsub("\\(.*?\\)", "", x)
data_pca$COUNTRY = as.vector(sapply(data_pca$COUNTRY, sistema))
data_pca$COUNTRY[data_pca$COUNTRY == "C\xf4te d'Ivoire" ] = "Côte d'Ivoire"
data_pca$COUNTRY = as.vector(sapply(data_pca$COUNTRY, leva_parentesi))

world = ne_countries(scale = "medium", returnclass = "sf")
countries_regions = data.frame(
  country = data_pca$COUNTRY, 
  region = data_pca$cluster
)
world_regions = world %>%
  left_join(countries_regions, by = c("name_long" = "country"))

ggplot(data = world_regions) +
  geom_sf(aes(fill = region), color = "white", lwd = 0.2) +
  scale_fill_manual(values = col_map) +
  labs(title = "Regioni", fill = "Legenda")