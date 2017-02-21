library(lme4)
library(rgdal)
library(maptools)
library(classInt)
library(RColorBrewer)
library(merTools)
library(ggplot2)

OA <- readOGR(dsn = '/Users/samcomber/Documents/spatial_analysis/p3', layer="OA", stringsAsFactors = FALSE)
OAdata <- OA@data

OAdata <- OAdata[order(OAdata$MSOA_CD),]
head(OAdata)

sample.20 <- sample(unique(OAdata$MSOA_CD), 20 , replace = FALSE)

ggplot(OAdata[OAdata$MSOA_CD %in% sample.20,], aes(x = unemp, y = age_men)) + geom_point() +
  geom_smooth(method="lm", se = FALSE) + 
  facet_wrap(~ MSOA_CD) +
  xlab("Unemployment rates") +
  ylab("Average age") + 
  theme_bw()

# model intercepts and effect of unemployment rates vary across MSOAs
model.0 <- lmer(S_Rent ~ Ethnic + age_men + (1 + unemp|MSOA_CD), data = OAdata)
summary(model.0)

# extract random effect estimates
MSOA_re <- REsim(model.0)
MSOA_re

p <- plotREsim(MSOA_re)
p

MSOA <- readOGR(dsn = '/Users/samcomber/Documents/spatial_analysis/p3', layer="MSOA", stringsAsFactors = FALSE)
MSOA_code <- MSOA_re[MSOA_re$term == "(Intercept)", "groupID"]
MSOA_intercept <- MSOA_re[MSOA_re$term == "(Intercept)", "mean"]
MSOA_slope <- MSOA_re[MSOA_re$term == "unemp", "mean"]
MSOA_link <- data.frame(MSOA_code, MSOA_intercept, MSOA_slope)

MSOA@data <- merge(MSOA@data , MSOA_link, by.x = "MSOA_CD", by.y = "MSOA_code", all.x=TRUE)
head(MSOA@data)

temp <- MSOA@data$MSOA_intercept
brks.temp <- classIntervals(temp, n = 5, style = "quantile")
brks.temp <- round(brks.temp$brks, digits = 2)
brks.temp[1] <- brks.temp[1] - 0.01
brks.temp[5 + 1] <- brks.temp[5 + 1] + 0.01

# Scale bar and legend
xx <- MSOA@bbox

scalebar <- list("SpatialPolygonsRescale", layout.scale.bar(),
                 offset = c(min(xx[1,] + 1000),xx[2,1] + 1000), scale = 5000,    fill=c("transparent","black"))

text1 <- list("sp.text", c(min(xx[1,]) + 1000,xx[2,1] + 1500), "0")
text2 <- list("sp.text", c(min(xx[1,]) + 6000,xx[2,1] + 1500), "5 km")
northarrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
                   offset = c(min(xx[1,]) + 2000,xx[2,1] + 2000), scale = 1500)

## Plot
spplot(MSOA, "MSOA_intercept", sp.layout=list(scalebar,text1,text2,northarrow),
       at = brks.temp, col.regions = brewer.pal(5, "YlOrRd"),
       col="transparent",pretty=TRUE,
       colorkey=list(labels=list(at=brks.temp)))