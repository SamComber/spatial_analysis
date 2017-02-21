library(rgdal)
library(maptools)
library(classInt)
library(RColorBrewer)
library(lme4)
library(merTools)
library(ggplot2)

oa <- readOGR(dsn = '/Users/samcomber/Documents/spatial_analysis', layer="OA", stringsAsFactors = FALSE)

OAdata <- oa@data

# fit random intercept model - i.e. model intercept term which varies across MSOAs
OAdata$H_bad_Prop <- (OAdata$H_Vbad + OAdata$H_bad) / OAdata$pop

model.1 <- lmer(unemp ~ S_Rent + dis_ind + Ethnic + (1|MSOA_CD), data = OAdata)
summary(model.1)

VPC <- 0.0007079 / (0.0007079 + 0.0020604)
VPC
# 25% variation in unemployment outcome can be accounted for by differences between MSOAs

fe.eff <- FEsim(model.1)
fe.eff

plotFEsim(fe.eff) + theme_bw()  + labs(x = "Median model coefficients and CIs", y = "Unemployment")
MSOA_re <- REsim(model.1)
p <- plotREsim(MSOA_re)
p


MSOA <- readOGR(dsn = "/Users/samcomber/Documents/spatial_analysis", layer = "MSOA", stringsAsFactors = FALSE)
MSOA@data <- merge(MSOA@data, MSOA_re, by.x = "MSOA_CD", by.y = "groupID", all.x = TRUE)

head(MSOA@data)

temp <- MSOA@data$mean
brks.temp <- classIntervals(temp, n=5, style="quantile")
brks.temp <- round(brks.temp$brks, digits = 2)
brks.temp[1] <- brks.temp[1] - 0.01
brks.temp[5 + 1] <- brks.temp[5 + 1] + 0.01

xx <- MSOA@bbox

scalebar <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(
  min(xx[1,] + 1000), xx[2,1] + 1000), scale = 5000, fill =c("transparent", "black"))

text1 <- list("sp.text", c(min(xx[1,]) + 1000, xx[2,1] + 1500), "0")
text2 <- list("sp.text", c(min(xx[1,]) + 6000, xx[2,1] + 1500), "5 km")
northarrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(
  min(xx[1,]) + 2000, xx[2,1] + 2000
), scale=1500)

# visualise mean of each MSOA random effect spatially
spplot(MSOA, "mean", sp.layout=list(scalebar, text1,text2, northarrow), at = brks.temp, col.regions = brewer.pal(5, "YlOrRd"),
      col="transparent", pretty=TRUE, colorkey = list(labels=list(at=brks.temp)))
head(MSOA)

confint(model.1, level = 0.95)
