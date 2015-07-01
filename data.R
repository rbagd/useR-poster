setwd("/home/rytis/git/phd-presentations/poster/")
library(rsdmx)
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/"
key <- "MEI_BTS_COS/..BLSA.M/all?startTime=2000-01"
xml_data <- readSDMX(paste0(url,key))
data <- as.data.frame(xml_data)

library(reshape2)
library(xts)
countries <- c("BEL","FRA","DEU","NLD") #, "LUX", "ESP", "ITA", "SWE", "FIN", "DNK")
wdata <- dcast(subset(data, LOCATION %in% countries  & SUBJECT == "BSCI"),
               obsTime ~ LOCATION + SUBJECT, value.var="obsValue")
wdata <- xts(wdata[,-1], as.yearmon(wdata[,1]))
ddata <- wdata - lag(wdata, 3)

key_gdp <- paste0("QNA/", paste(countries, collapse="+"), ".B1_GE.VPVOBARSA.Q/all?startTime=2000-01")
xml_gdp <- readSDMX(paste0(url,key_gdp))
gdp <- as.data.frame(xml_gdp)
gdp_data <- dcast(gdp, obsTime ~ LOCATION, value.var="obsValue")

gdp_data <- xts(gdp_data[,-1], as.yearqtr(gdp_data[,1], format="%Y-Q%q"))
qgrowth <- (gdp_data / lag(gdp_data, 1) - 1)*100

all_data <- merge(ddata, xts(qgrowth, as.yearmon(time(qgrowth))+0.2))

library(xtsExtra)
save.image("data.Rdata")

load("data.Rdata")
png(filename="figures/comparison.png", height=15, width=24, units="cm", res=400, pointsize=12)
#setEPS()
#postscript("figures/comparison.eps", paper="special", onefile=FALSE, horizontal=FALSE, height=4, width=10)
#svg("figures/comparison.svg")
par(mfrow=c(2,1), mar=c(1,1,1,1))
plot(all_data[6:nrow(all_data),1:4], lwd=0.6, main="3-month diff. in manufacturing confidence indicator", cex=0.8, cex.axis=1.2)
plot(na.approx(all_data[,5:8]), lwd=0.7, main="Quarterly GDP growth", legend.loc='bottomright', cex=0.8, cex.axis=1.2)
dev.off()

setwd("/home/rytis/git/belgoindex/dfm-estimator")
devtools::load_all()
b <- dfm(all_data[4:nrow(all_data),], 2, 2, 2, rQ='identity', rC='upper', max_iter=1000)

par(mfrow=c(1,1)) #, mar=c(1,1,1,1))
plot(all_data[6:nrow(all_data),1:4], lwd=0.6, main="3-month diff. in manufacturing confidence indicator", cex=0.8, cex.axis=1.2)
plot(na.approx(all_data[,5:8]), lwd=0.7, main="Quarterly GDP growth", legend.loc='bottomright', cex=0.8, cex.axis=1.2)

dplot <- na.approx(all_data[(100+4):nrow(all_data),11:15])
qmlF <- xts(head(tail(b$qml[100:nrow(b$qml),1], -2), -2), time(dplot))
qmlF <- xts(b$qml, time(tail(all_data, -3))
#qpca <- xts(head(tail(b$pca[,1], -2), -2), time(dplot))
colors <- c("#3D3D3D", "#2A7BB4", "#146F19", "#D6EA33")

png(filename="figures/factor.png", height=10, width=18, units="cm", res=400, pointsize=12)
par(mfrow=c(1,1), mar=c(1,1,1,1)); plot(-qmlF[110:nrow(qmlF),1]/4, lwd=2, colorset="red", main="Quarterly GDP growth and 1st factor", grid.col="#E5E5E5", cex=0.8, cex.axis=1.2); for (i in 1:4) { lines(dplot[9:nrow(dplot),i], type='l', lwd=0.9, lty=2, col=colors[i]) }
par(mar=c(0,0,0,0))
legend("bottomleft", c("Belgium", "France", "Germany", "Netherlands", "Factor 1"), col=c(colors, "red"), lwd=c(rep(0.9,4), 2), lty=c(rep(2,4), 1), cex=0.7, inset=c(0.79,0.0))
dev.off()
