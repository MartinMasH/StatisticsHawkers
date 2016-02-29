##No se ejecutará por el momento, hay que cambiar el source

# Cargar previamente Rcommander

# Lee el conjunto de datos desde 01-01-2016 hasta 24-02-2016
CarbonBlack <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/1 -- Carbon Black - Dark One - Cantidad 20160101-20160224.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
DiamondBlack <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/2 -- Diamond Black - Dark Classic - Cantidad 20160101-20160224.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
Carey <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/3 -- Carey - Daylight Classic - Cantidad 20160101-20160224.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
CrystalBlack <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/4 -- Crystal Black - Dark One - Cantidad 20160101-20160224.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)

# Elimina la última línea
CarbonBlack <- head(CarbonBlack, -1)
DiamondBlack <- head(DiamondBlack, -1)
Carey <- head(Carey, -1)
CrystalBlack <- head(CrystalBlack, -1)

# Carga los scripts necesarios
source("Confianza.R")

# Limpia los datos desde 01-01-2016 hasta 24-02-2016
CarbonBlackClean <- sigmaiter(CarbonBlack$Cantidad, 6, 0.1, 0.1, 0.27)
DiamondBlackClean <- sigmaiter(DiamondBlack$Cantidad, 6, 0.1, 0.1, 0.27)
CareyClean <- sigmaiter(Carey$Cantidad, 6, 0.1, 0.1, 0.27)
CrystalBlackClean <- sigmaiter(CrystalBlack$Cantidad, 6, 0.1, 0.1, 0.27)

# Calcula los intervalos de confianza centrados
CarbonBlackInterCentr <- confcent(CarbonBlackClean, 0.05, 0.2)
DiamondBlackInterCentr <- confcent(DiamondBlackClean, 0.05, 0.2)
CareyInterCentr <- confcent(CareyClean, 0.05, 0.2)
CrystalBlackInterCentr <- confcent(CrystalBlackClean, 0.05, 0.2)

# Calcula los intervalos de confianza mínimos
CarbonBlackInterMin <- confmin(CarbonBlackClean, 0.05)
DiamondBlackInterMin <- confmin(DiamondBlackClean, 0.05)
CareyInterMin <- confmin(CareyClean, 0.05)
CrystalBlackInterMin <- confmin(CrystalBlackClean, 0.05)

# Representa la limpieza de datos
par(mfrow=c(2,4))
plot(CarbonBlack, type="l", xlab="CarbonBlack", col="red")
plot(DiamondBlack, type="l", xlab="DiamondBlack", col="red")
plot(Carey, type="l", xlab="Carey", col="red")
plot(CrystalBlack, type="l", xlab="CrystalBlack", col="red")

plot(CarbonBlackClean, type="l", xlab="CarbonBlack Clean", col="green")
plot(DiamondBlackClean, type="l", xlab="DiamondBlack Clean", col="green")
plot(CareyClean, type="l", xlab="Carey Clean", col="green")
plot(CrystalBlackClean, type="l", xlab="CrystalBlack Clean", col="green")

mtext("Limpieza de datos", side = 3, line = -2, outer = TRUE)

X11()

# Representa los intervalos de confianza centrados y mínimos
# Centrados
par(mfrow=c(2,4))
plot(CarbonBlackClean, type="l", xlab="CarbonBlack Clean")
abline(a = CarbonBlackInterCentr[1], b = 0, col="green")
abline(a = CarbonBlackInterCentr[2], b = 0, col="green")

plot(DiamondBlackClean, type="l", xlab="DiamondBlack Clean")
abline(a = DiamondBlackInterCentr[1], b = 0, col="green")
abline(a = DiamondBlackInterCentr[2], b = 0, col="green")

plot(CareyClean, type="l", xlab="Carey Clean")
abline(a = CareyInterCentr[1], b = 0, col="green")
abline(a = CareyInterCentr[2], b = 0, col="green")

plot(CrystalBlackClean, type="l", xlab="CrystalBlack Clean")
abline(a = CrystalBlackInterCentr[1], b = 0, col="green")
abline(a = CrystalBlackInterCentr[2], b = 0, col="green")

# Mínimos
plot(CarbonBlackClean, type="l", xlab="CarbonBlack Clean")
abline(a = CarbonBlackInterMin[1], b = 0, col="blue")
abline(a = CarbonBlackInterMin[2], b = 0, col="blue")

plot(DiamondBlackClean, type="l", xlab="DiamondBlack Clean")
abline(a = DiamondBlackInterMin[1], b = 0, col="blue")
abline(a = DiamondBlackInterMin[2], b = 0, col="blue")

plot(CareyClean, type="l", xlab="Carey Clean")
abline(a = CareyInterMin[1], b = 0, col="blue")
abline(a = CareyInterMin[2], b = 0, col="blue")

plot(CrystalBlackClean, type="l", xlab="CrystalBlack Clean")
abline(a = CrystalBlackInterMin[1], b = 0, col="blue")
abline(a = CrystalBlackInterMin[2], b = 0, col="blue")

mtext("IC-0.2, Centrados (Verde), Mínimos (Azul)", side = 3, line = -2, outer = TRUE)

# Lee el conjunto de datos desde 01-01-2016 hasta 25-02-2016
CarbonBlack2 <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/1 -- Carbon Black - Dark One - Cantidad 20160101-20160225.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
DiamondBlack2 <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/2 -- Diamond Black - Dark Classic - Cantidad 20160101-20160225.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
Carey2 <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/3 -- Carey - Daylight Classic - Cantidad 20160101-20160225.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)
CrystalBlack2 <- readXL("C:/Users/Magic Designer/Desktop/Testing Intervalos/4 -- Crystal Black - Dark One - Cantidad 20160101-20160225.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Conjunto de datos2", stringsAsFactors=TRUE)

# Elimina la última línea
CarbonBlack2 <- head(CarbonBlack2, -1)
DiamondBlack2 <- head(DiamondBlack2, -1)
Carey2 <- head(Carey2, -1)
CrystalBlack2 <- head(CrystalBlack2, -1)

# Limpia los datos desde 01-01-2016 hasta 25-02-2016
CarbonBlack2Clean <- sigmaiter(CarbonBlack2$Cantidad, 6, 0.1, 0.1, 0.27)
DiamondBlack2Clean <- sigmaiter(DiamondBlack2$Cantidad, 6, 0.1, 0.1, 0.27)
Carey2Clean <- sigmaiter(Carey2$Cantidad, 6, 0.1, 0.1, 0.27)
CrystalBlack2Clean <- sigmaiter(CrystalBlack2$Cantidad, 6, 0.1, 0.1, 0.27)

X11()

# Centrados
par(mfrow=c(2,4))
plot(CarbonBlack2Clean, type="l", xlab="CarbonBlack Clean")
abline(a = CarbonBlackInterCentr[1], b = 0, col="green")
abline(a = CarbonBlackInterCentr[2], b = 0, col="green")

plot(DiamondBlack2Clean, type="l", xlab="DiamondBlack Clean")
abline(a = DiamondBlackInterCentr[1], b = 0, col="green")
abline(a = DiamondBlackInterCentr[2], b = 0, col="green")

plot(Carey2Clean, type="l", xlab="Carey Clean")
abline(a = CareyInterCentr[1], b = 0, col="green")
abline(a = CareyInterCentr[2], b = 0, col="green")

plot(CrystalBlack2Clean, type="l", xlab="CrystalBlack Clean")
abline(a = CrystalBlackInterCentr[1], b = 0, col="green")
abline(a = CrystalBlackInterCentr[2], b = 0, col="green")

# Mínimos
plot(CarbonBlack2Clean, type="l", xlab="CarbonBlack Clean")
abline(a = CarbonBlackInterMin[1], b = 0, col="blue")
abline(a = CarbonBlackInterMin[2], b = 0, col="blue")

plot(DiamondBlack2Clean, type="l", xlab="DiamondBlack Clean")
abline(a = DiamondBlackInterMin[1], b = 0, col="blue")
abline(a = DiamondBlackInterMin[2], b = 0, col="blue")

plot(Carey2Clean, type="l", xlab="Carey Clean")
abline(a = CareyInterMin[1], b = 0, col="blue")
abline(a = CareyInterMin[2], b = 0, col="blue")

plot(CrystalBlack2Clean, type="l", xlab="CrystalBlack Clean")
abline(a = CrystalBlackInterMin[1], b = 0, col="blue")
abline(a = CrystalBlackInterMin[2], b = 0, col="blue")

mtext("IC-0.2, Centrados (Verde), Mínimos (Azul), Añadido un día", side = 3, line = -2, outer = TRUE)



#################################

# Limpia con sigmaitert los datos desde 01-01-2016 hasta 24-02-2016
CarbonBlackCleanT <- sigmaitert(CarbonBlack$Cantidad, 4, 0.1, 0.1, 0.27, 5)
DiamondBlackCleanT <- sigmaitert(DiamondBlack$Cantidad, 4, 0.1, 0.1, 0.27, 5)
CareyCleanT <- sigmaitert(Carey$Cantidad, 4, 0.1, 0.1, 0.27, 5)
CrystalBlackCleanT <- sigmaitert(CrystalBlack$Cantidad, 4, 0.1, 0.1, 0.27, 5)

X11()

# Representa la nueva limpieza de datos
par(mfrow=c(2,4))
plot(CarbonBlack, type="l", xlab="CarbonBlack", col="red")
plot(DiamondBlack, type="l", xlab="DiamondBlack", col="red")
plot(Carey, type="l", xlab="Carey", col="red")
plot(CrystalBlack, type="l", xlab="CrystalBlack", col="red")

plot(CarbonBlackCleanT, type="l", xlab="CarbonBlack Clean", col="green")
plot(DiamondBlackCleanT, type="l", xlab="DiamondBlack Clean", col="green")
plot(CareyCleanT, type="l", xlab="Carey Clean", col="green")
plot(CrystalBlackCleanT, type="l", xlab="CrystalBlack Clean", col="green")

mtext("Limpieza de datos con sigmaiterT", side = 3, line = -2, outer = TRUE)


#################################




