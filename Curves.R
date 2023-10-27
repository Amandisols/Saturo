library(readxl)
library(tidyverse)

dat <- read_excel("F22WS1N4.xlsx", 
                  sheet = "Raw Data")

summary <- read_excel("F22WS1N4.xlsx", 
                      sheet = "Summary",
                      col_names = c("Settings", "Value"))


# Extract presoak period & Kfs

# For some reason this gives me back extra NAs
# presoak <- as.numeric(summary$Value[summary$Settings == "Soak Time (min)"])
# presoak <- presoak[1]
# Kfs <- as.numeric(summary$Value[summary$Settings == "Kfs (cm/s)"])
# Kfs <- Kfs[1]

presoak <- as.numeric(summary[5,2])
Kfs <- as.numeric(summary[22,2])
Kfs_units <- as.character(summary[22,1])
Kfs_units <- unlist(strsplit(Kfs_units, split = " ", fixed = TRUE))[2]


#Subset to only low pressure data (<5cm pressure head)
datLow <- subset(dat, `Pressure (cm)` <= 5)

#Model for a power function
m <- lm(log(`Flux (cm/s)`) ~ log(`Time (min)`), data=datLow)

#Create dataframe for predicted values 
pred <- data.frame(Time = seq(1:max(datLow$`Time (min)`)))
pred$predFlux <- exp(coef(m)[1])*pred$Time^coef(m)[2]

# Base R plot
# #Plot entire infiltration curve
# plot(datLow$`Time (min)`, datLow$`Flux (cm/s)`,
#      xlab = "Time (min)", ylab = "Flux (cm/s)")
# lines(pred$Time, pred$predFlux)
# text(150, 0.0018, substitute(b0*x^b1, list(b0=round(exp(coef(m)[1]),5), b1=round(coef(m)[2],5))))
# text(150, 0.0016, substitute(plain("R-squared: ") * r2, list(r2= round(summary(m)$r.squared,2)))) #need to round these numbers


# Average initial infiltration for the presoak period
init_infil_pred <- round(with(pred, mean(predFlux[Time <= presoak])),5)
init_infil_raw <- round(with(dat, mean(`Flux (cm/s)`[`Time (min)`<= presoak])),5)
datLow$Presoak <- ifelse(datLow$`Time (min)` <= presoak, "Yes", "No")


#Plot and save output

png('testGraph.png', units = "in", width = 6, height = 6, res = 600)

datLow %>%
  ggplot(aes(x = `Time (min)`, y =`Flux (cm/s)`)) +
  geom_point(aes(color = Presoak), size = 2)+
  geom_line(data = pred, aes(x = Time, y = predFlux), size =1) +
  scale_color_manual(values= c("black", "deepskyblue2"))+
  theme_classic()+
  labs(title = "Fit Infiltration Curve",
       subtitle = bquote("y="~ .(as.numeric(round(exp(coef(m)[1]),5))) ~ "x"^.(as.numeric(round(coef(m)[2],5)))~ "    " ~ "r"^2== ~.(round(summary(m)$r.squared,2))),
       caption = paste("Presoak Period:", init_infil_pred, Kfs_units)) +
  theme(plot.caption = element_text(color = "deepskyblue2", face = "bold", size = 10),
        legend.position = "none")

dev.off()


#cm/s to cm/hr: multiply by 3600


# TO DO:
## Create output data file with all values
## How many options for Kfs units?
## Kfs in half-hour increments?

