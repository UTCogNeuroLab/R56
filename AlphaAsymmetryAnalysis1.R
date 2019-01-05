###Alpha Analyses### 
#2018 Alex Alario#
#please let me know if you have any questions! alexalario96@gmail.com


##Creating dataframe with alpha asymmetry scores and midfrontal alpha scores##

#this creates a file with alpha asymmetry scores#
#I did this for each day, so I would have a separate file for day 1 and day 2 scores
# can make this into one file if you changed the names of the columns

data=read.csv("/Users/alexalario/Documents/R56/temporary.csv", header = TRUE, sep = ",",na = c("-","99999","NA"))

#Equation: ln[Right] - ln[Left]  e.g.: ln(F2) - (F1) (From John Allen, 2010)
#(F2–F1, F4–F3, F6–F5, F8–F7)
#Eyes Open: lnF2 - lnF1
data$OpenAVGF21=log(data$F2_AverageOpenAVG) - log(data$F1_AverageOpenAVG)
data$OpenAVGF43=log(data$F4_AverageOpenAVG) - log(data$F3_AverageOpenAVG)
data$OpenAVGF65=log(data$F6_AverageOpenAVG) - log(data$F5_AverageOpenAVG)
data$OpenAVGF87=log(data$F8_AverageOpenAVG) - log(data$F7_AverageOpenAVG)
data$OpenAVGF21=log(data$F2_AverageOpenCSD) - log(data$F1_AverageOpenCSD)
data$OpenCSDF43=log(data$F4_AverageOpenCSD) - log(data$F3_AverageOpenCSD)
data$OpenCSDF65=log(data$F6_AverageOpenCSD) - log(data$F5_AverageOpenCSD)
data$OpenCSDF87=log(data$F8_AverageOpenCSD) - log(data$F7_AverageOpenCSD)
data$ClosedAVGF21=log(data$F2_AverageClosedAVG) - log(data$F1_AverageClosedAVG)
data$ClosedAVGF43=log(data$F4_AverageClosedAVG) - log(data$F3_AverageClosedAVG)
data$ClosedAVGF65=log(data$F6_AverageClosedAVG) - log(data$F5_AverageClosedAVG)
data$ClosedAVGF87=log(data$F8_AverageClosedAVG) - log(data$F7_AverageClosedAVG)
data$ClosedCSDF21=log(data$F2_AverageClosedCSD) - log(data$F1_AverageClosedCSD)
data$ClosedCSDF43=log(data$F4_AverageClosedCSD) - log(data$F3_AverageClosedCSD)
data$ClosedCSDF65=log(data$F6_AverageClosedCSD) - log(data$F5_AverageClosedCSD)
data$ClosedCSDF87=log(data$F8_AverageClosedCSD) - log(data$F7_AverageClosedCSD)

write.csv(data, "alphadata.csv")
##can then average eyes open and eyes closed to get one score for each location/reference type
# I didnt do this until a later step but it depends on what you are looking at!

#I am not sure if this works, I did this by hand in the excel sheet
a = log(data$Fp1_AverageOpenCSD) + log(data$Fp2_AverageOpenCSD) + log(data$AF3_AverageOpenCSD) +
  log(data$AF4_AverageOpenCSD) + log(data$F1_AverageOpenCSD) + log(data$F2_AverageOpenCSD) + log(data$Fz_AverageOpenCSD)

midfrontOpenCSD = a/7

write.csv(midfrontOpenCSD, "midfrontOpenCSD.csv")

##For asymmetry, I replaced the value with "99999" (by hand) at locations where I had interpolated at least one of the involved locations
##For the midfrontal values, I replaced values with "99999" when 3 locations within were interpolated

#I then excluded ("99999") values that were 2xIQR > 75 quartile or 2xIQR < 25 quartile for that location by hand...though there is likely a better way to do that
#to determine quartile values for each asymmetry score/midfrontal score
quantile(data$midfrontClosedCSD)

#to determine IQR value for each asymmetry score/midfrontal score
IQR(data$midfrontClosedCSD, na.rm = TRUE)
