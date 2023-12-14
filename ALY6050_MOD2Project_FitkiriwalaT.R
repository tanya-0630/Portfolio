print("Tanya Fitkiriwala")


install.packages('triangle')
library(triangle)  

install.packages('EnvStats')
library(EnvStats)  

##part1##
##(1)##
#PROJECT1 BENEFIT 10000 SIMULATIONS
Damb1 <- rtri(10000, min = 1.1, mode = 2, max = 2.8)
Damb2 <- rtri(10000, min = 8, mode = 12, max = 14.9)
Damb3 <- rtri(10000, min = 1.4, mode = 1.41, max = 2.2)
Damb4 <- rtri(10000, min = 6.5, mode = 9.8, max =14.6)
Damb5 <- rtri(10000, min = 1.7, mode = 2.4, max =3.6)
Damb6 <- rtri(10000, min=0, mode=1.6,max=2.4)
total <- Damb1 + Damb2 + Damb3 + Damb4 + Damb5 + Damb6
benefit1 <- as.data.frame(cbind(Damb1,Damb2,Damb3,Damb4,Damb5,Damb6,total))

#PROJECT1 COST 10000 SIMULATIONS
Damc1 <- rtri(10000,min = 13.2, mode = 14.2, max = 19.1)
Damc2 <- rtri(10000, min = 3.5, mode = 4.9, max = 7.4)
total2 <- Damc1 + Damc2
cost1 <- as.data.frame(cbind(Damc1, Damc2, total2))

################################################################################

#PROJECT2 BENEFIT 10000 SIMULATIONS
Damb7 <- rtri(10000, min = 2.1, mode = 3, max = 4.8)
Damb8 <- rtri(10000, min = 8.7, mode = 12.2, max = 13.6)
Damb9 <- rtri(10000, min = 2.3, mode = 3, max = 3.1)
Damb10 <- rtri(10000, min = 5.9, mode = 8.7, max =15)
Damb11 <- rtri(10000, min = 0, mode = 3.4, max =3.41)
Damb12 <- rtri(10000, min=0, mode=1.2,max=1.8)
total3 <- Damb7 + Damb8 + Damb9 + Damb10 + Damb11 + Damb12
benefit2 <- as.data.frame(cbind(Damb7,Damb8,Damb9,Damb10,Damb11,Damb12,total3))

#PROJECT1 COST 10000 SIMULATIONS
Damc3 <- rtri(10000,min = 12.8, mode = 15.8, max = 20.1)
Damc4 <- rtri(10000, min = 3.8, mode = 5.7, max = 8)
total4 <- Damc3 + Damc4
cost2 <- as.data.frame(cbind(Damc3, Damc4, total4))

benefit_cost_ratio1 <- as.data.frame(round((benefit1$total/cost1$total2), digits = 3))
benefit_cost_ratio2 <- as.data.frame(round((benefit2$total3/cost2$total4), digits =3))
ratio <- as.data.frame(cbind(benefit_cost_ratio1,benefit_cost_ratio2))
colnames(ratio) <- c('alpha1','alpha2')


##(2)##
##frequency table of alpha 1
freq_table1 <- table(ratio$alpha1)
freq_table1
##frequency graph of alpha 1
barplot(freq_table1, width = 4, main = 'Frequency Graph: Dam1 Project', xlab = 'alpha1',
        ylab = 'Frequency')

min(ratio$alpha1)
max(ratio$alpha1)

##frequency table of alpha 2
freq_table2 <- table(ratio$alpha2)
freq_table2
##frequency graph of alpha 2
barplot(freq_table2, width = 4, main = 'Frequency Graph: Dam2 Project', xlab = 'alpha2',
        ylab = 'Frequency')

min(ratio$alpha2)
max(ratio$alpha2)


###(3)####
##Dam1

##observed values
m1<- round(mean(benefit1$total) , digits = 3)
sd1 <- round(sd(benefit1$total), digits =3)
m2 <- round(mean(cost1$total2), digits = 3)
sd2 <- round(sd(cost1$total2), digits =3)
m3 <- round(mean(ratio$alpha1), digits =3)
sd3 <- round(sd(ratio$alpha1), digits =3)

#theoretical
#total benefits
b1 <- (1.1+2+2.8)/3
b2 <- (8+12+14.9)/3
b3 <- (1.4+1.4+2.2)/3
b4 <- (6.5+9.8+14.6)/3
b5 <- (1.7+2.4+3.6)/3
b6 <- (0+1.6+2.4)/3

me1 <- round((b1+b2+b3+b4+b5+b6), digits = 3)#mean
me1

bs1 <- ((1.1^2 + 2^2 + 2.8^2 - 1.1*2 -1.1*2.8 -2*2.8)/18)
bs2 <- ((8^2+12^2+14.9^2 - 8*12 -8*14.9 - 12*14.9)/18)
bs3 <- ((1.4^2 + 1.4^2 + 2.2^2 - 1.4*1.4 - 1.4*2.2 -1.4*2.2)/18)
bs4 <- ((6.5^2+9.8^2+14.6^2 - 6.5*9.8-6.5*14.6 -9.8*14.6)/18)
bs5 <- ((1.7^2+2.4^2+3.6^2 - 1.7*2.4 -1.7*3.6 -2.4*3.6)/18)
bs6 <- ((0^2 + 1.6^2 + 2.4^2 - 0*1.6 - 0*2.4 - 1.6*2.4)/18)

sde1 <- round(sqrt(bs1+bs2+bs3+bs4+bs5+bs6), digits =3) ##standard deviation
sde1

#totalcost

c1 <- (13.2+14.2+19.1)/3
c2 <- (3.5+4.9+7.4)/3
mc1 <- round((c1+c2), digits =3)
mc1


cs1 <- ((13.2^2 + 14.2^2 + 19.1^2 - 13.2*14.2 - 13.2*19.1-14.2*19.1)/18)
cs2 <- ((3.5^2+4.9^2+7.4^2 - 3.5*4.9 -3.5*7.4 -4.9*7.4)/18)

sdc1 <- round(sqrt(cs1+cs2), digits =3)
sdc1

tb1 <- as.data.frame(cbind(c(m1,sd1,m2,sd2,m3,sd3),c(me1,sde1,mc1,sdc1,'X','X')))
row.names(tb1) <- c('Mean of the Total Benefits', 'SD of the total Benefits', 'Mean of the Total Cost',
                    'SD of the Total Cost', 'Mean of the Benefit-Cost Ratio', 'SD of the Benefit-Cost Ratio')

colnames(tb1) <- c('Observed', 'Theoretical')



####Dam2###

##observed
m4<- round(mean(benefit2$total3) , digits = 3)
sd4 <- round(sd(benefit2$total3), digits =3)
m5 <- round(mean(cost2$total4), digits = 3)
sd5 <- round(sd(cost2$total4), digits =3)
m6 <- round(mean(ratio$alpha2), digits =3)
sd6 <- round(sd(ratio$alpha2), digits =3)

##theoretical
##total benefit
b7 <- (2.1+3+4.8)/3
b8 <- (8.7+12.2+13.6)/3
b9 <- (2.3+3+3)/3
b10 <- (5.9+8.7+15)/3
b11 <- (0+3.4+3.4)/3
b12 <- (0+1.2+1.8)/3

me2 <- round((b7+b8+b9+b10+b11+b12), digits =3)

bs7 <- (2.1^2 + 3^2 +4.8^2 - 2.1*3 -2.1*4.8 -3*4.8)/18
bs8 <- (8.7^2 + 12.2^2 + 13.6^2 - 8.7*12.2 -8.7*13.6 - 12.2*13.6)/18
bs9 <- (2.3^2+3^2+3^2 - 2.3*3 -2.3*3 - 3*3)/18
bs10 <- (5.9^2+8.7^2+15^2 - 5.9*8.7 -5.9*15.-8.7*15)/18
bs11 <- (0^2 + 3.4^2 + 3.4^2 - 0*3.4 - 0*3.4 - 3.4*3.4)/18
bs12 <- (0^2 + 1.2^2 + 1.8^2 - 0*1.2 - 0*1.8 - 1.2*1.8)/18

sde2 <- round((bs7+bs8+bs9+bs10+bs11+bs12), digits = 3)

##total cost
c3 <- (12.8+15.8+20.1)/3
c4 <- (3.8+5.7+8)/3

mc2 <- round((c3+c4), digits =3)

cs3 <- (12.8^2+15.8^2+20.1^2 - 12.8*15.8 - 12.8*20.1 - 15.8*20.1)/18
cs4 <- (3.8^2 + 5.7^2 + 8^2 - 3.8*5.7 - 5.7*8 - 3.8*8)/18

sdc2 <- round((cs3+cs4), digits = 3)


tb2 <- as.data.frame(cbind(c(m4,sd4,m5,sd5,m6,sd6),c(me2,sde2,mc2,sdc2,'X','X')))
row.names(tb2) <- c('Mean of the Total Benefits', 'SD of the total Benefits', 'Mean of the Total Cost',
                    'SD of the Total Cost', 'Mean of the Benefit-Cost Ratio', 'SD of the Benefit-Cost Ratio')

colnames(tb2) <- c('Observed', 'Theoretical')

###############################################################################

##part-2 chi-square test
## theoretical values of dam 1 selected

##estimated values
min <- min(ratio$alpha1)
max <- max(ratio$alpha1)
range <- max - min
width <- range/sqrt(10000)
bins <- round(sqrt(10000))
breaks <- seq(min-width, max+width, length.out = bins)

# obtain the frequency distribution for the observed data
benefit_cost_ratio.cut = cut(ratio$alpha1, breaks, right=FALSE)
benefit_cost_ratio.cut.freq = table(benefit_cost_ratio.cut)
barplot(benefit_cost_ratio.cut.freq, main="Frequency Distribution",  ylab="Frequency")
### remove the first element of the breaks  vector (not necessary). I need to remove it in order to append to the df
breaks <- breaks[-1]

freq_df <- data.frame(benefit_cost_ratio.cut.freq,breaks)

# check if count matches trials
sum(freq_df$Freq)


# observed stats:
mean_benefits <- mean(benefit1$total)
sd_benefits <- sd(benefit1$total)
mean_costs <- mean(cost1$total2)
sd_costs <- sd(cost1$total2)

mean_ratio <- mean(ratio$alpha1)
sd_ratio <- sd(ratio$alpha1)

##----------------------------------------------------------------------###
# gamma distribution
# alpha is the shape parameter
# beta is the inverse of scale parameter
alpha <- mean_ratio^2/sd_ratio^2
beta <- sd_ratio^2/mean_ratio
scale <- 1/beta

theo_prob <- dgamma(breaks, alpha,scale)

# exp means expected
freq_exp <- theo_prob / sum(theo_prob) * trials
# check to see if the sum adds up
sum(freq_exp)
freq_df <- data.frame(freq_df, freq_exp)


barplot(freq_df$Freq, main="Frequency Distribution",  ylab="Frequency")

# need to convert the observed values into barplot (later)
plot(freq_df$breaks, freq_df$Freq)
lines(freq_df$breaks, freq_df$freq_exp)

## goodness of fit
chisq.test(freq_df$Freq, freq_df$freq_exp) 


##part3: recommendation

#alpha1
library(moments)
final_table <- as.data.frame(cbind(rbind(min(ratio$alpha1),max(ratio$alpha1),mean(ratio$alpha1),
                                   median(ratio$alpha1),(sd(ratio$alpha1))^2,sd(ratio$alpha1),
                                   skewness(ratio$alpha1),sum(ratio$alpha1 > 2, na.rm=TRUE)/10000,
                             sum(ratio$alpha1 > 1.8, na.rm=TRUE)/10000,sum(ratio$alpha1 > 1.5, na.rm=TRUE)/10000,
sum(ratio$alpha1 > 1.2, na.rm=TRUE)/10000,sum(ratio$alpha1 > 1, na.rm=TRUE)/10000),
rbind(min(ratio$alpha2),max(ratio$alpha2),mean(ratio$alpha2),
      median(ratio$alpha2),(sd(ratio$alpha2))^2,sd(ratio$alpha2),
      skewness(ratio$alpha2),sum(ratio$alpha2 > 2, na.rm=TRUE)/10000,
      sum(ratio$alpha2 > 1.8, na.rm=TRUE)/10000,sum(ratio$alpha2 > 1.5, na.rm=TRUE)/10000,
      sum(ratio$alpha2 > 1.2, na.rm=TRUE)/10000,sum(ratio$alpha2 > 1, na.rm=TRUE)/10000)))

colnames(final_table) <- c('alpha1','alpha2')
rownames(final_table) <- c('Minimum','Maximum','Mean','Median','Variance','Standard Deviation',
                           'Skewness','P(i >2)','P(i >1.8)','P(i >1.5)','P(i >1.2)','P(i >1)')

final_table$'alpha1>alpha2' <- ifelse(final_table$alpha1 > final_table$alpha2, 1,0)
final_table[0:7,3] <- c('','','','','','','')

#recommended project
sum(final_table$`alpha1>alpha2` == 1, na.rm=TRUE)/5

#since the result of 1 is 0.4, the data says to recommend dam2 project
