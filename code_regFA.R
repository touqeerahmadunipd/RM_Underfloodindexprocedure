library(lmomRFA)
#Data for region 1
name<-c(0001,0002,0003,0004,0005,0006,0007,0008,0009,0010,0011,0012,0013) 
n<-c(30,30,30,30,30,30,30,30,30,30,30,30,30) 
mean<-c(12.78467,12.28833,12.39033,12.36967,12.18167,12.28933,12.03700,15.86733,16.48367,13.51867,13.77700,12.02833,12.11367)  
t<-c(0.04506116,0.04476819,0.04524013,0.06064799,0.05120848,0.07276285,0.06511818,0.05249562,0.04354781,0.05369842,0.04525700,0.03746040,0.04006399) 
t_3<-c(0.05479706,0.08107108,-0.02341455,0.23809451,0.17371814,0.33033465,0.24859066,0.24400721,0.17295159,0.33059861,0.19122763,0.24201278,0.11349265)
t_4<-c(0.03506980,0.23283795,0.11929776,0.19808158,0.21957794,0.35010022,0.31569106,0.28477134,0.14030886,0.25018247,0.18671606,0.19282307,0.09305994)

Data<-data.frame(name,n,mean,t, t_3, t_4)
rmom <- regavlmom(Data)
rfit <- regfit(Data, "glo")

evplot(rfit, col = "blue")                    # Plot the regional growth curve

# Compute error bounds for quantile estimates.  We will
# (optimistically) generate bounds for a homogeneous region
fval <- c(1-(1/2), 1-(1/5), 1-(1/10), 1-(1/20), 1-(1/50), 1-(1/100), 1-(1/200), 1-(1/500), 1-(1/1000))   # A lot of quantiles
simq <- regsimq(rfit$qfunc, nrec=Cascades$n, nrep=10000, f=fval,
                fit=rfit$dist)

# Regional growth curve, and bounds
rbounds <- regquantbounds(simq, rfit)
evplot(rfit, rbounds, col = "purple")
# 
Time <- seq(1, 10, by = 1)
 Quantile <- rbounds$qhat
 LowerBound <- rbounds$bound.0.05
 UpperBound <- rbounds$bound.0.95
# 
# # Create a dataframe
data_df <- data.frame(Time = c(2, 5, 10, 20, 50, 100, 200, 500, 1000), fval,Quantile, LowerBound, UpperBound)
#
evplot( xlim=c(0.5,8.9),ylim = c(0.8,1.5))
grid()

lines( data_df$Quantile,  type = "l", col="blue", lwd=2)
grid()
lines(data_df$LowerBound, type = "l", col="red", lty=2,lwd=2)
lines(data_df$UpperBound, type = "l",, col="red", lty=2, lwd=2)



