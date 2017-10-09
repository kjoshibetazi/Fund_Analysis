setwd("C:/Users/Kishan/Desktop/Heidi-Fund Analysis")

Fund_analysis<-function(price_multiplier,loan_ratio){
  
  #defining inputs
  PDP_cap=40000000
  PUD_cap=60000000
  
  risk_equity=80
  # loan_ratio=40
  int_rate=10
  uw_cost=7
  # price_multiplier=5
  count=8
  
  #Input columns
  Year<-list(2017:2024)
  PDP<-seq(68571,42491,length.out=8)
  Price=c(30,35,40,45,50,50,50,50)
  PUD_P50<-seq(300000,400000,length.out=8)
  PUD_P90<-seq(200000,8000,length.out = 8)
  
  #Calculations
  Net_price=Price*price_multiplier
  PDP_rev=PDP*Net_price
  PUD50_rev=PUD_P50*Net_price
  PUD90_rev=PUD_P90*Net_price
  
  Risked_returns=PUD50_rev
  Unrisked_returns=PDP_rev+PUD90_rev 
  
  cap_req=PDP_cap+PUD_cap
  Lent_cap=cap_req*loan_ratio/100*(1+uw_cost/100)
  Equity_cap=cap_req-Lent_cap
  
  Principal_left<-vector(mode='numeric')
  Interest<-vector(mode='numeric')
  Principal_pay<-vector(mode='numeric')
  Total_pay<-vector(mode='numeric')
  
  #Unrisked return calculation
  for(i in 1:count)
  {
    if(i==1)
    {Principal_left[i]=Lent_cap} 
    
    Interest[i]=Principal_left[i]*int_rate/100 
    Total_pay[i]=Unrisked_returns[i]
    Principal_pay[i]=Total_pay[i]-Interest[i]
    
    if(Principal_left[i]<=Principal_pay[i])
    {Principal_pay[i]=Principal_left[i] 
    Principal_left[i+1]=0
    Total_pay[i]=Principal_left[i]+Interest[i]}
    
    Principal_left[i+1]=Principal_left[i]-Principal_pay[i]  #for the next loop
  }
  Principal_left<-Principal_left[-c(count+1)]
  
  #Finals
  Cash_unrisked= Unrisked_returns-Total_pay
  Unrisked=cbind(Principal_left,Interest,Unrisked_returns,Total_pay,Principal_pay,Cash_unrisked)
  
  Cash_risked=Risked_returns

   #ROI Calculations
  Cum_int=sum(Interest)
  Cum_loan_pay=Cum_int+Lent_cap
 
  Cum_equity=(risk_equity/100)*(1-loan_ratio/100)*(sum(Cash_risked)+sum(Cash_unrisked)-Equity_cap)
  Cum_return=cap_req+Cum_equity+Cum_int

  Our_split=0.25*Cum_equity
  
  Total_ROI=100*(((Cum_return-Our_split)/cap_req)^(1/count)-1)
  ROI_int=100*(((Cum_int+cap_req)/cap_req)^(1/count)-1)
  
  #NPV calculations for ROI
  NPV<-function(Value,rate,total_period)
  {
    for (i in 1:total_period)
    {
      if (i==1)
      {pv<-vector(mode='numeric')}
      pv[i]=Value[i]/(1+rate/100)^(i)
    }
    result=sum(pv)
    return(result)
  }
  
  npv_unrisked_cash=NPV(Cash_unrisked,int_rate,count)
  npv_risked_cash=NPV(Cash_risked,int_rate,count)
  npv_interest=NPV(Interest,int_rate,count)
  
  npv_equity=(risk_equity/100)*(1-loan_ratio/100)*(npv_risked_cash+npv_unrisked_cash-Equity_cap)
  npv_net_return=npv_equity+npv_interest
  
  #Plots
  colors=c('green','red')
  type<-c('Risked','Unrisked')
  months=c(1,2,3,4,5,6,7,8)
  Returns<-cbind(Risked_returns,Unrisked_returns)
  Returns<-t(Returns)
  barplot(Returns,names.arg = months,xlab="month",ylab="Returns",col=colors)
  legend('topleft',type,cex=1,fill=colors)
  
  
  return(list("ROI"=Total_ROI,"NPV_Unrisked"=npv_unrisked_cash,"NPV_Risked"=npv_risked_cash,"NPV_Interest"=npv_interest,"Cum_Return"=npv_net_return))
}

price_m<-c(0.25,0.5,1,1.5,2,3,4)
loan_r<-c(20,30,40,50,60,70,80)

for (i in 1:length(price_m))
{
  if (i==1)
  {ROIs<-matrix(0,nrow=length(loan_r),ncol=length(price_m))
   NPV_Unrisked<-matrix(0,nrow=length(loan_r),ncol=length(price_m))
   NPV_Risked<-matrix(0,nrow=length(loan_r),ncol=length(price_m))
   NPV_Interest<-matrix(0,nrow=length(loan_r),ncol=length(price_m))
   Cum_return<-matrix(0,nrow=length(loan_r),ncol=length(price_m))}
  
  for (j in 1:length(loan_r))
  {
    aa<-Fund_analysis(price_m[i],loan_r[j])
    ROIs[j,i]=aa$ROI
    NPV_Unrisked[j,i]=aa$NPV_Unrisked
    NPV_Risked[j,i]=aa$NPV_Risked
    NPV_Interest[j,i]=aa$NPV_Interest
    Cum_return[j,i]=aa$Cum_Return
  }
}


#Plotting of the Results
persp3D(loan_r, price_m,ROIs, colvar = ROIs, phi = 20, theta = 35,
        xlab = "Loan Percentage", ylab = "Price Multiplier",
        main = "Return on Investment")

library (plotly)
library(ggplot2)
#Plot with surface 3D for ROI
plot_ly(x=price_m, y=loan_r, z=ROIs,type='surface')%>%
  layout(
    title = "Return on Investment",
    scene = list(
      xaxis = list(title = "Price Multiplier"),
      yaxis = list(title = "Loan Percentage"),
      zaxis = list(title = "ROI")
    ))

#Contour plot for ROI
plot_ly(x=price_m, y=loan_r, z=ROIs,type='contour',contours = list(showlabels = TRUE))%>%
  layout(
    title = "Return on Investment",
    scene = list(
      xaxis = list(title = "Price Multiplier"),
      yaxis = list(title = "Loan Percentage"),
      zaxis = list(title = "ROI")
    ))

#Plot with surface and scatter for median along Price_m and loan_r
pROI<-apply(ROIs,2,mean)
lROI<-apply(ROIs,1,mean)
plot_ly(x=price_m, y=loan_r, z=ROIs,type='surface')%>%
  add_trace(x=price_m,y=loan_r,z=pROI,mode='markers',type='scatter3d',marker = list(size = 5, color = "red", symbol = 104))%>%
  add_trace(x=price_m,y=loan_r,z=lROI,mode='markers',type='scatter3d',marker = list(size = 5, color = "blue", symbol = 104))%>%
  layout(
    title = "Return on Investment",
    scene = list(
      xaxis = list(title = "Price Multiplier"),
      yaxis = list(title = "Loan Percentage"),
      zaxis = list(title = "ROI")
    ))

##Plot with surface 3D for net_return to investor $
plot_ly(x=price_m, y=loan_r, z=Cum_return,type='surface')%>%
  layout(
    title = "Return on Investment in $",
    scene = list(
      xaxis = list(title = "Price Multiplier"),
      yaxis = list(title = "Loan Percentage"),
      zaxis = list(title = "Cumulative Returns")
    ))

#Contour plot for net return to investor $
plot_ly(x=price_m, y=loan_r, z=Cum_return,type='contour',colors= c('red','green'),contours = list(showlabels = TRUE))%>%
  layout(
    title = "Return on $100M investment",
      xaxis = list(title = "Price Multiplier"),
      yaxis = list(title = "Loan Percentage")
    )

Zero_return<-ifelse(Cum_return<0,Cum_return,0)
Pos_return<-ifelse(Cum_return>=0,Cum_return,0)
plot_ly(x=price_m, y=loan_r)%>%
 add_contour( z=Zero_return,colors='red',contours = list(showlabels = TRUE))%>%
 add_contour ( z=Pos_return,colors='green',contours = list(showlabels = TRUE)) %>%
  layout(
    title = "Return on $100M investment",
    xaxis = list(title = "Price Multiplier"),
    yaxis = list(title = "Loan Percentage")
  )
  
# #Same Contour plot functionality different function
# filled.contour(price_m,loan_r,Cum_return,
#                plot.title = title(main="Return on $100M investment",
#                xlab="Price Multiplier", ylab="Loan ratio"),
#               key.title=title(main="ROI"))