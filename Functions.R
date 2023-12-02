library(foreign)
library(haven)
library(tidyverse)

save(DEFF, neff, SQ, horse.diff, horse.sig, horse.toll, horserace, mose, mose_sing, mose_st, prop.diff, prop.sig, prop.toll, prop2, mose, file="Stat Functions.RData")


#This one measures design effect. RUN THIS CHUNK, then skip to line 40
DEFF <- function(x) ( sum(x*x)*nrow(x)/(sum(x)*sum(x)) )
# checks
stopifnot( DEFF(c(1,1,1)) == 1)
stopifnot( DEFF(c(10,10,10)) == 1)
stopifnot( DEFF(c(5,0,0)) == 5)


#These are measures for MOSE. IGNORE for stat testing. 
neff <- function(x) nrow(x)/DEFF(x)
# checks
stopifnot( neff(c(1,1,1)) == 3)
stopifnot( neff(c(10,10,10)) == 3)
stopifnot( neff(c(5,0,0)) == 1)

KFF_data <- read_sav("K:/Public Opinion & Media Research/Health Poll Report (Health News Index)/2020/1 January/Analysis/Kaiser Health Tracker Poll_January_Final no rotates_1.23.20.sav")
#If you load in any POSR dataset and call it KFF_data this should work:
#Example: Democrats MOSE

neff <- (KFF_data %>% filter(m4all == 3 | m4all==4) %>% select(weight) %>% as.vector() %>% neff() )


MOE_num <- qnorm(0.975)*sqrt(0.5*0.5/neff2)
MOE = if_else(is.na(MOE_num), "", paste0("+/- ", round(100*MOE_num), "%")) 
MOE

nrow((KFF_data %>% filter(m4all == 3 | m4all==4) %>% select(weight)))





#OK, this is what we were working on here. Run the function above for DEFF but you can ignore the rest.
#horserace is the p value for comparing 1 groups, 2 answers (ex. total, support or oppose q22) Deff=neff
#horserace<- function { 1.96*(Deff*((proportion of answer 1 + proportion of answer 2) - (difference in proportions of answer 1 -answer 2)^2 / (unweighted n -1) ^.5)

#prop2<-function (1.96 (Deff for group 1*(proportion of answers from group 1 *(1- proportion of answers from group 1)/unweighted n for group 1) + 
 #         (Deff for group 2*(proportion of answers from group 2 *(1- proportion of answers from group 2)/unweighted n for group 2)^.5)


#This function gives you the tollerance level at 0.5. x is the weights, y is the % who said answer 1, z is the % who said answer 2 
#The first number it gives you is the raw tollerance, the second is the abs. value of the difference in means minus this tollerance number
#The third line tells you if the difference is significant or not. 
#To use this function, you'll first run the function chunk so it saves in your environment. Then you will run:
#horserace(your data %>% filter(group. if no group, take out the filter) %>% select(weight), 0.68, 0.79), where those two
#numbers are the % who answered a Q one way or the other. 

horserace<- function (x, y, z) {
  horse.toll<-1.96*(DEFF(x)*((y+z)-((y-z)^2)) / ((nrow(x)-1)))^0.5 
  horse.diff<- abs(y-z)-horse.toll
  horse.sig<- (if (horse.diff>0) "YES")
  newlist<-list(y, z, horse.toll, horse.diff, horse.sig)
  return(newlist)
  }




horse.toll<- function (x, y, z) {(
  1.96*(DEFF(x)*((y+z)-((y-z)^2)) / ((nrow(x)-1)))^0.5 )}

horse.diff<- function (x, y, z) {(abs(y-z)-horse.toll(x, y, z))}
  
horse.sig<- function (x, y, z) {(if ((horse.diff(x,y,z))>0) 
    { 
    print("YES")
     }
    else if ((horse.diff(x,y,z))>-0.009) 
    { print("Borderline")
    }
      else print("No"))}
  
  
HORSERACEFebruary<- cbind(tribble(~"Group", "Answer 1", "Answer 2", "Proportion 1", "Proportion 2", "Tollerance at 0.5", "Tollerance Difference", "Significant?"),
                   tribble(~"Total","Favor Medicare-for-all", "Oppose Medicare-for-all", 0.52, 0.47,  
                           horse.toll((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47),
                           horse.diff((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47), 
                           horse.sig((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47)))

PROP2February <- cbind(tribble(~"Group 1", "Group 2", "Answer", "Proportion 1", "Proportion 2", "Tollerance at 0.5", "Tollerance Difference", "Significant?"),
                   tribble(~"Total","Favor Medicare-for-all", "Oppose Medicare-for-all", 0.52, 0.47,  
                           horse.toll((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47),
                           horse.diff((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47), 
                           horse.sig((KFF_data %>% filter(m4all==1 | m4all==2) %>% select(weight)), 0.52, 0.47)))

#This is the prop2 comparison that stat tests how two different groups gave one answer. Example, Democrats or Republicans favorability
#of the ACA. The first line gives you the raw tollerance at 0.5, second is the abs. value of the diff. in means minus the tollerance
#Third line tells you if it is significant.
#Run the chunk to save it into your environment. Then, run your stat testing in this format:
#prop2(your data %>% filter(first group) %>% select(weight)), 0.59, (your data %>% filter(second group) %>% select(weight)), 0.89
prop2<- function (a, b, c, d) { 
  prop.toll<- 1.96*(DEFF(a)*(b*(1-b)/nrow(a))+DEFF(c)*(d*(1-d)/nrow(c)))^0.5
  prop.diff<-abs(b-d)-prop.toll
  prop.sig<- (if (prop.diff>0) "YES")
  prop.list<-list(b, d, prop.toll, prop.diff, prop.sig)
  return(prop.list)
  
}

#EXAMPLE:
horserace(KFF_data %>% select(weight), 0.68, 0.79) 


prop2(KFF_data %>% filter(gendervar==1) %>% select(weight)), 0.59, (KFF_data %>% filter(gendervar==2) %>% select(weight)), 0.89)

prop.toll<- function (a, b, c, d) {1.96*(DEFF(a)*(b*(1-b)/nrow(a))+DEFF(c)*(d*(1-d)/nrow(c)))^0.5}
prop.diff<- function (a, b, c, d) {abs(b-d)-prop.toll(a, b, c, d)}

prop.sig<-function (a,b,c,d) {(if ((prop.diff(a, b, c, d))>0) 
{ 
  print("YES")
}
else if ((prop.diff(a, b, c, d))>-0.009) 
{ print("Borderline")
}
else print("No"))}


prop.sig((covid %>% filter(age>=60 & chroniccovid==1) %>% select(weight)), 0.64, covid %>% select(weight), 0.67)


prop2((KFF_data %>% select(weight_ssrs) %>% as.vector()), 0.56, 0.68) 



##MOSE functions:



# to read SPSS files
install.packages("foreign")
install.packages("haven")
# to produce graphics
install.packages("ggplot2")
install.packages("RColorBrewer")
# to process data
install.packages("tidyverse")
# to produce neat tables
install.packages("knitr")
install.packages("kableExtra")
# to produce a codebook
install.packages("vtable")

# to read SPSS files
library(foreign)
library(haven)
# to produce graphics
library(ggplot2)
library(RColorBrewer)
# to process data
library(tidyverse)
# to produce neat tables
library(knitr)
library(kableExtra)
# to produce a codebook
library(vtable)

mose<- function (x) {
  (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% select(weight) %>% as.vector() %>% neff() ))), "", paste0("+/- ", 
    round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )
}



mose(covid) #mose of the total. choose a filter() to specify a group



mose_st<- function (x){
  
  cbind(tribble(~" ", "Unweighted N", "Sum of weights", "Sum of weights squared", "DEFF", "SQRT DEFF", "+/- MOSE"), 
        tribble(~"Total", nrow(x), 
                (x %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>%  select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Democrats", nrow(x %>% filter(party==2)), 
                (x %>% filter(party==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(party==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(party==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(party==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==2) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Republicans", nrow(x %>% filter(party==1)), 
                (x %>% filter(party==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(party==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(party==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(party==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==1) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Independents", nrow(x %>% filter(party==3)), 
                (x %>% filter(party==3) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(party==3) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(party==3) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(party==3) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party==3) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Dem/Dem-leaning Indeps",   nrow(x %>% filter(party3==2)), 
                (x %>% filter(party3==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(party3==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(party3==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(party3==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party3==2) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party3==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Rep/Rep-leaning Indeps",   nrow(x %>% filter(party3==1)), 
                (x %>% filter(party3==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(party3==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(party3==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(party3==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party3==1) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(party3==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"18-29",   nrow(x %>% filter(recage2==1)), 
                (x %>% filter(recage2==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(recage2==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(recage2==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(recage2==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==1) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"30-49",   nrow(x %>% filter(recage2==2)), 
                (x %>% filter(recage2==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(recage2==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(recage2==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(recage2==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==2) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"50-64",  nrow(x %>% filter(recage2==3)), 
                (x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(recage2==3) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"65+",   nrow(x %>% filter(recage2==4)), 
                (x %>% filter(recage2==4) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(recage2==4) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(recage2==4) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(recage2==4) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==4) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Male",   nrow(x %>% filter(gendervar==1)), 
                (x %>% filter(gendervar==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(gendervar==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(gendervar==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(gendervar==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(gendervar==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Female",   nrow(x %>% filter(gendervar==2)), 
                (x %>% filter(gendervar==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(gendervar==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(gendervar==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(gendervar==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(gendervar==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"ACA Favorable",   nrow(x %>% filter(aca==1 |aca==2)), 
                (x %>% filter(aca==1 |aca==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(aca==1 |aca==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(aca==1 |aca==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(aca==1 |aca==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(aca==1 |aca==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"ACA Unfavorable",   nrow(x %>% filter(aca==3 |aca==4)), 
                (x %>% filter(aca==3 |aca==4) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(aca==3 |aca==4) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(aca==3 |aca==4) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(aca==3 |aca==4) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(aca==3 |aca==4) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Insured Under 65",   nrow(x %>% filter(agecov==1)), 
                (x %>% filter(agecov==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(agecov==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(agecov==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(agecov==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()),
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(agecov==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Uninsured Under 65",   nrow(x %>% filter(agecov==2)), 
                (x %>% filter(agecov==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(agecov==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(agecov==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(agecov==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(agecov==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Approve of Trump",   nrow(x %>% filter(trumpapprove2==1)), 
                (x %>% filter(trumpapprove2==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(trumpapprove2==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(trumpapprove2==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(trumpapprove2==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(trumpapprove2==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Disapprove of Trump",    nrow(x %>% filter(trumpapprove2==2)), 
                (x %>% filter(trumpapprove2==2) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(trumpapprove2==2) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(trumpapprove2==2) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(trumpapprove2==2) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(trumpapprove2==2) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )),
        
        tribble(~"Registered Voters",   nrow(x %>% filter(rvote==1)), 
                (x %>% filter(rvote==1) %>% select(weight) %>% as.vector() %>% sum()), 
                (x %>% filter(rvote==1) %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x %>% filter(rvote==1) %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x %>% filter(rvote==1) %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "",
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(rvote==1) %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )))
  
}


mose_sing<- function (x) {
  cbind(tribble(~" ", "Unweighted N", "Sum of weights", "Sum of weights squared", "DEFF", "SQRT DEFF", "+/- MOSE"),
        tribble(~" ",    nrow(x), 
                (x %>% select(weight) %>% as.vector() %>% sum()), 
                (x  %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()), 
                (x  %>% select(weight) %>% as.vector() %>% DEFF() ), 
                (x  %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()), 
                (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x  %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                         paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x  %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) )))
}


tribble(~"Half Sample A", NA, nrow(x %>% filter(halfsamp=="1")),
        (x %>% filter(halfsamp==1) %>% select(weight)%>% sum()),
        (x %>% filter(halfsamp==1) %>% select(weight)  %>% SQ() %>% sum()),
        (x %>% filter(halfsamp==1) %>% select(weight)%>% DEFF() ),
        (x %>% filter(halfsamp==1) %>% select(weight) %>% DEFF() %>% sqrt()),
        (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                 paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(halfsamp=="1") %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) ))

tribble(~"Half Sample B", NA, nrow(x %>% filter(halfsamp=="2")),
        (x %>% filter(halfsamp=="2") %>% select(weight) %>% as.vector() %>% sum()),
        (x %>% filter(halfsamp=="2") %>% select(weight) %>% as.vector()  %>% SQ() %>% sum()),
        (x %>% filter(halfsamp=="2") %>% select(weight) %>% as.vector() %>% DEFF() ),
        (x %>% filter(halfsamp=="2") %>% select(weight) %>% as.vector() %>% DEFF() %>% sqrt()),
        (if_else(is.na(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(recage2==3) %>% select(weight) %>% as.vector() %>% neff() ))), "", 
                 paste0("+/- ", round(100*(qnorm(0.975)*sqrt(0.5*0.5/(x %>% filter(halfsamp=="2") %>% select(weight) %>% as.vector() %>% neff() )))), "%") ) ))
  