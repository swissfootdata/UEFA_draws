library(readxl)
library(DatawRappr)

swiss_clubs <- read_excel("cldraw'21-22.xlsx", 
                           sheet = "17.Switzerland")

CL_quali <- read_excel("cldraw'21-22.xlsx", 
                           sheet = "CL")

ECL_quali <- read_excel("cldraw'21-22.xlsx", 
                          sheet = "ECL")

codes_ECL <- c("9w4JB","Eykfr","vpUP3","ISQIA","4g2T6","Iq1Aq","dC1Cf","Jzdhb","4qQtf")

#Qualified Teams
qualified_teams <- swiss_clubs[1:4,c(1:2,11,12)]
colnames(qualified_teams) <- c("Rank","Club","Coefficient","Competition")

write.csv(qualified_teams,"Output/Qualified_Teams.csv",row.names = FALSE)

print(qualified_teams)

###CL-Quali

#Get opponents CLQ2
CLQ2_Opponents <- CL_quali[15:34,6:8]

if (qualified_teams$Coefficient[1] > CLQ2_Opponents$...8[10]) {
  
  CLQ2_Opponents <- CLQ2_Opponents[11:20,]
  CLQ2_Seed <- "seeded"
  
 } else {
  
 CLQ2_Opponents <- CLQ2_Opponents[1:10,]
 CLQ2_Seed <- "unseeded"
    
 }

#Adapt
CLQ2_Opponents$...7 <- substring(CLQ2_Opponents$...7,1,3)
colnames(CLQ2_Opponents) <- c("Team","Country","Coefficient")

write.csv(CLQ2_Opponents,"Output/CLQ2_Opponents.csv",row.names = FALSE)

print(CLQ2_Opponents)

#Get opponents CLQ3
CLQ3_Opponents <- CL_quali[15:26,10:12]

if (qualified_teams$Coefficient[1] > CLQ3_Opponents$...12[6]) {
  
  CLQ3_Opponents <- CLQ3_Opponents[7:12,]
  CLQ3_Seed <- "seeded"
  
} else {
  
  CLQ3_Opponents <- CLQ3_Opponents[1:6,]
  CLQ3_Seed <- "unseeded"
  
}

#Adapt
CLQ3_Opponents$...11 <- substring(CLQ3_Opponents$...11,1,3)
colnames(CLQ3_Opponents) <- c("Team","Country","Coefficient")

write.csv(CLQ3_Opponents,"Output/CLQ3_Opponents.csv",row.names = FALSE)

print(CLQ3_Opponents)

#Get opponents CLQ4
CLQ4_Opponents <- CL_quali[15:22,14:16]

if (qualified_teams$Coefficient[1] > CLQ4_Opponents$...16[4]) {
  
  CLQ4_Opponents <- CLQ4_Opponents[5:8,]
  CLQ4_Seed <- "seeded"
  
} else {
  
  CLQ4_Opponents <- CLQ4_Opponents[1:4,]
  CLQ4_Seed <- "unseeded"
  
}

#Adapt
CLQ4_Opponents$...15 <- substring(CLQ4_Opponents$...15,1,3)
colnames(CLQ4_Opponents) <- c("Team","Country","Coefficient")

write.csv(CLQ4_Opponents,"Output/CLQ4_Opponents.csv",row.names = FALSE)

print(CLQ4_Opponents)

#Update Datawrapper Charts
datawrapper_auth("C13EmLLMTymQpzvUWuIIDtcR6i5iWv5wbZBKOoHpCbo1JTIiEltuS6pGwlQ5m8or", overwrite = TRUE)

dw_edit_chart("d8y8U",
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("d8y8U")

#CL
dw_edit_chart("pYvuP",
              title=paste0("Possible Opponents in CLQ2 for ",qualified_teams$Club[1]),
              intro=paste0(qualified_teams$Club[1]," would be the <b>",CLQ2_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("pYvuP")


dw_edit_chart("PY4I0",
              title=paste0("Possible Opponents in CLQ3 for ",qualified_teams$Club[1]),
              intro=paste0(qualified_teams$Club[1]," would be the <b>",CLQ3_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("PY4I0")

dw_edit_chart("dnbae",
              title=paste0("Possible Opponents in CLQ4 for ",qualified_teams$Club[1]),
              intro=paste0(qualified_teams$Club[1]," would be the <b>",CLQ4_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("dnbae")


###ECL-Quali

for (i in 2:nrow(qualified_teams)) {

#Get opponents ECLQ2
ECLQ2_Opponents <- ECL_quali[1:90,7:9]

if (qualified_teams$Coefficient[i] > ECLQ2_Opponents$...9[45]) {
  
  ECLQ2_Opponents <- ECLQ2_Opponents[46:90,]
  ECLQ2_Seed <- "seeded"
  
} else {
  
  ECLQ2_Opponents <- ECLQ2_Opponents[1:45,]
  ECLQ2_Seed <- "unseeded"
  
}

#Adapt
ECLQ2_Opponents$...8 <- substring(ECLQ2_Opponents$...8,1,3)
colnames(ECLQ2_Opponents) <- c("Team","Country","Coefficient")
ECLQ2_Opponents <- ECLQ2_Opponents[ECLQ2_Opponents$Country !="Sui",]

write.csv(ECLQ2_Opponents,paste0("Output/ECLQ2_Opponents_",i,".csv"),row.names = FALSE)

print(ECLQ2_Opponents)

#Get opponents ECLQ3
ECLQ3_Opponents <- ECL_quali[1:52,12:14]

if (qualified_teams$Coefficient[i] > ECLQ3_Opponents$...14[26]) {
  
  ECLQ3_Opponents <- ECLQ3_Opponents[27:52,]
  ECLQ3_Seed <- "seeded"
  
} else {
  
  ECLQ3_Opponents <- ECLQ3_Opponents[1:26,]
  ECLQ3_Seed <- "unseeded"
  
}

#Adapt
ECLQ3_Opponents$...13 <- substring(ECLQ3_Opponents$...13,1,3)
colnames(ECLQ3_Opponents) <- c("Team","Country","Coefficient")
ECLQ3_Opponents <- ECLQ3_Opponents[ECLQ3_Opponents$Country !="Sui",]

write.csv(ECLQ3_Opponents,paste0("Output/ECLQ3_Opponents_",i,".csv"),row.names = FALSE)

print(ECLQ3_Opponents)


#Get opponents ECLQ4
ECLQ4_Opponents <- ECL_quali[1:34,17:19]


if (qualified_teams$Coefficient[i] > ECLQ4_Opponents$...19[17]) {
  
  ECLQ4_Opponents <- ECLQ4_Opponents[18:34,]
  ECLQ4_Seed <- "seeded"
  
} else {
  
  ECLQ4_Opponents <- ECLQ4_Opponents[1:17,]
  ECLQ4_Seed <- "unseeded"
  
}

#Adapt
ECLQ4_Opponents$...18 <- substring(ECLQ4_Opponents$...18,1,3)
colnames(ECLQ4_Opponents) <- c("Team","Country","Coefficient")
ECLQ4_Opponents <- ECLQ4_Opponents[ECLQ4_Opponents$Country !="Sui",]

write.csv(ECLQ4_Opponents,paste0("Output/ECLQ4_Opponents_",i,".csv"),row.names = FALSE)

print(ECLQ4_Opponents)

dw_edit_chart(codes_ECL[i-1],
              title=paste0("Possible Opponents in ECLQ2 for ",qualified_teams$Club[i]),
              intro=paste0(qualified_teams$Club[i]," would be the <b>",ECLQ2_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart(codes_ECL[i-1])


dw_edit_chart(codes_ECL[i+2],
              title=paste0("Possible Opponents in ECLQ3 for ",qualified_teams$Club[i]),
              intro=paste0(qualified_teams$Club[i]," would be the <b>",ECLQ3_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart(codes_ECL[i+2])

dw_edit_chart(codes_ECL[i+5],
              title=paste0("Possible Opponents in ECLQ4 for ",qualified_teams$Club[i]),
              intro=paste0(qualified_teams$Club[i]," would be the <b>",ECLQ4_Seed," </b>team"),
              annotate=paste0("Last Update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart(codes_ECL[i+5])


}

#Commit
source("commit.R")
  