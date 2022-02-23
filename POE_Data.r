---
title: "POE Experiment"
date: "2/18/2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include = FALSE}
library(dplyr)
options(digits = 3) 
x1 = read.csv("data_exp_76718-v14_questionnaire-1w6g.csv")
x2 = read.csv("data_exp_76718-v14_questionnaire-4bbb.csv")
x3 = read.csv("data_exp_76718-v14_questionnaire-4vdr.csv")
SFRaw1 = read.csv("data_exp_76718-v14_questionnaire-5hlm.csv")
x5 = read.csv("data_exp_76718-v14_questionnaire-5o7y.csv")
SFText1 = read.csv("data_exp_76718-v14_questionnaire-6b4h.csv")
CDText1 = read.csv("data_exp_76718-v14_questionnaire-6c6w.csv")
CDText2 = read.csv("data_exp_76718-v14_questionnaire-bqqj.csv")
SFRaw2 = read.csv("data_exp_76718-v14_questionnaire-brl6.csv")
ConsentForm = read.csv("data_exp_76718-v14_questionnaire-cu2v.csv")
CDRaw1 = read.csv("data_exp_76718-v14_questionnaire-dkkj.csv")
CONFText1 = read.csv("data_exp_76718-v14_questionnaire-dz2x.csv")
CONFRaw1 = read.csv("data_exp_76718-v14_questionnaire-fs2u.csv")
CONFRaw2 = read.csv("data_exp_76718-v14_questionnaire-ik34.csv")
x15 = read.csv("data_exp_76718-v14_questionnaire-jmxy.csv")
SFText2 = read.csv("data_exp_76718-v14_questionnaire-krz8.csv")
SFText3 = read.csv("data_exp_76718-v14_questionnaire-lerw.csv")
CONFRaw3 = read.csv("data_exp_76718-v14_questionnaire-lud7.csv")
x19 = read.csv("data_exp_76718-v14_questionnaire-n3oe.csv")
x20 = read.csv("data_exp_76718-v14_questionnaire-qydd.csv")
x21 = read.csv("data_exp_76718-v14_questionnaire-s2g8.csv")
CDText3 = read.csv("data_exp_76718-v14_questionnaire-tsq6.csv")
x23 = read.csv("data_exp_76718-v14_questionnaire-txm8.csv")
SFRaw3 = read.csv("data_exp_76718-v14_questionnaire-vhaj.csv")
x25 = read.csv("data_exp_76718-v14_questionnaire-x6eu.csv")
x26 = read.csv("data_exp_76718-v14_questionnaire-n3oe.csv")
SFPEOE1 = read.csv("data_exp_76718-v14_task-2mze.csv")
CONFPEOE1 = read.csv("data_exp_76718-v14_task-4o6w.csv")
t3 = read.csv("data_exp_76718-v14_task-5gij.csv")
t4 = read.csv("data_exp_76718-v14_task-6efv.csv")
CDPEOE1 = read.csv("data_exp_76718-v14_task-8f92.csv")
TestBEnd = read.csv("data_exp_76718-v14_task-bwfv.csv")
CDPEOE2 = read.csv("data_exp_76718-v14_task-j1oy.csv")
TriviaGroupB = read.csv("data_exp_76718-v14_task-jaen.csv")
CDPEOE3 = read.csv("data_exp_76718-v14_task-kwyw.csv")
TestAEnd = read.csv("data_exp_76718-v14_task-l75d.csv")
t11 = read.csv("data_exp_76718-v14_task-mb7n.csv")
TriviaGroupA = read.csv("data_exp_76718-v14_task-pogz.csv")
TestAStart = read.csv("data_exp_76718-v14_task-sroz.csv")
CONFPEOE3 = read.csv("data_exp_76718-v14_task-sztm.csv")
CONFPEOE2 = read.csv("data_exp_76718-v14_task-tuea.csv")
TestBStart = read.csv("data_exp_76718-v14_task-u5e4.csv")
t17 = read.csv("data_exp_76718-v14_task-u7lv.csv")
t18 = read.csv("data_exp_76718-v14_task-w97d.csv")
```

# Overall Improvement (After learning - Before learning)
```{r, }
#Preliminary Splitting by participant
SplitAStart = split(TestAStart, TestAStart$Participant.Private.ID)
SplitBStart = split(TestBStart, TestBStart$Participant.Private.ID)

SplitAEnd = split(TestAEnd, TestAEnd$Participant.Private.ID)
SplitBEnd = split(TestBEnd, TestBEnd$Participant.Private.ID)

#Overall Performance
PerfAStart = sapply(SplitAStart, function(x){
  100*sum((x$Correct))/length(na.omit(x$Correct))
})

PerfBEnd = sapply(SplitBEnd, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

PerfBStart = sapply(SplitBStart, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

PerfAEnd = sapply(SplitAEnd, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

Improve1 = PerfBEnd - PerfAStart
Improve2 = PerfAEnd - PerfBStart

Improve1
Improve2
```
**Note:** Labels are participant IDs, numbers represent percentage gain/loss


# Pre and Post Scores for Each Participant
```{r}
ScoreA = rbind(PerfBStart, PerfAEnd)
ScoreB = rbind(PerfAStart, PerfBEnd)

rownames(ScoreA) = c("Before Training", "After Training")
rownames(ScoreB) = c("Before Training", "After Training")

combinedScores = data.frame(ScoreA,ScoreB)

rel.improve = c()
for (i in 1:ncol(combinedScores)){
  rel.improve = c(rel.improve, (combinedScores[2,i] - combinedScores[1,i])
                  / (100 - combinedScores[1,i]))
}

combinedScores = rbind(combinedScores, rel.improve)
rownames(combinedScores)[3] = "Relative Improvement"

data.frame(t(combinedScores))
```


# Improvement Over Topic 
```{r}
#Cleans up question labels
clean_Qtypes = function(df){
  df$Q.Type = substr(df$display,1,2)
  df = df[!is.na(df$randomise_trials),]
  return(df)
}
TestAStart = clean_Qtypes(TestAStart)
TestBStart = clean_Qtypes(TestBStart)
TestAEnd = clean_Qtypes(TestAEnd)
TestBEnd = clean_Qtypes(TestBEnd)

# Performance Across Each Test Across Each Topic
TopicPerf = function(df){
  a = split(df, df$Participant.Private.ID)
  final = sapply(a, function(x){
    tmp = split(x, x$Q.Type)
    sapply(tmp, function(y){
      (sum(y$Correct)/length(y$Correct))*100
      })
  })
  return(final)
}

TopicPerfAStart = TopicPerf(TestAStart)
TopicPerfBStart = TopicPerf(TestBStart)
TopicPerfAEnd = TopicPerf(TestAEnd)
TopicPerfBEnd = TopicPerf(TestBEnd)

TopicChange1 = TopicPerfBEnd - TopicPerfAStart
TopicChange2 = TopicPerfAEnd - TopicPerfBStart

TopicChange = merge(as.data.frame(TopicChange1), as.data.frame(TopicChange2), 
                   by="row.names") 
TopicChange = TopicChange[c(1,2,4,3),]

TopicChangeDisplay =data.frame(t(TopicChange))
names(TopicChangeDisplay) = TopicChangeDisplay[1,]
TopicChangeDisplay = TopicChangeDisplay[-1,]
TopicChangeDisplay
```
**Note1:** Labels are participant IDs, numbers represent percentage gain/loss \
**Note2:** CD = Cognitive Dissonance, CO = Conformity, OB = Obedience, SF = Social Facilitation. \
**Note3:** The average of the percentage changes don't equal to overall percentage improvement because some conditions have 5 questions while others have 6. 


# Pre and Post Scores for Each Topic
```{r}
clean_topic = function(df){
  for (i in 1:nrow(df)){
    df$display[i] = substr(df$display[i], 1,2)
  }
  return (df)
}

TmpAStart = clean_topic(TestAStart)
TmpBStart = clean_topic(TestBStart)
TmpAEnd = clean_topic(TestAEnd)
TmpBEnd = clean_topic(TestBEnd)

SplitATopicStart = split(TmpAStart, TmpAStart$display)
SplitBTopicStart = split(TmpBStart, TmpBStart$display)

SplitATopicEnd = split(TmpAEnd, TmpAEnd$display)
SplitBTopicEnd = split(TmpBEnd, TmpBEnd$display)

#Overall Performance
PerfAStart = sapply(SplitATopicStart, function(x){
  100*sum((x$Correct))/length(na.omit(x$Correct))
})

PerfBEnd = sapply(SplitBTopicEnd, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

PerfBStart = sapply(SplitBTopicStart, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

PerfAEnd = sapply(SplitATopicEnd, function(x){
  100*sum(na.omit(x$Correct))/length(na.omit(x$Correct))
})

TopicPrePost = data.frame(PerfAStart*.6 + PerfBStart*.4,
                          PerfBEnd*.6 + PerfAEnd*.4)

names(TopicPrePost) = c("Before Training", "After Training")
rownames(TopicPrePost)[3] = "OB (No training)"

TopicPrePost
```

# Relative Improvement Across Topic:
Note here the formula used is: (score improvement)/(100 - initial score)
```{r}
TopicRI = TopicChange

start = data.frame(TopicPerfAStart, TopicPerfBStart)
end = data.frame(TopicPerfBEnd, TopicPerfAEnd)
for(i in 1:nrow(TopicRI)){
  for(j in 2:ncol(TopicRI)){
    #Find this person's pre and post scores given ID and topic
    ID = names(TopicRI)[j]
    Topic = TopicRI$Row.names[i]
    preScore = start[which(rownames(start) == Topic), 
                     which(substr(names(start),2, nchar(names(start))) == ID)]
    
    endScore = end[which(rownames(end) == Topic), 
                   which(substr(names(end), 2, nchar(names(end))) == ID)] 
    #Calculate relative improvement :)
    r.improve = (endScore - preScore) / (100 - preScore)
    TopicRI[i,j] = r.improve
  }
}

TopicRI
```
# Improvement Over Condition
First we want to know what conditions each participant observed: 
```{r}
#List initialization and name matching to each dataframe
CondDf = data.frame("ID" = 1:10, "Cond1" = rep(NA, 10), "Cond2" = rep(NA,10),
                    "Cond3" = rep(NA,10))

CondDf$ID = unique(ConsentForm$Participant.Private.ID)[-11]

condList = list(SFRaw1, SFText1, CDText1, CDText2, SFRaw2, CDRaw1,CONFText1, 
             CONFRaw1, CONFRaw2, CONFRaw3, CDText3, SFRaw3, SFPEOE1, CONFPEOE1, 
             CDPEOE1, CDPEOE2, CDPEOE3, SFText2, SFText3, CONFPEOE2, CONFPEOE3)

condNameList = list("SFRaw1", "SFText1", "CDText1", "CDText2", "SFRaw2", "CDRaw1",
              "CONFText1", "CONFRaw1", "CONFRaw2", "CONFRaw3", "CDText3", 
              "SFRaw3", "SFPEOE1", "CONFPEOE1", "CDPEOE1", "CDPEOE2", "CDPEOE3", 
              "SFText2", "SFText3", "CONFPEOE2", "CONFPEOE3")

       
# Fills up NA values for the condition dataframe
for (i in 1:length(condList)){
  uniqueList = unique(condList[[i]]$Participant.Private.ID)
  for (j in 1:length(uniqueList)){
    if(!is.na(uniqueList[j])){
      #Find row of ID
      enterRow = which(CondDf$ID == uniqueList[j])
      if(is.na(CondDf$Cond1[enterRow])){
        CondDf$Cond1[enterRow] = condNameList[i]
      }
      else if (is.na(CondDf$Cond2[enterRow])){
        CondDf$Cond2[enterRow] = condNameList[i]
      }
      else{
        CondDf$Cond3[enterRow] = condNameList[i]
      }
    }
  }
}

CondDf
```

Now we can match each participant's score change to the their respective conditions. 
```{r}
CondChange = TopicChange
names(CondChange)[1] = "Condition/ID"
CondChange$`Condition/ID`[1] = "PEOE"
CondChange$`Condition/ID`[2] = "Raw"
CondChange$`Condition/ID`[3] = "Text"
CondChange$`Condition/ID`[4] = "None"

for (rowNum in 1:dim(CondDf)[1]){
  for (colNum in 2:dim(CondDf)[2]){
    #First find the condition of our participant
    val = grep("(PEOE|Raw|Text)", CondDf[rowNum,colNum], value = TRUE)
    check = regmatches(val, gregexpr("(PEOE|Raw|Text)", val))
    Part.Condition = check
    
    #Next find the topic of our participant
    val = grep("(CD|CO|SF)", CondDf[rowNum,colNum], value = TRUE)
    check = regmatches(val, gregexpr("(CD|CO|SF)", val))
    
    Part.Topic = check
    ID = CondDf$ID[rowNum]
    
    #Find the improvement given the topic and ID
    TopicCol = which(names(TopicChange) == ID)
    TopicRow = which(TopicChange$Row.names == Part.Topic)
    ImproveAmount = TopicChange[TopicRow, TopicCol]
    
    #Insert into final dataframe
    finalCol = which(names(CondChange) == ID)
    finalRow = which(CondChange$`Condition/ID` == Part.Condition)
    
    CondChange[finalRow, finalCol] = ImproveAmount
  }
}

CondChange
```

Now Let's look at the average change over conditions: 
```{r}
CondChangeMeans = rowMeans(CondChange[,-1])
names(CondChangeMeans) = c("PEOE", "Raw", "Text", "None")
CondChangeMeans
```

# Time Spent Per Condition
Now we want to see how long they spent in each of the conditions:
```{r}
#Initializing Resulting Dataframe
CondTimeDf = data.frame("ID" = 1:10, "Raw" = rep(NA, 10), 
                        "Text" = rep(NA,10), "PEOE" = rep(NA,10))
CondTimeDf$ID = unique(ConsentForm$Participant.Private.ID)[-11]

#Calculates the total time spent in each condition
timeList = c()
pureCondTimeList = c()
timeListFinal = c()
for (i in 1:length(condList)){
    tmp = split(condList[[i]], condList[[i]]$Participant.Private.ID)
    timeList = c(sapply(tmp, function(x){
     peoeIndex = grep("PEOE", condNameList)
     if (i %in% peoeIndex){
       val = sum(na.omit(x$Reaction.Time))/2
     }
     else{
       rowNum = which(x$Question.Key == "END QUESTIONNAIRE")
        val = as.numeric(x$Response[rowNum])
     }
     return(val)
   }))
    whichCond = grep("(PEOE|Raw|Text)", condList[i], value = TRUE)
    pureCondTimeList = c(pureCondTimeList, 
                         rep(regmatches(whichCond,
                                        regexpr("(PEOE|Raw|Text)", 
                                                whichCond)), length(timeList)))
    timeListFinal = c(timeListFinal, timeList)
}

# Fills up NA values for the time dataframe
for (j in 1:length(timeListFinal)){
  enterRow = which(CondTimeDf$ID == as.numeric(names(timeListFinal)[j]))
  if (pureCondTimeList[j] == "Raw"){
    CondTimeDf$Raw[enterRow] = timeListFinal[j]/60000
     }
  else if (pureCondTimeList[j] == "Text"){
    CondTimeDf$Text[enterRow] = timeListFinal[[j]]/60000
     }
  else{
    CondTimeDf$PEOE[enterRow] = timeListFinal[[j]]/60000
     }
}

CondTimeDf
```

Here are the means of time spent across conditions: 
```{r}
c(mean(CondTimeDf$Raw), mean(CondTimeDf$Text), mean(CondTimeDf$PEOE))
```
**Note:** I'm not too sure in what unit the original numbers are in, but I assumed they are in milliseconds. This is the data where I converted the numbers to minutes using that assumption. 

# Pre and Post Scores for Each Condition
```{r}
#find condition of each of the values, add to their respective vectors, average them 
TopicBefore = data.frame(TopicPerfAStart, TopicPerfBStart)
TopicAfter = data.frame(TopicPerfAEnd, TopicPerfBEnd)

get_PPCScores = function(df){
  PEOE = c()
  Raw = c()
  Text = c()
  None = c()
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      privID = as.numeric(substr(names(df)[1],2, nchar(names(df)[1])))
      topic = rownames(df)[i]
      if (topic == "OB"){
        None = c(None, df[i,j])
        next
      }
      #Now we need to find the condition in CondDf
      IndexRow = which(CondDf$ID == privID)
      #Now find topic
      IndexCol = which(regmatches(CondDf[IndexRow,], 
                                  regexpr("(CO|CD|SF)", CondDf[IndexRow,]))
                                  == topic) + 1
      Info = CondDf[IndexRow, IndexCol]
      Condition = regmatches(CondDf[IndexRow, IndexCol], 
                             regexpr("(PEOE|Raw|Text)", 
                                     CondDf[IndexRow, IndexCol]))
      if (Condition == "PEOE"){
        PEOE = c(PEOE, df[i,j])
      }
      else if (Condition == "Raw"){
        Raw = c(Raw, df[i,j])
      }
      else if (Condition == "Text"){
        Text = c(Text, df[i,j])
      }
    }
  }
  return(data.frame(mean(PEOE), mean(Raw), mean(Text), mean(None)))
}

PrePostCondScore = rbind(get_PPCScores(TopicBefore), get_PPCScores(TopicAfter))
rownames(PrePostCondScore) = c("Pretest", "Posttest")
PrePostCondScore
```
# Correlation Check
Note here we're looking for the correlation between the average pre-test scores for every participant and their posttest scores within each condition. 
```{r}
Get_Cond_Scores = function(df){
  PEOE = c()
  Raw = c()
  Text = c()
  None = c()
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      privID = as.numeric(substr(names(df)[1],2, nchar(names(df)[1])))
      topic = rownames(df)[i]
      if (topic == "OB"){
        None = c(None, df[i,j])
        next
      }
      #Now we need to find the condition in CondDf
      IndexRow = which(CondDf$ID == privID)
      #Now find topic
      IndexCol = which(regmatches(CondDf[IndexRow,], 
                                  regexpr("(CO|CD|SF)", CondDf[IndexRow,]))
                                  == topic) + 1
      Info = CondDf[IndexRow, IndexCol]
      Condition = regmatches(CondDf[IndexRow, IndexCol], 
                             regexpr("(PEOE|Raw|Text)", 
                                     CondDf[IndexRow, IndexCol]))
      if (Condition == "PEOE"){
        PEOE = c(PEOE, df[i,j])
      }
      else if (Condition == "Raw"){
        Raw = c(Raw, df[i,j])
      }
      else if (Condition == "Text"){
        Text = c(Text, df[i,j])
      }
    }
  }
  return(data.frame(PEOE, Raw, Text, None))
}

CondScoresBefore = Get_Cond_Scores(TopicBefore)
rownames(CondScoresBefore) = names(TopicBefore)

CondScoresAfter = Get_Cond_Scores(TopicAfter)
rownames(CondScoresAfter) = names(TopicAfter)

CondScoresBefore = CondScoresBefore[ order((row.names(CondScoresBefore))), ]
CondScoresBefore$Avg = rowSums(CondScoresBefore)/ncol(CondScoresBefore)


CondScoresAfter = CondScoresAfter[ order((row.names(CondScoresAfter))), ]

CondScoresCorr = data.frame(cor(CondScoresAfter$PEOE, CondScoresBefore$Avg), 
           cor(CondScoresAfter$Raw, CondScoresBefore$Avg), 
           cor(CondScoresAfter$Text, CondScoresBefore$Avg),
           cor(CondScoresAfter$None, CondScoresBefore$Avg))

names(CondScoresCorr) = c("PEOE", "Raw", "Text", "None")
rownames(CondScoresCorr) = c("Correlation")

CondScoresCorr
```

# Relative Improvement Across Conditions
Note here the formula used is: (score improvement)/(100 - initial score)
```{r}
CondRI = CondChange

start = data.frame(t(CondScoresBefore))
end = data.frame(t(CondScoresAfter))

for(i in 1:nrow(CondRI)){
  for(j in 2:ncol(CondRI)){
    #Find this person's pre and post scores given ID and condition
    ID = names(CondRI)[j]
    Condition = CondRI$`Condition/ID`[i]
    preScore = start[which(rownames(start) == Condition), 
                     which(substr(names(start), 2, nchar(names(start))) == ID)]
    endScore = end[which(rownames(end) == Condition), 
                   which(substr(names(end), 2, nchar(names(end))) == ID)] 
    #Calculate relative improvement :)
    r.improve = (endScore - preScore) / (100 - preScore)
    CondRI[i,j] = r.improve
  }
}

CondRI
```

# Randomization Check
Here we will check if the "Order" node has performed correctly. To check that, we will look at the distribution of the way in which our conditions were ordered. Possible combinations include: "ABC", "ACB", "CAB", "BCA", "CBA", and "BAC". 
```{r}
randList = c()
orderList = c("order.srz4", "order.35h7", "order.wmgj", "order.2g48", 
              "order.wxwl", "order.7v6m", "order.n645",
              "order.wv2s", "order.9qsx", "order.f4am", "order.xmzm",
              "order.p56u")
for (i in 1:length(condList)){
    tmp = split(condList[[i]], condList[[i]]$Participant.Private.ID)
    randList = c(randList, sapply(tmp, function(x){
    randNames = names(x)
    for (j in 1:length(orderList)){
      Index = which(randNames == orderList[j])
      if (!is.na(x[1, Index])){
        return (x[1, Index])
      }
    }
   }))
}

table(randList)
```
