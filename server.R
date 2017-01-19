#Final  Project
#Mental Health Group

#Load ggplot2 library
library(shiny)
library(ggplot2)
library(lme4)
library(arm)

shinyServer(function(input,output){
  
  #Load Dataset
  panss.data <- read.csv("panss-data.csv", stringsAsFactors=FALSE)
  panss.data <- as.data.frame(panss.data)
  patient.list<-unique(panss.data$id)
  
  #Overview of study participants - pie chart for sex
  
  output$sexpie <- renderPlot({
    sex_table <- as.data.frame(cbind(id = panss.data$id, sex = panss.data$sex))
    sex_table <- sex_table[!duplicated(sex_table$id),]
    sex_table <- na.omit(sex_table)

    sexpie = sex_table$sex
    sex.freq = table(sex_table$sex)
    piecolors = c("pink", "lightblue")
    piepercent <- round(100*sex.freq/sum(sex.freq),1)
    pie(sex.freq, labels = c("Female", "Male"), col = piecolors, 
        main = "Sex Distribution of Study Participants", cex = .7)
    })
  
  #Overview of study participants - pie chart for race
  
  output$racepie <- renderPlot({
    race_table <- as.data.frame(cbind(id = panss.data$id, race = panss.data$race))
    race_table <- race_table[!duplicated(race_table$id),]
    race_table <- na.omit(race_table)
    
    racepie = race_table$race
    race.freq = table(race_table$race)
    pielabels = c("American Indian", "Pacific Islander", "Asian", "Hispanic", "Black", "White")
    pie(sort(race.freq), labels=pielabels, main = "Race Distribution of Study Participants", cex = .7)
  })

  
  #Univariate Visualization of Onset Age
 
  onsetagedf <- as.data.frame(cbind(id = panss.data$id, onset = panss.data$onsetage))
  onsetagedf <- onsetagedf[!duplicated(onsetagedf$id),]
  onsetagedf <- na.omit(onsetagedf)
  
  output$onsetageplot <- renderPlot({
    
    onset.hist <- hist(onsetagedf$onset, breaks=seq(0,250,5), plot = FALSE)
    
    h <- hist(onsetagedf$onset)
    h$density = h$counts/sum(h$counts)*100
    plot(h, xlab="Age at Symptom Onset in Years", ylab="Percentage of Patients",
       main="Age at Symptom Onset (523 Patients)", xlim=c(0,45), freq=FALSE, 
       col=ifelse(onset.hist$breaks < input$age, "#adebad", "forestgreen"))
    grid()
    OnsetAgeinput <-  panss.data$onsetage[which(panss.data$onsetage == input$age)]
  
      })
  

  agecount <- as.vector(onsetagedf$onset)
  countdf <- rle(sort(agecount))
  countdf <- data.frame(age = countdf$values, n = countdf$lengths)
  
  for (i in 1:60){
    
    if (i %in% countdf$age){
    } else {
        countdf<- rbind(countdf, c(i, 0))
    }} 
  
  countdf <- countdf[order(countdf$age),]
  

  
  
output$onsetagetext <- renderText({
  
  numage <- as.numeric(input$age)
  numerator <- 0
  
  for (i in 1: numage) {
    
    numerator <- countdf[i,2] + numerator
  }
  
  denominator <- sum(countdf$n)
  
  percentage <- (numerator / denominator) * 100
  
  percentage <- round(percentage, 2)
  
  paste0(percentage, "% of patients are ", numage, " years or younger.")
  
})
  
  #Univariate Visualization of firsthospage

firsthopdf <- as.data.frame(cbind(id = panss.data$id, firsthop = panss.data$firsthospage))
firsthopdf <- firsthopdf[!duplicated(firsthopdf$id),]
firsthopdf <- na.omit(firsthopdf)

  output$firsthospageplot <- renderPlot({
    

    
    firsthosp.hist <- hist(firsthopdf$firsthop, breaks=seq(0,200,5), plot = FALSE)
    
    h <- hist(firsthopdf$firsthop)
    h$density = h$counts/sum(h$counts)*100
    plot(h, xlab="Age at First Hospitalization in Years", ylab="Percentage of Patients",
         main="Age at First Hospitalization (523 Patients)", xlim=c(0,60), freq=FALSE,
    col=ifelse(firsthosp.hist$breaks < input$age,"#ffc0b6", "#ff5336"))
    grid()
    Firsthospageinput <-  panss.data$firsthospage[which(panss.data$firsthospage == input$age)]
    #  abline(v = Firsthospageinput,col = "black",lty = 2, lwd=5)
  })
  
  
  
 hospcount <- as.vector(firsthopdf$firsthop)
 counthopdf <- rle(sort(hospcount))
 counthopdf <- data.frame(hopage = counthopdf$values, n = counthopdf$lengths)
  
  for (i in 1:60){
    
    if (i %in% counthopdf$hopage){
    } else {
      counthopdf<- rbind(counthopdf, c(i, 0))
    }} 
  
  counthopdf <- counthopdf[order(counthopdf$hopage),]
  
  output$firsthoptext <- renderText({
    
    numage <- as.numeric(input$age)
    numerator <- 0
    
    for (i in 1: numage) {
      
      numerator <- counthopdf[i,2] + numerator
    }
    
    denominator <- sum(counthopdf$n)
    
    percentage <- (numerator / denominator) * 100
    
    percentage <- round(percentage, 2)
    
    paste0(percentage, "% of patients are first hospitalized at ", numage, " years old or younger.")
    
  })
  
  #Mulitvariate Visualization of Sex vs Onset Age 
  
  # Color distinguishes gender 
  # User input sex and onset age - vertical bar shows input
  
  onset_sex_df <- cbind(id = panss.data$id, onsetage = panss.data$onsetage, sex = panss.data$sex)
  onset_sex_df <- as.data.frame(onset_sex_df)
  onset_sex_df <- onset_sex_df[!duplicated(onset_sex_df$id),]
  onset_sex_df <- na.omit(onset_sex_df)
  
  #Convert onset age from Factor to Numeric
  
  onset_sex_df <- within(onset_sex_df,{
    onsetage <- as.numeric(as.character(onsetage))
  })
  
 # create dataframe for individual gender
  onset_male_df <- onset_sex_df[onset_sex_df$sex =='MALE',]
  onset_female_df <- onset_sex_df[onset_sex_df$sex == "FEMALE",]
  
  malecount <- as.vector(onset_male_df$onsetage)
  malecount <- rle(sort(malecount))
  malecountdf <- data.frame(onsetage = malecount$values, n = malecount$lengths)
  
  for (i in 1:60){
    
    if (i %in% malecountdf$onsetage){
    } else {
      malecountdf<- rbind(malecountdf, c(i, 0))
    }} 
  
  malecountdf <- malecountdf[order(malecountdf$onsetage),]
  
  femalecount <- as.vector(onset_female_df$onsetage)
  femalecount <- rle(sort(femalecount))
  femalecountdf <- data.frame(onsetage = femalecount$values, n = femalecount$lengths)
  
  for (i in 1:60){
    
    if (i %in% femalecountdf$onsetage){
    } else {
      femalecountdf<- rbind(femalecountdf, c(i, 0))
    }} 
  
  femalecountdf <- femalecountdf[order(femalecountdf$onsetage),]
  
  output$maletext <- renderText({
    
    numage <- as.numeric(input$age2)
    numerator <- 0
    
    for (i in 1: numage) {
      
      numerator <- malecountdf[i,2] + numerator
    }
    
    denominator <- sum(malecountdf$n)
    
    percentage <- (numerator / denominator) * 100
    
    percentage <- round(percentage, 2)
    
    paste0(percentage, "% of males")
    
  })
  
  output$femaletext <- renderText({
    
    numage <- as.numeric(input$age2)
    numerator2 <- 0
    
    for (i in 1: numage) {
      
      numerator2 <- femalecountdf[i,2] + numerator2
    }
    
    denominator2 <- sum(femalecountdf$n)
    
    percentage2 <- (numerator2 / denominator2) * 100
    
    percentage2 <- round(percentage2, 2)
    
    paste0(percentage2, "% of females")
    
  })
  
  output$agetext <- renderText({
    numage <- as.numeric(input$age2)
    paste0(numage)
  }
    
  )
  
  
  
  #display onset vs gender frequencies, dependent on user selection
  output$genderonsetplot <- renderPlot({
    
    gender_onset_plot <- ggplot(onset_sex_df, aes(x= onsetage)) + geom_density(aes(group = sex, color = sex, fill = sex), alpha = 0.6) 
    gender_onset_plot <- gender_onset_plot + scale_fill_manual(values = c('pink', 'lightblue'))
    gender_onset_plot <- gender_onset_plot + ggtitle('Age of Symptom Onset by Gender') + labs(x = 'Onset Age in Years', y = 'Density' )
    
   # male_onset_plot <-ggplot(onset_male_df, aes(x= onsetage)) + geom_density(color = "lightblue", fill = "lightblue") 
   # male_onset_plot <- male_onset_plot + ggtitle('Age of Symptom Onset for Males') + labs( x = 'Onset Age in Years', y = "Density")
    
   # female_onset_plot <- ggplot(onset_female_df, aes(x= onsetage)) + geom_density(color = "pink", fill = "pink")
   # female_onset_plot <- female_onset_plot + ggtitle('Age of Symptom Onset for Femles') + labs( x = 'Onset Age in Years', y = "Density")
    
    #female_onset_plot + geom_vline(xintercept = input$age2)
  #  male_onset_plot + geom_vline(xintercept = input$age2)
    gender_onset_plot + geom_vline(xintercept = input$age2) + theme_bw() + theme(plot.title = element_text(face = "bold"))
    
  })
  
  #Multivariate Visualization of Treatment vs Change in Total PANSS Score for full-time patients
  
  output$treatmentchangePANSSplot <- renderPlot({
    par(mar = c(5,5,5,5) + 0.1)
    boxplot(totpanchange ~ treatment, at=c(2,1,4,3,5,6),
            data=panss.data, main="Change in Total PANSS Score Based on Treatment",
            ylab="Change in Total PANSS Score", xlab="Treatment", las=2, cex.axis = .6,
            names=c("Haloperidol","Placebo", "Risperidone 16mg", "Risperidone 10mg", "Risperidone 2mg", "Risperidone 6mg"),horizontal=FALSE,
            col = if(input$treatment == "Placebo") {
              c("white", "skyblue1", "white", "white", "white", "white")
            } else if (input$treatment == "Haloperidol") {
              c("skyblue1", "white", "white", "white", "white", "white")
            } else if (input$treatment == "Risperidone_2mg") {
              c("white", "white", "white", "white", "skyblue1", "white")
            } else if (input$treatment == "Risperidone_6mg")  {
              c("white", "white", "white", "white", "white", "skyblue1")
            } else if (input$treatment == "Risperidone_10mg") {
              c("white", "white", "white", "skyblue1", "white", "white")
            } else if (input$treatment == "Risperidone_16mg") {
              c("white", "white", "skyblue1", "white", "white", "white")
            }    
    )
    grid(nx = NULL)
    
  })
  
  output$treatmentchangePANSStext <- renderText({
       if(input$treatment == "Placebo") {
         output <- c("Patients taking the placebo for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "PLACEBO"])), "points.")
       } else if (input$treatment == "Haloperidol") {
         output <- c("Patients taking haloperidol for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "HALOPERIDOL"])), "points.")
       } else if (input$treatment == "Risperidone_2mg") {
         output <- c("Patients taking 2mg of risperidone for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "RISPERIDONE_2MG"])), "points.")
       } else if (input$treatment == "Risperidone_6mg") {
         output <- c("Patients taking 6mg of risperidone for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "RISPERIDONE_6MG"])), "points.")
       } else if (input$treatment == "Risperidone_10mg") {
        output <- c("Patients taking 10mg of risperidone for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "RISPERIDONE_10MG"])), "points.")
       } else if (input$treatment == "Risperidone_16mg") {
         output <- c("Patients taking 16mg of risperidone for eight weeks experienced a median change in PANSS score of", median(na.omit(panss.data$totpanchange[panss.data$treatment == "RISPERIDONE_16MG"])), "points.")
        }    
      paste0(output)
  })
  
  
  # preparation for regression output 
  
  weeks <- c(-1,0,1,2,4,6,8)
  ordered.panss.data <- panss.data[with(panss.data, order(id, time)), ]
  
  ## treatment IDs
  # PLACEBO = 0
  # HALOPERIDOL = 1
  # RISPERIDONE_2MG = 2
  # RISPERIDONE_6MG = 3
  # RISPERIDONE_10MG = 4
  # RISPERIDONE_16MG = 5
  
  ordered.panss.data$treatmentID <- vector(length = 2992)
  for (i in 1:2992) {
    if (ordered.panss.data$treatment[i] == "PLACEBO") {
      ordered.panss.data$treatmentID[i] = 0
    } else if (ordered.panss.data$treatment[i] == "HALOPERIDOL") {
      ordered.panss.data$treatmentID[i] = 1
    } else if (ordered.panss.data$treatment[i] == "RISPERIDONE_2MG") {
      ordered.panss.data$treatmentID[i] = 2
    } else if (ordered.panss.data$treatment[i] == "RISPERIDONE_6MG") {
      ordered.panss.data$treatmentID[i] = 3
    } else if (ordered.panss.data$treatment[i] == "RISPERIDONE_10MG") {
      ordered.panss.data$treatmentID[i] = 4 
    } else if (ordered.panss.data$treatment[i] == "RISPERIDONE_16MG") {
      ordered.panss.data$treatmentID[i] = 5
    } else {
    }
  }
  
  colordf <- ordered.panss.data[,c(2,7,15)]
  colordf <- colordf[!duplicated(colordf$id),]
  (n <- dim(colordf)[1])
  colordf$lastfollowup <- vector(length = n)
  
  for (i in 1:n){
    colordf$lastfollowup[i] <- max(panss.data$time[panss.data$id == colordf$id[i]  ])}
  
  colordf<- colordf[colordf$lastfollowup == 8,]
  
  weekly.df <- as.data.frame(cbind(ordered.panss.data$id, ordered.panss.data$onsetage,
                                   ordered.panss.data$treatmentID, ordered.panss.data$time,
                                   ordered.panss.data$totpan))
  colnames(weekly.df) <- c("id", "onsetage", "treatment", "time", "totpan")
  
  # multilevel regression output
  
  output$my.regression <- renderPlot({

    if(input$my.treatment == "none") {
      my.treatmentID = 0}
    if(input$my.treatment == "haloperidol") {
      my.treatmentID = 1}
    if(input$my.treatment == "risperidone (2mg)") {
      my.treatmentID = 2}
    if(input$my.treatment == "risperidone (6mg)") {
      my.treatmentID = 3}
    if(input$my.treatment == "risperidone (10mg)") {
      my.treatmentID = 4}
    if(input$my.treatment == "risperidone (16mg)") {
      my.treatmentID = 5}
    
    plot(x=0, y=mean(ordered.panss.data$totpan), type="n", xlim=c(-1,8), ylim=c(30,150), xlab="Week", ylab="Total PANSS Score", main = "Antipsychotic Treatment Outcome Predictions Over Eight Weeks")

    for (i in 1:270){
      if (colordf[i,3] == my.treatmentID){
        lines(ordered.panss.data$totpan[ordered.panss.data$id == colordf[i,1]]~ordered.panss.data$time[ordered.panss.data$id==colordf[i,1]],
              col = if (colordf[i,3] == 0 ) {
                "pink"
              } else if (colordf[i,3] == 1){
                "lightsalmon"
              } else if (colordf[i,3]== 2){
                "skyblue1"
              } else if (colordf[i,3] == 3){
                "#a6f2a6"
              } else if (colordf[i,3]== 4){
                "#f8c968"
              } else {"#afeeee"}
        )}   
      }
    grid(nx = NULL)
    recorded_plot <- recordPlot()
    
      numset <- input$numind
      indset <- c(input$ninput1, 
                  input$ninput2, 
                  input$ninput3, 
                  input$ninput4, 
                  input$ninput5,
                  input$ninput6)
      inputset <- c()
      for (i in 1:numset) {
        inputset <- c(inputset, indset[i])
      }
    
    
    
    n = 752
    newrows<-data.frame(id=rep(n+1,input$numind))
    newrows$onsetage<-rep(input$my.onsetage,input$numind)
    newrows$treatment<-rep(my.treatmentID, input$numind)
    newrows$time<-weeks[1:input$numind]
    newrows$totpan<-inputset
    
    weekly.df.new<-as.data.frame(rbind(weekly.df, newrows))
    
    predictrows<-data.frame(id = rep(n+1,(7-numset)))
    predictrows$onsetage<-rep(input$my.onsetage,(7-numset))
    predictrows$treatment<-rep(my.treatmentID, (7-numset))
    predictrows$time<-weeks[(numset+1):length(weeks)]
    
    panss.rint <- lmer(totpan ~ time + onsetage + treatment + (1 | id), data = weekly.df.new, REML = FALSE)
    sigma_rint <- sigma.hat(panss.rint)$sigma$data
    
    last.row<-dim(coef(panss.rint)$id)[1]
    coef_rint <- as.numeric(coef(panss.rint)$id[last.row,])
    names(coef(panss.rint)$id)
    
    matrix_rint <- as.matrix(cbind(1, predictrows$time, predictrows$onsetage, predictrows$treatment))
    
    n.sims<-10000
    sim_values_rint<-matrix(apply(matrix_rint%*%coef_rint, 1, rnorm, n=n.sims, sd=sigma_rint), nrow=n.sims, ncol=length(predictrows$time), byrow=FALSE)
    fit_rint<-apply(sim_values_rint,2,mean) #mean
    lwr_rint<-apply(sim_values_rint,2,quantile, p=0.025) #lower 2.5%
    upr_rint<-apply(sim_values_rint,2,quantile, p=0.975) #upper 97.5%
    
    lines(weekly.df.new$totpan[weekly.df.new$id==(n+1)] ~ weekly.df.new$time[weekly.df.new$id==(n+1)], col= "black", lty="solid", lwd=2)
    polygon( c(predictrows$time, rev(predictrows$time)), c(lwr_rint, rev(upr_rint)), col=rgb(0,75,100,50, maxColorValue = 255), border=NA )
    lines(fit_rint~predictrows$time, col="black", lty="dotted", lwd=2)
    
    
  })
  
  output$explanation.regression <- renderText({
    paste0("The above plot shows your predicted treatment outcomes. The colored lines show the symptom progressions for all the patients that were on your selected antipsychotic treatment for the full eight weeks of the study. The black solid line shows the PANSS scores that you inputted. The black dotted line shows predicted PANSS scores based on our model. The shaded region shows the range of PANSS scores we can expect to see at each time.")
  })
  
  output$my.regressiontext <- renderText({
    if(input$my.treatment == "none") {
      my.treatmentID = 0}
    if(input$my.treatment == "haloperidol") {
      my.treatmentID = 1}
    if(input$my.treatment == "risperidone (2mg)") {
      my.treatmentID = 2}
    if(input$my.treatment == "risperidone (6mg)") {
      my.treatmentID = 3}
    if(input$my.treatment == "risperidone (10mg)") {
      my.treatmentID = 4}
    if(input$my.treatment == "risperidone (16mg)") {
      my.treatmentID = 5}
    
    numset <- input$numind
    indset <- c(input$ninput1, 
                input$ninput2, 
                input$ninput3, 
                input$ninput4, 
                input$ninput5,
                input$ninput6)
    inputset <- c()
    for (i in 1:numset) {
      inputset <- c(inputset, indset[i])
    }
    
    n = 752
    newrows<-data.frame(id=rep(n+1,input$numind))
    newrows$onsetage<-rep(input$my.onsetage,input$numind)
    newrows$treatment<-rep(my.treatmentID, input$numind)
    newrows$time<-weeks[1:input$numind]
    newrows$totpan<-inputset
    
    weekly.df.new<-as.data.frame(rbind(weekly.df, newrows))
    
    predictrows<-data.frame(id = rep(n+1,(7-numset)))
    predictrows$onsetage<-rep(input$my.onsetage,(7-numset))
    predictrows$treatment<-rep(my.treatmentID, (7-numset))
    predictrows$time<-weeks[(numset+1):length(weeks)]
    
    panss.rint <- lmer(totpan ~ time + onsetage + treatment + (1 | id), data = weekly.df.new, REML = FALSE)
    sigma_rint <- sigma.hat(panss.rint)$sigma$data
    
    last.row<-dim(coef(panss.rint)$id)[1]
    coef_rint <- as.numeric(coef(panss.rint)$id[last.row,])
    names(coef(panss.rint)$id)
    
    matrix_rint <- as.matrix(cbind(1, predictrows$time, predictrows$onsetage, predictrows$treatment))
    
    n.sims<-10000
    sim_values_rint<-matrix(apply(matrix_rint%*%coef_rint, 1, rnorm, n=n.sims, sd=sigma_rint), nrow=n.sims, ncol=length(predictrows$time), byrow=FALSE)
    fit_rint<-apply(sim_values_rint,2,mean) #mean
    lwr_rint<-apply(sim_values_rint,2,quantile, p=0.025) #lower 2.5%
    upr_rint<-apply(sim_values_rint,2,quantile, p=0.975) #upper 97.5%

    paste0("Based on your given age at symptom onset of ", input$my.onsetage, ", your selected treatment of ", input$my.treatment, " and your previous PANSS scores, your expected PANSS score at the end of eight weeks is ", round(tail(fit_rint, n=1)), " (95% prediction interval: ", round(tail(lwr_rint, n=1)), ", ", round(tail(upr_rint,n=1)), ").")
  })
  output$tablestatement <- renderText({

    if (input$numind == 6) {
    } else {
      paste0("Predictions for all weeks are given in the table below.")
    } 
    
  })
  output$my.regressiontable <- renderTable({
    
    if(input$my.treatment == "none") {
      my.treatmentID = 0}
    if(input$my.treatment == "haloperidol") {
      my.treatmentID = 1}
    if(input$my.treatment == "risperidone (2mg)") {
      my.treatmentID = 2}
    if(input$my.treatment == "risperidone (6mg)") {
      my.treatmentID = 3}
    if(input$my.treatment == "risperidone (10mg)") {
      my.treatmentID = 4}
    if(input$my.treatment == "risperidone (16mg)") {
      my.treatmentID = 5}
    
    numset <- input$numind
    indset <- c(input$ninput1, 
                input$ninput2, 
                input$ninput3, 
                input$ninput4, 
                input$ninput5,
                input$ninput6)
    inputset <- c()
    for (i in 1:numset) {
      inputset <- c(inputset, indset[i])
    }
    
    n = 752
    newrows<-data.frame(id=rep(n+1,input$numind))
    newrows$onsetage<-rep(input$my.onsetage,input$numind)
    newrows$treatment<-rep(my.treatmentID, input$numind)
    newrows$time<-weeks[1:input$numind]
    newrows$totpan<-inputset
    
    weekly.df.new<-as.data.frame(rbind(weekly.df, newrows))
    
    predictrows<-data.frame(id = rep(n+1,(7-numset)))
    predictrows$onsetage<-rep(input$my.onsetage,(7-numset))
    predictrows$treatment<-rep(my.treatmentID, (7-numset))
    predictrows$time<-weeks[(numset+1):length(weeks)]
    
    panss.rint <- lmer(totpan ~ time + onsetage + treatment + (1 | id), data = weekly.df.new, REML = FALSE)
    sigma_rint <- sigma.hat(panss.rint)$sigma$data
    
    last.row<-dim(coef(panss.rint)$id)[1]
    coef_rint <- as.numeric(coef(panss.rint)$id[last.row,])
    names(coef(panss.rint)$id)
    
    matrix_rint <- as.matrix(cbind(1, predictrows$time, predictrows$onsetage, predictrows$treatment))
    
    n.sims<-10000
    sim_values_rint<-matrix(apply(matrix_rint%*%coef_rint, 1, rnorm, n=n.sims, sd=sigma_rint), nrow=n.sims, ncol=length(predictrows$time), byrow=FALSE)
    fit_rint<-apply(sim_values_rint,2,mean) #mean
    lwr_rint<-apply(sim_values_rint,2,quantile, p=0.025) #lower 2.5%
    upr_rint<-apply(sim_values_rint,2,quantile, p=0.975) #upper 97.5%
    
    outputtable<-as.table(cbind(round(fit_rint),round(lwr_rint),round(upr_rint)))
    for(i in 1:length(predictrows$time)){
      rownames(outputtable)[i] <- paste0("Week ", predictrows$time[i])
      
      if (predictrows$time[i] == -1) {
        rownames(outputtable)[i] = "Week prior to treatment"
      } else if (predictrows$time[i] == 0) {
        rownames(outputtable)[i] = "Start of treatment"
      } else {
        rownames(outputtable)[i] = paste0("Week ", predictrows$time[i], " of treatment")
      }
    }
    colnames(outputtable)<-c("Mean Prediction", "Lower Bound", "Upper Bound")
    if (input$numind == 6) {
    } else {
      print(outputtable, row.names = FALSE)
    }
  })
  
  output$ninputs <- renderUI({
    weeks <- c(-1,0,1,2,4,6,8)
    numind <- as.integer(input$numind)
    lapply(1:numind, function(i) {
      sliderInput(inputId = paste0("ninput", i), 
                  if (i == 1) {
                    label = "Week prior to treatment"
                  } else if (i == 2) {
                    label = "Week of start of treatment"
                  } else {
                      label = paste0("Week ", weeks[i], " of treatment")
                      }, min = 31, max = 161, value = 83, step = 1)
    
    
  })
})
  
  output$information.table <- renderTable({
    study.length <- c("Have not started treatment", "Just started treatment", "1 week into treatment", "2 weeks into treatment", "4 weeks into treatment", "6 weeks into treatment", "8 weeks into treatment")
    #weeks <- c("-1", "-1, 0", "-1, 0, 1", "-1, 0, 1, 2", "-1, 0, 1, 2, 4", "-1, 0, 1, 2, 4, 6", "-1, 0, 1, 2, 4, 6, 8")
    available.data <- c(1,2,3,4,5,6,7)
    info.table <- as.table(cbind(study.length, available.data)) #, weeks))
    row.names(info.table) <- NULL
    colnames(info.table) <- c("Where are you in your treatment regiment?", "Number of available PANSS scores to input") #, "Week number(s)")
    print(info.table, row.names = FALSE)
    })
  
})