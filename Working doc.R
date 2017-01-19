panss.data1 <- read.csv("panss-data.csv", stringsAsFactors=FALSE)
patient.list<-unique(panss.data$id)

####

onsetagedf <- as.data.frame(cbind(id = panss.data$id, onset = panss.data$onsetage))
onsetagedf <- onsetagedf[!duplicated(onsetagedf$id),]
onsetagedf <- na.omit(onsetagedf)

onset.hist <- hist(onsetagedf$onset, plot = FALSE)

hist(onsetagdf$onset) <- hist(onsetagedf$onset, xlab="Age at Symptom Onset in Years", 
     ylab="Number of Patients", main="Age at Symptom Onset (523 Patients)", xlim=c(0,45), 
     xaxt="n", labels=TRUE, col=ifelse(onset.hist$breaks < input$age, "#adebad", "forestgreen"))
axis(side = 1, at = seq(0, 45, by = 5), labels = TRUE, tcl = -0.2)

h <- hist(onsetagedf$onset, xlab="Age at Symptom Onset in Years", ylab="Number of Patients",
         main="Age at Symptom Onset (523 Patients)", xlim=c(0,45), xaxt="n", labels=TRUE)
         col=ifelse(onset.hist$breaks < input$age, "#adebad", "forestgreen"))
h$density = h$counts/sum(h$counts)*100
plot(h, xlab="Age at Symptom Onset in Years", ylab="Percentage of Patients",
     main="Age at Symptom Onset (523 Patients)", freq=FALSE)
OnsetAgeinput <-  panss.data$onsetage[which(panss.data$onsetage == input$age)]


  ####

  # Make a dataframe with one record per patient
  pt.data<-data.frame(id=unique(panss.data$id))
  
  # How many patients do we have?
  (n<-dim(pt.data)[1])
  
  
  ### More about text
  p("Our app is designed using a data set obtained from Marder, M.D. and Meibach, Ph.D’s clinical trial evaluating the effectiveness of risperidone in treating patients with schizophrenia."),
  br(),
  p("Variables provided include:"),
  p(tags$b(" •	id"), "assigns each patient with a unique id number"),
  p(tags$b(" •	pospan"), "gives the patient’s score for positive symptoms"),
  p(tags$b(" •	negpan"), "gives the patient’s score for negative symptoms"), 
  p(tags$b(" •	genpan"), "gives the patient’s score for general symptoms"),
  p(tags$b(" •	totpan"), "gives the patient’s total score for symptoms (total of pospan, negpan, and genpan)"),
  p(tags$b(" •	treatment"), "indicates the assigned treatment group for the patient, either PLACEBO, RISPERIDONE_2MG, RISPERIDONE_6MG, RISPERIDONE_10MG, RISPERIDONE_16MG, or HALOPERIDOL. These groups correspond to differing medications and dosages"),
  p(tags$b(" •	time"), "refers to the week in the study, receiving a value between -1 and 8, with -1 corresponding to the week prior to the start of treatment"),
  p(tags$b(" •	age"), "refers to the age of the patient at baseline and is consistent throughout the duration of the study"),
  p(tags$b(" •	onsetage"), "gives the age at which schizophrenic symptoms were first experienced"),
  p(tags$b(" •	firsthospage"), "gives the age at the patient’s first hospitalization for schizophrenic symptoms"),
  p(tags$b(" •	sex"), "gives the patient’s gender, either Male or Female"),
  p(tags$b(" •	race"), "gives the patient’s race, either White, Oriental, Polynesian/East Asian, Black, Hispanic, American Indian"),
  br(),
  
  
  ### Working on pie charts for race, gender, age
  
  output$racepie <- renderPlot({
    race_table <- as.data.frame(cbind(id = panss.data$id, race = panss.data$race))
    race_table <- race_table[!duplicated(race_table$id),]
    race_table <- na.omit(race_table)
      
    racepie = race_table$race
    race.freq = table(race_table$race)
    pielabels = c("American Indian", "Black", "Hispanic", "Oriental", "Polynesian/East Asian", "White")
    pie(race.freq, labels=pielabels, main = "Race Distribution of Study Participants", cex = 1.0)
  })
  
  output$sexpie <- renderPlot({
    sex_table <- as.data.frame(cbind(id = panss.data$id, sex = panss.data$sex))
    sex_table <- sex_table[!duplicated(sex_table$id),]
    sex_table <- na.omit(sex_table)
    
    sexpie = sex_table$sex
    sex.freq = table(sex_table$sex)
    piecolors = c("pink", "lightblue")
    piepercent <- round(100*sex.freq/sum(sex.freq),1)
    pie(sex.freq, labels = c("Female", "Male"), col = piecolors, 
       main = "Sex Distribution of Study Participants", cex = 1.0)
    })
  
  ### 
  output$agepie <- renderPlot({
    age_table <- as.data.frame(cbind(id = panss.data$id, agedf = panss.data$age))
    age_table <- age_table[!duplicated(agedf$id),]
    
    Age0to15 <- subset(age_table, as.numeric(age_table$age) > 0 & as.numeric(age_table$age) <= 15, select = c(id, age))
    Age15to30 <- subset(age_table, as.numeric(age) > 15 & as.numeric(age) <=30, select = c(id, age))
    Age30to45 <- subset(age_table, as.numeric(age) > 30 & as.numeric(age) <=45, select = c(id, age))
    Age45to65 <- subset(age_table, as.numeric(age) > 45 & as.numeric(age) <=65, select = c(id, age))
    
    AgeGroup1 <- data.frame(group = "0-15 Years", value = Age0to15$age)
    AgeGroup2 <- data.frame(group = "15-30 Years", value = Age15to30$age)
    AgeGroup3 <- data.frame(group = "30-45 Years", value = Age30to45$age)
    AgeGroup4 <- data.frame(group = "45-65 Years", value = Age45to65$age)
    
    age.groups <- rbind(AgeGroup1, AgeGroup2, AgeGroup3, AgeGroup4)
    
    agepie = age.groups$age
    age.freq = table(age.groups$age)
    agepercent <- round(100*age.freq/sum(age.freq),1)
    pie(age.freq, main = "Age Distribution of Study Participants", cex = 1.0)
    
  })
  

  ###
  
  output$BMItext <- renderText({
    
    for(i in 1:154){
      if(!(input$treatment %in% panss.data$treatment)){
        output <- c(" ")
      }else{
        if(input$treatment == "Placebo"){
          output <- c("Patients taking placebo reported an average PANSS score reduction of", ... )
        }}
    }
    paste0(output)
  })
  
  
  
  
  
  panss.data <- as.data.frame(panss.data)
  patient.list<-unique(panss.data$id)
  
  belowonset_df <- cbind(id = panss.data$id, onsetage = panss.data$onsetage)
  belowonset_df <- as.data.frame(belowonset_df)
  belowonset_df <- belowonset_df[!duplicated(belowonset_df$id),]
  belowonset_df <- na.omit(belowonset_df)
  
  belowonset_df <- subset(belowonset_df, as.numeric(onsetage) > input$age, select=c(id, onsetage))
  percentbelow <-  round(100*belowage_table/sum(belowage_table),1)
  
  # Reactive text
  
  output$onsetagetext <- renderText({
    
    belowdf <- onsetagedf$onset < input$age
    
    for(i in 1:523){
      if(!(input$age %in% panss.data$onsetage)){
        output <- c("Sorry, but this information is not in the dataset!")
      }else{
        if(input$age > panss.data$onsetage){
          output <- c("Approximately", #round(100*XX/sum(XX),1),"of patients exhibited first symptoms at a younger age than you.")
        }}
    }
    paste0(output)
    
  })
          
          library(ggplot2)
          
          onsetagedf <- as.data.frame(cbind(id = panss.data$id, onset = panss.data$onsetage))
          onsetagedf <- onsetagedf[!duplicated(onsetagedf$id),]
          onsetagedf <- na.omit(onsetagedf)
  
          OnsetAgefreq <- ggplot(data = onsetagedf, aes(onsetagedf$onset)) + 
            geom_histogram(binwidth = 7, ylim=c(0,250), xlim=c(0,45), colour="black", fill="cyan") + 
            labs(x = "Age at Symptom Onset in Years",
                 y = "Number of Patients")
          OnsetAgefreq
  
  
          
          
          #Univariate Visualization of Onset Age
          
          output$onsetageplot <- renderPlot({
            
            onsetagedf <- as.data.frame(cbind(id = panss.data$id, onset = panss.data$onsetage))
            onsetagedf <- onsetagedf[!duplicated(onsetagedf$id),]
            onsetagedf <- na.omit(onsetagedf)
            
            onset.hist <- hist(onsetagedf$onset, breaks=seq(0,250,5), plot = FALSE)
            
            hist(onsetagedf$onset, breaks=seq(0,250,5), xlab="Age at Symptom Onset in Years",
                 ylab="Number of Patients" , main="Age at Symptom Onset (523 Patients)", ylim=c(0,250), xlim=c(0,45),
                 xaxt="n", labels=TRUE, col=ifelse(onset.hist$breaks < input$age, "#adebad", "forestgreen"))
            axis(side = 1, at = seq(0, 45, by = 5), labels = TRUE, tcl = -0.2)
            OnsetAgeinput <-  panss.data$onsetage[which(panss.data$onsetage == input$age)]
          })
          
  
  