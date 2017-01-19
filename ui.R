library(shiny)
library(ggplot2)

shinyUI(navbarPage("Exploring Antipsychotic Treatment Outcomes for Patients with Schizophrenia",
                   tabPanel("The Clinical Trial",
                              mainPanel(width = 12,
                                br(),
                                p(tags$b("The Original Clinical Trial")),
                                p("In June 1994, The American Journal of Psychiatry published the results of an eight-week clinical trial, led by Stephen R. Marder, M.D. and Richard C. Meibach, Ph.D., evaluating risperidone in the treatment of schizophrenia."),
                                p("Schizophrenia is a chronic mental disorder that influences how a person thinks, feels and behaves. Symptoms are usually detected between ages 16 and 30, and are categorized into positive, negative and general subsets."),
                                p("Positive symptoms include delusions, hallucinations, grandiosity, suspiciousness and hostility. Negative symptoms include emotional and social withdrawal, difficulty in abstract thinking, lack of spontaneity and conversation flow, and stereotyped thinking. General symptoms include anxiety, depression, uncooperativeness, unusual thoughts, disorientation, poor attention span, lack of judgment and insight, poor impulse control, and active efforts to avoid social interactions."),
                                p("The exact causes of schizophrenia are unknown. Patients diagnosed with schizophrenia are most often treated with antipsychotic drugs that target the symptoms of the disorder. The Positive and Negative Syndrome Scale (PANSS) is used to assess the severity of a patient’s schizophrenia. PANSS assigns a numerical value, in a range of 1 (absent) to 7 (extreme), to each positive, negative and general symptom. A higher score correlates with a more severe diagnosis. Physicians use the total PANSS score to gain an appropriate understanding of each patient’s breadth of symptoms."),
                                p("Despite the widespread use and effectiveness of antipsychotic drugs, the drugs are not universally effective in psychotic patients. Many are associated with serious neurologic side effects. The goal was, and continues to be, developing antipsychotic compounds with improved effectiveness and minimal side effects."),
                                p("The study sought to evaluate the effectiveness of the antipsychotic drug risperidone, and determine its optimal dose, for treating patients with schizophrenia. This double-blinded study enrolled 523 schizophrenic patients from both the United States and Canada. Participants were randomly assigned to eight weeks of daily treatment with placebo, risperidone (2mg, 6mg, 10mg or 16mg), or haloperidol."), 
                                fluidRow(
                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("sexpie"), plotOutput("racepie"), width = 12),
                                p("The pie charts above show the sex and race distributions of the 523 patients in the study. 17% of the patients were female, while 83% of them were male. 71% of the patients were White, 18% were Black, 8% were Hispanic, 1% were Asian, 1% were Pacific Islander, and less than 1% were American Indian. Clinical improvement of the patients was evaluated based on reduction in PANSS scores. Investigators determined that 6mg was the optimal dose of risperidone for schizophrenic patients. Risperidone was evaluated as a safe antipsychotic drug that can affect the positive and negative symptoms of schizophrenia."), 
                                hr(), width = 12)
                            )
                   ),
                   tabPanel("About Our App",
                              mainPanel(
                                width = 12,
                                br(),
                                p(tags$b("About Our App")),
                                p("A major concern for schizophrenic patients is the effectiveness of their antipsychotic treatment regimen, and the ways in which individual health characteristics affect their progress on these regimens. Key clinical questions include:"),
                                p(" 1. How do my health characteristics compare to others?"),
                                p(" 2. Given my health characteristics, what can I expect of various treatments?"),
                                p(" 3. Which treatment would be most effective for me?"),
                                p("Our app generates various visualizations and a multilevel linear regression model that addresses these concerns for patients diagnosed with schizophrenia."),
                                br(),
                                p(tags$b("Who can use this app?")),
                                p(tags$i("Patients already taking an antipsychotic treatment"), "can input their total PANSS scores for the weeks they have been enrolled on the regimen, their age of symptom onset, and treatment type, and visualize expectations for the predicted change in their total PANSS score."),
                                p(tags$i("Patients deciding which antipsychotic treatment to take"), "can input their initial total PANSS score and age of symptom onset, change the treatment types, and visualize how each treatment can influence their PANSS scores during the weeks on a specific regimen."),
                                br(),
                                p(tags$b("How do I use the app?")),
                                p("Patients can use this app to understand how their health characteristics compare to others, and how various treatments can affect symptom decrease. By inputting their own health data, the visualizations and regression model are individualized to provide patients with informed answers to relevant clinical questions:"),
                                p(" •	Was my onset of schizophrenic symptoms at an unusual age?"),
                                p(" •	Was my first hospitalization for schizophrenic symptoms at an unusual age?"),
                                p(" •	How does my age at symptom onset compare to others? How does it compare to others of the same sex as me?"),
                                p(" •	How does my treatment type and dose affect my change in total PANSS score?"),
                                p(" •	How does my age at symptom onset affect my response to each treatment type?"),
                                p("When an individual inputs his or her health data, the regression model will use the information to predict his or her treatment outcomes, measured in total PANSS score reductions. The model shows how age at symptom onset, treatment type, and duration of treatment affects symptom decrease."),
                                br(),
                                p(tags$b("How do I use the regression model?")),
                                p("The eight-week study tracked patients' PANSS scores seven times: before the study, at the start of the study, and at 1, 2, 4, 6 and 8 weeks into the study. These times are labeled with 'week numbers': -1, 0, 1, 2, 4, 6, and 8, respectively. Accordingly, the regression model makes PANSS score predictions at these week numbers, taking the user's available PANSS information, and predicting treatment outcomes for the rest of the study. When inputting one's own PANSS scores, use the following table to understand what information you have available to input:"),
                                tableOutput("information.table"),
                                p("For example, a patient who is 2 weeks into treatment should have 4 available PANSS scores to input, with data from before the treatment, at the start of treatment, and 1 and 2 weeks in to the treatment."),
                                hr(),
                                p(tags$i("App created by Alexandra Capellini, Danielle Franco, and Quan Bui. All data taken from 'Risperidone in the Treatment of Schizophrenia', published by Stephen R. Marder and Richard C. Meibach. Special thanks to Yates Coley and Todd Fojo."),
                                hr())
                            )
                  ),
                   navbarMenu("Explore Data",
                            tabPanel("Age Characteristics",
                              mainPanel(
                                sliderInput("age", label = "Input an age to compare:", min=0, max=60, value = 20)
                              ),
                              tabsetPanel(
                                tabPanel("First Symptoms", 
                                         plotOutput("onsetageplot"), 
                                         h4(textOutput("onsetagetext")),
                                         hr()
                                         ),
                                tabPanel("First Hospitalization",
                                         plotOutput("firsthospageplot"),
                                         h4(textOutput("firsthoptext")),
                                         hr()
                                         )
                              )
                   ),
                            tabPanel("Data Relationships",
                                tabsetPanel(
                                  tabPanel("Gender vs. Age of Symptom Onset", 

                                           br(), 
                                           sliderInput("age2", label = "Input age at symptom onset:", min = 0, max = 42, value = 20),
                                           br(),
                                           plotOutput("genderonsetplot"),
                                           p( h4(textOutput('maletext')),
                                              h4(' and '), 
                                              h4(textOutput('femaletext')), 
                                              h4(' had symptom onset at '), 
                                              h4(textOutput('agetext')),
                                              h4(' years old or younger. ') ),
                                              hr()
                                           
                                           ),
                                  tabPanel("Treatment vs. Change in Total PANSS Score", 
      
                                           br(),
                                           selectInput("treatment", 
                                                       label = "Select treatment type", 
                                                       choices = c("Placebo", "Haloperidol", "Risperidone_2mg", "Risperidone_6mg", "Risperidone_10mg", "Risperidone_16mg"), selected = "Placebo"), 
                                           helpText("Selected treatment appears as the white boxplot."),
                                           br(),
                                           plotOutput("treatmentchangePANSSplot"),
                                           br(),
                                           h4(textOutput("treatmentchangePANSStext")),
                                           br(),
                                           hr()
                                  )
                                  )
                                )
                   ),
                   tabPanel("Predict Treatment Outcomes",
                            sidebarLayout(
                              sidebarPanel(
                                p(tags$b("Enter your data to predict future PANSS scores:")),
                                sliderInput("my.onsetage", "Age at symptom onset:", min = 4, max = 42, value = 21, step = 1),
                                selectInput("my.treatment", label = "Antipsychotic treatment:", choices = c("none", "haloperidol", "risperidone (2mg)",
                                                                                                              "risperidone (6mg)", "risperidone (10mg)", "risperidone (16mg)"), selected = "None"),
                                helpText("Please see the 'About Our App' page for further instructions on how to get the following values."),
                                sliderInput("numind", label = "Number of available PANSS scores:",
                                            min = 1, max = 6, value = 3),
                                strong("Total PANSS score for each week:"),
                                uiOutput("ninputs") #,
                              ),
                              mainPanel(
                                plotOutput("my.regression"),
                                textOutput("explanation.regression"),
                                br(),
                                strong(textOutput("my.regressiontext")),
                                br(),
                                textOutput("tablestatement"),
                                br(),
                                tableOutput("my.regressiontable"),
                                hr()
                                
                              )    
                            )
                            
                   )
))
