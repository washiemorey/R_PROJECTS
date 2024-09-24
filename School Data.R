#School data Analysis
#...........Created by WASHIE MOREY ..............#
#.............On 8th August 2024...............#

#Importing data to R studio
data <- read.csv("C:\\Users\\hp\\OneDrive\\Desktop\\R Project\\Student scores.csv",
                 header = TRUE)
#Converting admission number and stream into factors
library(tidyverse)
data1 <- data %>% mutate(ADMISION_NUMBER = as.factor(ADMISION_NUMBER),
                         STREAM = as.factor(STREAM))

#finding number of students in each stream that did Exams
students_perstream <- data1 %>% group_by(STREAM) %>% summarise(studentcount = n()) %>% 
  arrange(desc(studentcount))

#finding students that did Exams for each subject and subject mean By stream
#English
Eng_students <- data1 %>% group_by(STREAM) %>% select(STREAM,NAME,ENGLISH) %>% na.omit() #Selecting required columns
eng.students <- Eng_students %>%summarise(StudentsCount = n()) %>% arrange(STREAM) #English Students count
eng_Mean <- Eng_students %>% summarise(Mean = round(mean(ENGLISH),1)) %>% arrange(STREAM)    #subject mean per stream
table <- data.frame(STREAM = eng.students$STREAM,STUDENTS = eng.students$StudentsCount,MEAN = eng_Mean$Mean) #table of stream,students and mean
table1 <- table %>% arrange(desc(MEAN))        #Table arranged by mean in descending order
library(ggplot2)                               #Visualizing mean score by stream
table1$STREAM <- factor(table1$STREAM,levels = table1$STREAM)  #for bars to appear are arranged by mean
ggplot(table1,aes(STREAM,MEAN))+
  geom_bar(stat = "identity",fill = "steelblue") +
  geom_point(aes( y = STUDENTS))+                                  #Adding stdents points
  geom_line(aes(y = STUDENTS,group = 1),col = "steelblue")+       #Line connecting students points
  labs(title = "ENGLISH MEAN VARIATION ACROSS STREAMS",
       x = "STREAM",y = "MEAN VALUE") +
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+   #Students number scale name
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),     #Adjusting streams to appear vertical for clarity
        plot.title = element_text(hjust = 0.5,face = "bold"),             #Centering tittle and making it bold
        axis.title.x = element_text(face =  "bold"),                      #Making axes labels bold
        axis.title = element_text(face = "bold"))+                          
  theme(panel.background = element_rect(fill = "lightcyan")) +           #styling panel area
  theme(plot.background = element_rect(fill = "lightgreen"))             #Styling plot area

#Kiswahili 
kis_Students <- data1 %>% group_by(STREAM) %>% select(STREAM,NAME,KISWAHILI) %>% na.omit()   #Selecting required columns
kis.students <- kis_Students %>% summarise(StudentCount = n()) %>% arrange(STREAM)               #Kiswahili Students count
kis_mean <- kis_Students %>% summarise(Mean = round(mean(KISWAHILI),1))  %>% group_by(STREAM)    #subject mean per stream
table <- data.frame(STREAM = kis.students$STREAM,STUDENTS = kis.students$StudentCount,MEAN = kis_mean$Mean ) %>%   #table of stream,students and mean
  arrange(desc(MEAN))                                                                                  #and arranging in descending order
table$STREAM <- factor(table$STREAM,levels = table$STREAM)  #Viualisation 
library(ggplot2)
ggplot(table,aes(STREAM,MEAN))+
  geom_bar(stat = "identity",fill = "red")+
  geom_point(aes(y = STUDENTS))+
  geom_line(aes(y = STUDENTS,group = 1))+
  labs(title = "KISWAHILI MEAN SCORE VARIATION ACROSS STREAMS",
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+
  theme(axis.text.x = element_text(angle = 90,vjust = 1,hjust = .5),  #Adjusting streams to appear vertical for clarity
        plot.title = element_text(hjust = .5,face = "bold"),          #Centering tittle and making it bold
        axis.title.x = element_text(face = "bold"),                   #Making axes labels bold
        axis.title.y = element_text(face = "bold"))+
  theme(plot.background = element_rect(fill = "lightcyan"))+          #Styling plot area
  theme(panel.background = element_rect(fill = "cyan"))               #styling panel area

#Maths 
math_students <- data1 %>% group_by(STREAM) %>% select(STREAM,NAME,MATHS) %>% na.omit()  #Selecting required columns and ommiting NA values
math.students <- math_students %>% summarise(StudentCount = n()) %>% arrange(STREAM)          #Maths Students count
math_mean <- math_students %>% summarise(Mean = round(mean(MATHS),1)) %>% arrange(STREAM)     #Maths mean per stream
mathtable <- data.frame(STREAM = math.students$STREAM,STUDENTS = math.students$StudentCount,       #table of stream,students and mean
                        MEAN = math_mean$Mean) %>% arrange(MEAN)                              #arranging in ascending order
mathtable$STREAM <- factor(mathtable$STREAM,levels = mathtable$STREAM)  #Visualizing mean with its
library(ggplot2)                                                        #variation by students
ggplot(mathtable,aes(STREAM,MEAN))+
  geom_bar(stat = "identity",fill = "grey")+
  geom_point(aes(y = STUDENTS)) +                                             #Plotting students
  geom_line(aes(y = STUDENTS,group = 1))+
  labs(title = "MATHS MEAN SCORE VARIATION ACROSS STREAMS",                   #Naming title and axes
       x = "STREAM", y = "MEAN VALUE")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "NUMBER OF STUDENTS"))+   #Label of students numbers
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = 1),
        plot.title = element_text(hjust = .5,face = "bold"),                  #Centering tittle and making it bold
        axis.title.x = element_text(face = "bold"),                           #Making axes labels bold
        axis.title.y = element_text(face = "bold"))+
  theme(panel.background = element_rect(fill = "cyan"))+                      #styling panel area
  theme(plot.background = element_rect(fill = "lightcyan"))                   #Styling plot area

#BIOLOGY
bio_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,BIOLOGY) %>% na.omit()           #Selecting required columns and ommiting NA values
bio.students <- bio_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                    #Biology Students count
bio_mean <- bio_students %>% summarise(Biomean = round(mean(BIOLOGY),2)) %>% arrange(STREAM)       #Biology mean per stream
biotable <- data.frame(STREAM = bio.students$STREAM,STUDENTS = bio.students$Studentscount,                 #table of stream,students and mean
                       MEAN = bio_mean$Biomean) %>% arrange(MEAN)                                  #arranging in ascending order
biotable$STREAM <- factor(biotable$STREAM,levels = biotable$STREAM)       #visualizing
library(ggplot2)
ggplot(biotable,aes(STREAM,MEAN)) +                               #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "green") +                    #adding bars
  geom_point(aes(y = STUDENTS)) +                                 #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "green") +          #line for students
  labs(title = "BIOLOGY MEAN VARIATION ACROSS STREAMS",           #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+ #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),     #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),               #bolding xlab
        axis.title.y = element_text(face = "bold"))+              #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) + #making x values appear verticaly
  theme(panel.background = element_rect(fill = "lightcoral"))+    #styling panel area
  theme(plot.background = element_rect(fill = "lightcyan"))       #styling plot area

#PHYSICS
phyc_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,PHYSICS) %>% na.omit()           #Selecting required columns and ommiting NA values
phyc.students <- phyc_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                    # Physics Students count
phyc_mean <- phyc_students %>% summarise(Phycmean = round(mean(PHYSICS),2)) %>% arrange(STREAM)     #Physics mean per stream
phyctable <- data.frame(STREAM = phyc.students$STREAM,STUDENTS = phyc.students$Studentscount,                 #table of stream,students and mean
                       MEAN = phyc_mean$Phycmean) %>% arrange(MEAN)                                  #arranging in ascending order
phyctable$STREAM <- factor(phyctable$STREAM,levels = phyctable$STREAM)       #visualizing
library(ggplot2)
ggplot(phyctable,aes(STREAM,MEAN)) +                               #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "orange") +                    #adding bars
  geom_point(aes(y = STUDENTS)) +                                 #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "green") +          #line for students
  labs(title = "PHYSICS MEAN VARIATION ACROSS STREAMS",           #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+ #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),     #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),               #bolding xlab
        axis.title.y = element_text(face = "bold"))+              #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) + #making x values appear verticaly
  theme(panel.background = element_rect(fill = "lightcyan"))+    #styling panel area
  theme(plot.background = element_rect(fill = "lightcoral"))       #styling plot area

#CHEMISTRY
chem_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,CHEMISTRY) %>% na.omit()           #Selecting required columns and ommiting NA values
chem.students <- chem_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                      #Chemistry Students count
chem_mean <- chem_students %>% summarise(Chemmean = round(mean(CHEMISTRY),2)) %>% arrange(STREAM)     #Chemistry mean per stream
chemtable <- data.frame(STREAM = chem.students$STREAM,STUDENTS = chem.students$Studentscount,                   #table of stream,students and mean
                       MEAN = chem_mean$Chemmean) %>% arrange(desc(MEAN))                                   #arranging in descending order
chemtable$STREAM <- factor(chemtable$STREAM,levels = chemtable$STREAM)       #visualizing
library(ggplot2)
ggplot(chemtable,aes(STREAM,MEAN)) +                                   #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "lightblue") +                    #adding bars
  geom_point(aes(y = STUDENTS)) +                                       #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "lightblue",size = 1) +          #line for students
  labs(title = "CHEMISTRY MEAN VARIATION ACROSS STREAMS",               #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+  #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),        #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),                  #bolding xlab
        axis.title.y = element_text(face = "bold"))+                  #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) +   #making x values appear verticaly
  theme(panel.background = element_rect(fill = "coral"))+             #styling panel area
  theme(plot.background = element_rect(fill = "lightcyan"))           #styling plot area

#HISTORY
hist_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,HISTORY) %>% na.omit()           #Selecting required columns and ommiting NA values
hist.students <- hist_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                    #History Students count
hist_mean <- hist_students %>% summarise(Histmean = round(mean(HISTORY),2)) %>% arrange(STREAM)     #Histpry mean per stream
histtable <- data.frame(STREAM = hist.students$STREAM,STUDENTS = hist.students$Studentscount,                 #table of stream,students and mean
                       MEAN = hist_mean$Histmean) %>% arrange(MEAN)                                 #arranging in ascending order
histtable$STREAM <- factor(histtable$STREAM,levels = histtable$STREAM)       #visualizing
library(ggplot2)
ggplot(histtable,aes(STREAM,MEAN)) +                               #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "black") +                    #adding bars
  geom_point(aes(y = STUDENTS),col = "red") +                                 #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "red") +          #line for students
  labs(title = "HISTORY MEAN VARIATION ACROSS STREAMS",           #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+  #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),       #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),                #bolding xlab
        axis.title.y = element_text(face = "bold"))+               #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) + #making x values appear verticaly
  theme(panel.background = element_rect(fill = "lightgreen"))+    #styling panel area
  theme(plot.background = element_rect(fill = "cyan"))            #styling plot area

#GEOGRAPHY
geog_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,GEORAPHY) %>% na.omit()           #Selecting required columns and ommiting NA values
geog.students <- geog_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                    #Geography Students count
geog_mean <- geog_students %>% summarise(Geogmean = round(mean(GEORAPHY),2)) %>% arrange(STREAM)     #Geography mean per stream
geogtable <- data.frame(STREAM = geog.students$STREAM,STUDENTS = geog.students$Studentscount,                 #table of stream,students and mean
                        MEAN = geog_mean$Geogmean) %>% arrange(MEAN)                                 #arranging in ascending order
geogtable$STREAM <- factor(geogtable$STREAM,levels = geogtable$STREAM)       #visualizing
library(ggplot2)
ggplot(geogtable,aes(STREAM,MEAN)) +                               #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "brown") +                    #adding bars
  geom_point(aes(y = STUDENTS)) +                                 #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "brown") +          #line for students
  labs(title = "GEOGRAPHY MEAN VARIATION ACROSS STREAMS",           #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+  #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),       #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),                #bolding xlab
        axis.title.y = element_text(face = "bold"))+               #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) + #making x values appear verticaly
  theme(panel.background = element_rect(fill = "lightgreen"))+    #styling panel area
  theme(plot.background = element_rect(fill = "cyan"))            #styling plot area

#CRE
cre_students <- data1 %>% group_by(STREAM) %>% select(NAME,STREAM,CRE) %>% na.omit()           #Selecting required columns and ommiting NA values
cre.students <- cre_students %>% summarise(Studentscount = n()) %>% arrange(STREAM)                    #CRE Students count
cre_mean <- cre_students %>% summarise(Cremean = round(mean(CRE),2)) %>% arrange(STREAM)     #CRE mean per stream
cretable <- data.frame(STREAM = cre.students$STREAM,STUDENTS = cre.students$Studentscount,                 #table of stream,students and mean
                        MEAN = cre_mean$Cremean) %>% arrange(MEAN)                                 #arranging in ascending order
cretable$STREAM <- factor(cretable$STREAM,levels = cretable$STREAM)       #visualizing
library(ggplot2)
ggplot(cretable,aes(STREAM,MEAN)) +                               #Plotting mean on x axis and mean on y axis
  geom_bar(stat = "identity",fill = "white") +                    #adding bars
  geom_point(aes(y = STUDENTS),col = "red") +                                 #adding points for students count
  geom_line(aes(y = STUDENTS,group = 1),col = "red") +          #line for students
  labs(title = "CRE MEAN VARIATION ACROSS STREAMS",               #labels for title,x axis,yaxis
       y = "MEAN VALUE",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "STUDENTS NUMBER"))+  #adding students scale & name
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),        #Centering tittle and making bold   
        axis.title.x = element_text(face = "bold"),                  #bolding xlab
        axis.title.y = element_text(face = "bold"))+                 #bolding ylab
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 1)) + #making x values appear verticaly
  theme(panel.background = element_rect(fill = "black"))+               #styling panel area
  theme(plot.background = element_rect(fill = "cyan"))                   #styling plot area

#AGRICULTURE
agric_students <- data1 %>%group_by(STREAM) %>% select(STREAM,NAME,AGRCULTURE) %>% na.omit()            #Selecting required columns and ommiting NA values
agric.students <- agric_students %>% summarise(StudentsCount = n()) %>% group_by(STREAM)                      #AGRICULTURE Students count
agric_mean <- agric_students %>% summarise(Agricmean = round(mean(AGRCULTURE),2)) %>% group_by(STREAM)  #AGRICULTURE mean per stream
Agrictable <- data.frame(STREAM = agric.students$STREAM,STUDENTS = agric.students$StudentsCount,                    #table of stream,students and mean
                         MEAN = agric_mean$Agricmean) %>% arrange(desc(MEAN))                           #arranging in ascending order
Agrictable$STREAM <- factor(Agrictable$STREAM,levels = Agrictable$STREAM)  #visulization
library(ggplot2)
ggplot(Agrictable,aes(STREAM,MEAN))+                       #plotting stream on x axis & mean on y
  geom_bar(stat = "identity",fill = "green")+              #Adding mean bars
  geom_point(aes(y = STUDENTS),col = "black")+             #Plotting students points
  geom_line(aes(y = STUDENTS,group = 1),col = "black")+    #Adding line for students
  labs(title = "AGRICULTURE MEAN VARIATION ACROSS STREAMS", #adding tittle x-axis y-axis mean names
       y = "MEAN VALUES",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+ #Adding axis for students
  theme(plot.title = element_text(hjust = .5, face = "bold"),           #Centering and bolding tittle
        axis.title.x = element_text(face = "bold"),                     #Bolding axes
        axis.title.y = element_text(face = "bold")) +                   #Arranging x axis values verticaly
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust =1))+ #Arranging x axis values verticaly
  theme(panel.background = element_rect(fill = "maroon"))+           #styling panel area
  theme(plot.background = element_rect(fill = "lightcoral"))         #styling plot area

#COMPUTER
comp_students <- data1 %>%group_by(STREAM) %>% select(STREAM,NAME,COMPUTER) %>% na.omit()            #Selecting required columns and ommiting NA values
comp.students <- comp_students %>% summarise(StudentsCount = n()) %>% group_by(STREAM)                      #COMPUTER Students count
comp_mean <- comp_students %>% summarise(Compmean = round(mean(COMPUTER),2)) %>% group_by(STREAM)  #COMPUTER mean per stream
comptable <- data.frame(STREAM = comp.students$STREAM,STUDENTS = comp.students$StudentsCount,                    #table of stream,students and mean
                         MEAN = comp_mean$Compmean) %>% arrange(desc(MEAN))                           #arranging in ascending order
comptable$STREAM <- factor(comptable$STREAM,levels = comptable$STREAM)  #visulization
library(ggplot2)
ggplot(comptable,aes(STREAM,MEAN))+                       #plotting stream on x axis & mean on y
  geom_bar(stat = "identity",fill = "green")+              #Adding mean bars
  geom_point(aes(y = STUDENTS),col = "black")+             #Plotting students points
  geom_line(aes(y = STUDENTS,group = 1),col = "black")+    #Adding line for students
  labs(title = "COMPUTER MEAN VARIATION ACROSS STREAMS", #adding tittle x-axis y-axis mean names
       y = "MEAN VALUES",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+ #Adding axis for students
  theme(plot.title = element_text(hjust = .5, face = "bold"),           #Centering and bolding tittle
        axis.title.x = element_text(face = "bold"),                     #Bolding axes
        axis.title.y = element_text(face = "bold")) +                   #Arranging x axis values verticaly
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust =1))+ #Arranging x axis values verticaly
  theme(panel.background = element_rect(fill = "cyan"))+           #styling panel area
  theme(plot.background = element_rect(fill = "lightcoral"))       #styling plot area

#fRENCH
french_students <- data1 %>%group_by(STREAM) %>% select(STREAM,NAME,FRENCH) %>% na.omit()            #Selecting required columns and ommiting NA values
french.students <- french_students %>% summarise(StudentsCount = n()) %>% group_by(STREAM)                      #french Students count
french_mean <- french_students %>% summarise(frenchcmean = round(mean(FRENCH),2)) %>% group_by(STREAM)  #french mean per stream
frenchtable <- data.frame(STREAM = french.students$STREAM,STUDENTS = french.students$StudentsCount,                    #table of stream,students and mean
                         MEAN = french_mean$frenchcmean) %>% arrange(desc(MEAN))                           #arranging in ascending order
frenchtable$STREAM <- factor(frenchtable$STREAM,levels = frenchtable$STREAM)  #visulization
library(ggplot2)
ggplot(frenchtable,aes(STREAM,MEAN))+                       #plotting stream on x axis & mean on y
  geom_bar(stat = "identity",fill = "brown")+              #Adding mean bars
  geom_point(aes(y = STUDENTS),col = "black")+             #Plotting students points
  geom_line(aes(y = STUDENTS,group = 1),col = "black")+    #Adding line for students
  labs(title = "fRENCH MEAN VARIATION ACROSS STREAMS",     #adding tittle x-axis y-axis mean names
       y = "MEAN VALUES",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+ #Adding axis for students
  theme(plot.title = element_text(hjust = .5, face = "bold"),           #Centering and bolding tittle
        axis.title.x = element_text(face = "bold"),                     #Bolding axes
        axis.title.y = element_text(face = "bold")) +                   #Arranging x axis values verticaly
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust =1))+    #Arranging x axis values verticaly
  theme(panel.background = element_rect(fill = "lightgreen"))+          #styling panel area
  theme(plot.background = element_rect(fill = "lightcoral"))            #styling plot area


  #BUSINESS
bs_students <- data1 %>%group_by(STREAM) %>% select(STREAM,NAME,BUSINESS) %>% na.omit()            #Selecting required columns and ommiting NA values
bs.students <- bs_students %>% summarise(StudentsCount = n()) %>% group_by(STREAM)                      #Business Students count
bs_mean <- bs_students %>% summarise(Bsmean = round(mean(BUSINESS),2)) %>% group_by(STREAM)  #Business mean per stream
Bstable <- data.frame(STREAM = bs.students$STREAM,STUDENTS = bs.students$StudentsCount,                    #table of stream,students and mean
                          MEAN = bs_mean$Bsmean) %>% arrange(desc(MEAN))                           #arranging in ascending order
Bstable$STREAM <- factor(Bstable$STREAM,levels = Bstable$STREAM)  #visulization
library(ggplot2)
ggplot(Bstable,aes(STREAM,MEAN))+                       #plotting stream on x axis & mean on y
  geom_bar(stat = "identity",fill = "red")+              #Adding mean bars
  geom_point(aes(y = STUDENTS),col = "black")+             #Plotting students points
  geom_line(aes(y = STUDENTS,group = 1),col = "black")+    #Adding line for students
  labs(title = "BUSINESS MEAN VARIATION ACROSS STREAMS",     #adding tittle x-axis y-axis mean names
       y = "MEAN VALUES",x = "STREAM")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "STUDENTS NUMBER"))+ #Adding axis for students
  theme(plot.title = element_text(hjust = .5, face = "bold"),           #Centering and bolding tittle
        axis.title.x = element_text(face = "bold"),                     #Bolding axes
        axis.title.y = element_text(face = "bold")) +                   #Arranging x axis values verticaly
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust =1))+    #Arranging x axis values verticaly
  theme(panel.background = element_rect(fill = "lightgreen"))+          #styling panel area
  theme(plot.background = element_rect(fill = "lightcoral"))            #styling plot area

#Top 5 students in each subject
#English
top_english <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,ENGLISH) %>% 
  arrange(desc(ENGLISH)) %>% slice(1:5)
#Kiswahili
top_kis <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,KISWAHILI) %>% 
  arrange(desc(KISWAHILI)) %>% slice(1:5)
#MATHEMATICS
top_math <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,MATHS) %>% 
  arrange(desc(MATHS)) %>% slice(1:5)
#BIOLOGY
top_bio <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,BIOLOGY) %>% 
  arrange(desc(BIOLOGY)) %>% slice(1:5)
#PHYSICS
top_phyc <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,PHYSICS) %>% 
  arrange(desc(PHYSICS)) %>% slice(1:5)
#CHEMISTRY
top_chem <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,CHEMISTRY) %>% 
  arrange(desc(CHEMISTRY)) %>% slice(1:5)
#HISTORY
top_hist <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,HISTORY) %>% 
  arrange(desc(HISTORY)) %>% slice(1:5)
#CRE
top_cre <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,CRE) %>% 
  arrange(desc(CRE)) %>% slice(1:5)
#GEOGRAPHY
top_geog <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,GEORAPHY) %>% 
  arrange(desc(GEORAPHY)) %>% slice(1:5)
#AGRICULTURE
top_agric <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,AGRCULTURE) %>% 
  arrange(desc(AGRCULTURE)) %>% slice(1:5)
#COMPUTER
top_comp <- data1 %>% select(ADMISION_NUMBER,NAME,STREAM,COMPUTER) %>% 
  arrange(desc(COMPUTER)) %>% slice(1:5)
#fRENCH
top_french <- data1 %>% select(ADMISION_NUMBER,STREAM,NAME,FRENCH) %>% 
  arrange(desc(FRENCH)) %>% slice(1:5)
#BUSINESS
top_bs <- data1 %>% select(ADMISION_NUMBER,STREAM,NAME,BUSINESS) %>% 
  arrange(desc(BUSINESS)) %>% slice(1:5)

#Overall mean of each subject
data_mean <- data1 %>% select(-ADMISION_NUMBER,-NAME,-STREAM,-SUBJECTS) %>%  #Removing non subject collumns
  summarise(across(everything(),~mean(.x,na.rm = TRUE)))           #calculating mean
mean <- data.frame(round(data_mean,2))               #converting to data frame and rounding 
mean <- t(mean) %>% as.data.frame()                  #converting column names into row names
mean$SUBJECT <- rownames(mean)                        #making rownames column
colnames(mean)[1] <- "SUBJECT MEAN"                   #Giving column name
rownames(mean) <- NULL                                #Removing rownames
mean <- mean %>% arrange(desc(`SUBJECT MEAN`))     #Arranging mean values in descending order
TOTAL <- sum(mean$`SUBJECT MEAN`)                  #Total Average mean

#Visualizing mean of each subject as percentage of Total mean  using piechart
pct_mean <- round((mean$`SUBJECT MEAN`/sum(mean$`SUBJECT MEAN`) * 100),2)  #Percentage of each subject
labs = paste0(mean$SUBJECT,"-",pct_mean,"%",sep = "")  #Labels of subject and its corresponding percentage
pie(mean$`SUBJECT MEAN`,labels = labs,main = "MEAN VISUALISATION")


#finding  total marks for each student
total_marks <- data1 %>% mutate(TOTAL.MARKS = rowSums(select(.,ENGLISH,KISWAHILI,MATHS,BIOLOGY,PHYSICS,
                                                      CHEMISTRY,HISTORY,GEORAPHY,CRE,AGRCULTURE,
                                                      FRENCH,COMPUTER,BUSINESS),na.rm = TRUE))
#Distribution of total marks
library(gtExtras)
library(svglite)
total_dist <- total_marks %>% select(TOTAL.MARKS) %>%
  summarise(MEAN = round(mean(TOTAL.MARKS),2),
            MEDIAN = round(median(TOTAL.MARKS),2),
            STANDARD.DEV = round(sd(TOTAL.MARKS),2),
            DISTRIBUTION = list(TOTAL.MARKS),
            MINIMUM = min(TOTAL.MARKS),
            MAXIMUM = max(TOTAL.MARKS))
#Visualizing distribution of total marks
total_dist %>% gt() %>% gt_plt_dist(DISTRIBUTION) %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "TOTAL MARKS DISTRIBUTION VISUALIZATION")
#Number of students that failed to do exam in compulsory subjects in each stream
#English
eng <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(ENGLISH))) #Students in each stream
sum(eng$NA_COUNT)                        #Total students
#KISWAHILI 
kis <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(KISWAHILI))) #Students in each stream
sum(kis$NA_COUNT)                        #Total students
#MATHS
math <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(MATHS))) #Students in each stream
sum(math$NA_COUNT)                        #Total students
#BIOLOGY
bio <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(BIOLOGY))) #Students in each stream
sum(bio$NA_COUNT)                        #Total students
#PHYSICS
phyc <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(PHYSICS))) #Students in each stream
sum(phyc$NA_COUNT)                        #Total students
#CHEMISTRY
chem <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(CHEMISTRY))) #Students in each stream
sum(chem$NA_COUNT)                        #Total students
#HISTORY
hist <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(HISTORY))) #Students in each stream
sum(hist$NA_COUNT)                        #Total students
#CRE
cre <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(CRE))) #Students in each stream
sum(cre$NA_COUNT)                        #Total students
#GEOGRAPHY
geog <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(GEORAPHY))) #Students in each stream
sum(geog$NA_COUNT)                        #Total students
#BUSINESS
bs <- data1 %>% group_by(STREAM) %>% summarise(NA_COUNT = sum(is.na(BUSINESS))) #Students in each stream
sum(bs$NA_COUNT) #Total students

#NUMBER that did not complete Exam together with Subjects exam they did not do
students <- data1 %>% filter(SUBJECTS < 11) %>%  arrange(desc(SUBJECTS))


#......ANALYSIS BY GRADES................#
#Grades function
assign_grade <- function(score) {
  if (is.na(score)) {
    return("NA")
  } else if (score >= 80 && score <= 100) {
    return("A")
  } else if (score >= 75 && score < 80) {
    return("A-")
  } else if (score >= 70 && score < 75) {
    return("B+")
  } else if (score >= 65 && score < 70) {
    return("B")
  } else if (score >= 60 && score < 65) {
    return("B-")
  } else if (score >= 55 && score < 60) {
    return("C+")
  } else if (score >= 50 && score < 55) {
    return("C")
  } else if (score >= 45 && score < 50) {
    return("C-")
  } else if (score >= 40 && score < 45) {
    return("D+")
  } else if (score >= 35 && score < 40) {
    return("D")
  } else if (score >= 30 && score < 35) {
    return("D-")
  } else if (score < 30) {
    return("E")
  } else {
    return(NA)
  }
}

grades <- data1   #Assigning data1 to new data frame grades 
grades[ , 4:16] <- lapply(data1[ , 4:16],function(x) sapply(x,assign_grade))  #Asigning grades to instead of values
#Grades of each subject per stream
#English
math.grades <- grades %>% select(STREAM,ENGLISH) %>% group_by(STREAM)
table(math.grades)
#Kisawahili 
kis.grades <- grades %>% select(STREAM,KISWAHILI) %>% group_by(STREAM) 
table(kis.grades)
#Maths
math.grades <- grades %>% select(STREAM,MATHS) %>% group_by(STREAM)
table(math.grades) 
#Biology
bio.grades <- grades %>% select(STREAM,BIOLOGY) %>% group_by(STREAM)
table(bio.grades)
#Physics
phyc.grades <- grades %>% select(STREAM,PHYSICS) %>% group_by(STREAM)
table(phyc.grades)
chem.grades <- grades %>% select(STREAM,CHEMISTRY) %>% group_by(STREAM)
table(chem.grades)
#History
his.grades <- grades %>% select(STREAM,CHEMISTRY) %>% group_by(STREAM)
table(his.grades)
#Cre
cre.grades <- grades %>% select(STREAM,CRE) %>% group_by(STREAM)
table(cre.grades)
#Geography
geog.grades <- grades %>% select(STREAM,GEORAPHY) %>% group_by(STREAM)
table(geog.grades)
#Agriculture
agric.grades <- grades %>%select(STREAM,AGRCULTURE) %>% group_by(STREAM)
table(agric.grades)
#Computer
comp.grades <- grades %>% select(STREAM,COMPUTER) %>% group_by(STREAM)
table(comp.grades)
#french
french.grades <- grades %>% select(STREAM,FRENCH) %>% group_by(STREAM)
table(french.grades)
#Business
bs.grades <- grades %>% select(STREAM,BUSINESS) %>% group_by(STREAM)
table(bs.grades)
