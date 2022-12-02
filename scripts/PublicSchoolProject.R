library(dplyr)
library(ggplot2)


# I already did cosmetic changes to my data in excel in order to convert
# to csv and make things easier to read in
# Now im creating a new column to represent Hispanic and black population by district

ClassSizebyEthnicity <- read.csv("raw_data/ClassSizebyRaceEthnicity.csv")

district_race <- ClassSizebyEthnicity %>%
  select(District.Name, African.American.., Hispanic.., White..) %>%
  group_by(District.Name) %>%
  mutate(percent_black_hispanic = sum(African.American.., Hispanic.., na.rm = TRUE ))



artcourse <- read.csv("raw_data/artcourse.csv")

art_course_taken <- artcourse %>%
  select(District.Name, All.Grades, Total.Students) %>%
  mutate(percent_students_in_art = (All.Grades / Total.Students)*100)



#get a new column with percent 
#students in art class and then add that to the district race data set

sat_performance <- read.csv("raw_data/sat_performance.csv")
  
sat_art <- select(sat_performance, Tests.Taken, District.Name) %>%
  inner_join(art_course_taken, by = "District.Name")

art_sat_race <- inner_join(sat_art,district_race, by = "District.Name") %>%
   select(-African.American.., -Hispanic..)
#I added how many students took sat to this data so that I can divide it by
 # total students once I figure out how to do that
 

cheese <- read.csv("raw_data/StudentDisciplineDataReport.csv") %>%
  select(District.Name, Students.Disciplined) %>%
  inner_join(art_sat_race, by = "District.Name")
# added number of students disciplined to data set
# I will turn this into a proportion using total number of students
# once I figure out how to do that



grouped_data <- read.csv("raw_data/TeacherSalaries.csv") %>%
  select(District.Name,Average.Salary) %>%
  inner_join(cheese, by = "District.Name")
#added average salary, completing my data set for now
#I ended up losing a good amount of data points, so I'll see how it goes



proportion_data <- mutate(grouped_data, percent_take_sat = (Tests.Taken/Total.Students)*100) %>%
  mutate(percent_disciplined = (Students.Disciplined / Total.Students)*100)%>%
  select(-Students.Disciplined, -Tests.Taken, -All.Grades)

write.csv(proportion_data,"clean_data/Proportion_Data.csv", row.names = FALSE)


#new data set with all districts less than 5%, between 5 & 30, 
#between 30 and 50, and above 50 percent black and hispanic.
proportion_data_wclass <- mutate(proportion_data, percent_poc = case_when(
  percent_black_hispanic <= 5  ~ " <5%",
  percent_black_hispanic > 5 & percent_black_hispanic <30 ~ ">5% & <30%",
  percent_black_hispanic > 30 & percent_black_hispanic <50 ~ ">30% & <50%",
  percent_black_hispanic >= 50 ~ ">50%"))
                           

#got workable data set
#would love to get this in a better order
 grouped_by_poc <- group_by(proportion_data_wclass, percent_poc) %>%
   summarise(percent_students_in_art = mean(percent_students_in_art),
             percent_take_sat = mean(percent_take_sat),
             percent_disciplined= mean(percent_disciplined),
             average_salary = mean(Average.Salary)) 
  

 #start graphing



#plot for average salary
 #would love to get these in a better order
ggplot(proportion_data_wclass,aes(x = percent_poc, y = Average.Salary, color=percent_poc) )+
  geom_boxplot()
        
ggplot(proportion_data_wclass,aes(x = percent_poc, y = percent_students_in_art, color=percent_poc))+
  geom_boxplot()

ggplot(proportion_data_wclass,aes(x = percent_poc, y = percent_take_sat, color=percent_poc) )+
  geom_boxplot()

ggplot(proportion_data_wclass,aes(x = percent_poc, y = percent_disciplined, color=percent_poc) )+
  geom_boxplot()


ggplot(proportion_data_wclass,aes(x = percent_black_hispanic, y = percent_take_sat) )+
  geom_point(mapping =aes(color=percent_poc)) +
  geom_smooth()



#simple histogram to show spread in racial makeup
ggplot(grouped_data, aes(percent_black_hispanic))+
  geom_histogram()
