library(readxl)
library(tidyverse)



LEA_short    = read.csv("/home/ethan/Desktop/RStudio Workspace/Final Project/ccd_lea_029_1920_w_1a_082120/ccd_lea_029_1920_w_1a_082120.csv")
LEA_long     = read.csv("/home/ethan/Desktop/RStudio Workspace/Final Project/ccd_lea_052_1920_l_1a_082120/ccd_lea_052_1920_l_1a_082120.csv")
LEA_geo     = read_xlsx("/home/ethan/Desktop/RStudio Workspace/Final Project/EDGE_SCHOOLDISTRICT_TL20_SY1920/EDGE_SCHOOLDISTRICT_TL20_SY1920.xlsx")
State_names  = read.csv("/home/ethan/Desktop/RStudio Workspace/Final Project/us-state-ansi-fips.csv") %>%
  mutate(stusps = str_sub(stusps, 2, 3))
# This file needed to be mutated, because the state FP code, which is two digits, was in the form of a string and preceded by a space. All further
# opterations would not work with this format.

# These are the absolute directories for the data files on my device. They may be changed to relative directories in the working directory
# if the files are present.




Student_total_by_district = LEA_long %>%
  filter(TOTAL_INDICATOR == "Derived - Education Unit Total minus Adult Education Count") %>%
  mutate(LEAID = str_pad(as.character(LEAID), 7, side = "left", pad = "0"))%>%
  select(LEAID, STUDENT_COUNT)

LEA_geo_modified = left_join(LEA_geo, Student_total_by_district, by = c("GEOID" = "LEAID")) %>%
  filter(as.integer(STATEFP) < 58) %>%
  filter(is.na(STUDENT_COUNT) == F) # the entries with NA for student count are artificially removed.

# The intention of this modification is to limit the data set to just the 50 states + D.C.
# That is, the territories, including Puerto Rico, are not considered.




LEA_geo_modified_continental = LEA_geo_modified %>%
  filter(!(STATEFP %in% c("02", "15")))



LEA_school_count = LEA_short %>%
  mutate(LEAID = str_pad(as.character(LEAID), 7, side = "left", pad = "0"))%>%
  select(LEAID, OPERATIONAL_SCHOOLS)

LEA_modified_school_count = left_join(LEA_geo_modified, LEA_school_count, by = c("GEOID" = "LEAID"))

LEA_modified_school_count %>%
  summarize(Average_number_of_schools = mean(OPERATIONAL_SCHOOLS), Standard_deviation = sd(OPERATIONAL_SCHOOLS))

quantile(LEA_modified_school_count$OPERATIONAL_SCHOOLS, c(seq(0.001, 0.01, 0.001), seq(0.02, 0.98, 0.02), seq(0.99, 0.999, 0.001)))
# explotaroty, checking the number of schools a school district would have by percentile.

color_1 = match(c("AK", "AL", "AR", "CT", "DE", "HI", "IL", "ME", "MI", "MN", "MT", "NE", "NM", "NV", "SC", "VA", "WA"), State_names$stusps)
color_2 = match(c("AZ", "DC", "FL", "KS", "KY", "MS", "NC", "ND", "OR", "PA", "RI", "TX", "VT", "WI", "WY"), State_names$stusps)
color_3 = match(c("CA", "CO", "GA", "ID", "IN", "LA", "MA", "MO", "NJ", "SD", "WV"), State_names$stusps)
color_4 = match(c("IA", "MD", "NH", "NY", "OH", "OK", "TN", "UT"), State_names$stusps)

group_1 = State_names$st[color_1]
group_2 = State_names$st[color_2]
group_3 = State_names$st[color_3]
group_4 = State_names$st[color_4]

# These groups are manually created so that the map can be colored using 4 colors for clear distinction.

LEA_geo_modified_color_continental  = LEA_geo_modified_continental %>%
  mutate(color_group = case_when(as.integer(STATEFP) %in% group_1 ~ 1, 
                           as.integer(STATEFP) %in% group_2 ~ 2, 
                           as.integer(STATEFP) %in% group_3 ~ 3, 
                           as.integer(STATEFP) %in% group_4 ~ 4))






# Plot 1c marks every school district with a point having an area proportional to their student population, effectively making the shade indicate
# density, and plot 1d marks every school district with a point of identical size to indicate their locations.

# Plot 1b equivalent to 1c except that they are not manually assigned one of the 4 colors, which leads to clarity issues.
# Plot 1a is equivalent to 1b but does not have Hawaii/Alaska excluded.



plot_1_a = LEA_geo_modified %>% ggplot(aes(y = as.numeric(INTPTLAT), x = as.numeric(INTPTLON), color = STATEFP))+
  geom_point(aes(size = STUDENT_COUNT), alpha = 0.2, shape = 16)+
  xlim(-170, -60)+
  ylim(15, 75)+
  scale_size_continuous(range = c(0, 15))+
  coord_fixed()

ggsave(filename = "1_a_200.png", plot_1_a,
       width = 36, height = 20, dpi = 200, units = "in", device='png', limitsize = FALSE)



plot_1_b = LEA_geo_modified %>% ggplot(aes(y = as.numeric(INTPTLAT), x = as.numeric(INTPTLON), color = STATEFP))+
  geom_point(aes(size = STUDENT_COUNT), alpha = 0.25, shape = 16)+
  xlim(-130, -60)+
  ylim(25, 50)+
  xlab("Latitude")+
  ylab("Longtitude")+
  scale_size_continuous(range = c(0, 20))+
  coord_fixed()

ggsave(filename = "1_b_150.png", plot_1_b,
       width = 28, height = 10, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_1_c = LEA_geo_modified_color_continental %>% ggplot(aes(y = as.numeric(INTPTLAT), x = as.numeric(INTPTLON), color = as_factor(color_group)))+
  geom_point(aes(size = STUDENT_COUNT), alpha = 0.25, shape = 16)+
  scale_x_continuous(limits = c(-125, -65), breaks = seq(-130, -60, 1), name = "Latitude")+
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 1), name = "Longitude")+
  scale_size_continuous(name = "Number of student\nin school district", labels = comma, range = c(0, 20))+
  coord_fixed()+
  ggtitle("Distribution Of Student Population By School District In The Continental 48 States")+
  scale_color_manual(values=c("red", "dark orange", "green", "blue"))+
  borders("state")+
  guides(color = "none")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave(filename = "1_c_150.png", plot_1_c,
       width = 25, height = 12, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_1_d = LEA_geo_modified_color_continental %>% ggplot(aes(y = as.numeric(INTPTLAT), x = as.numeric(INTPTLON), color = as_factor(color_group)))+
  geom_point(alpha = 0.6, shape = 16, size = 1)+
  scale_x_continuous(limits = c(-125, -65), breaks = seq(-130, -60, 1), name = "Latitude")+
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 1), name = "Longitude")+
  scale_size_continuous(name = "Number of student\nin school district", labels = comma, range = c(0, 20))+
  coord_fixed()+
  ggtitle("Geographical Location Of School District In The Continental 48 States")+
  scale_color_manual(values=c("red", "dark orange", "green", "blue"))+
  borders("state")+
  guides(color = "none")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave(filename = "1_d_150.png", plot_1_d,
       width = 25, height = 11, dpi = 150, units = "in", device='png', limitsize = FALSE)





range(as.numeric(LEA_geo_modified_color$INTPTLAT))

range(as.numeric(LEA_geo_modified_color$INTPTLON))

table(LEA_geo_modified$STATEFP)

table(LEA_geo_modified_color_continental$STATEFP)

factor(LEA_geo_modified_color_continental$STATEFP)

# Explotatory


















Student_total_by_race_gender_district = LEA_long %>%
  mutate(LEAID = str_pad(as.character(LEAID), 7, side = "left", pad = "0"))%>%
  filter(TOTAL_INDICATOR == "Derived - Subtotal by Race/Ethnicity and Sex minus Adult Education Count", SEX != "Not Specified", as.integer(str_sub(LEAID, 1, 2)) < 58) %>%
  select(LEAID, RACE_ETHNICITY, SEX, STUDENT_COUNT)

Student_total_by_race_gender_district



group_by(Student_total_by_race_gender_district, SEX) %>%
  summarize(n = n())

str(Student_total_by_race_gender_district)



Student_total_by_race_and_district = Student_total_by_race_gender_district %>%
  group_by(LEAID, RACE_ETHNICITY) %>% 
  summarize(Total = sum(STUDENT_COUNT)) %>%
  mutate(FP = str_sub(LEAID, 1, 2))

group_by(Student_total_by_race_and_district, FP, RACE_ETHNICITY)

Student_total_by_race_and_state = group_by(Student_total_by_race_and_district, FP, RACE_ETHNICITY) %>%
  summarize(Total_per_state = sum(Total)) %>%
  mutate(State_abb = State_names$stusps[match(as.integer(FP), State_names$st)])

Student_total_by_race_and_state_factored = Student_total_by_race_and_state %>%
  mutate(State_abb = State_names$stusps[match(as.integer(FP), State_names$st)]) %>%
  mutate(FP = factor(FP), RACE_ETHNICITY = factor(RACE_ETHNICITY))








Student_total_by_state = group_by(Student_total_by_race_and_district, FP) %>%
  summarize(Total_in_state = sum(Total))


White_student_total_by_state = Student_total_by_race_and_state %>%
  filter(RACE_ETHNICITY == "White")

White_student_percent_by_state_lowest_white_percent_first = full_join(White_student_total_by_state, Student_total_by_state, by = "FP") %>%
  mutate(State_abb = State_names$stusps[match(as.integer(FP), State_names$st)]) %>%
  mutate(White_percent = Total_per_state / Total_in_state) %>%
  arrange(White_percent)

Student_total_by_race_and_state_factored_lowest_white_percent_first = left_join(Student_total_by_race_and_state, White_student_percent_by_state_lowest_white_percent_first, by = "FP")

Student_total_by_race_and_state_factored_lowest_white_percent_first$State_abb.x = factor(Student_total_by_race_and_state_factored_lowest_white_percent_first$State_abb.x, levels = White_student_percent_by_state_lowest_white_percent_first$State_abb[order(White_student_percent_by_state_lowest_white_percent_first$White_percent)])







# Plots 2a and 2b aims to show the racial composition of every state by percentage. The only difference is that 2a lists the states by their
# abbreviation in alphabetical order, while 2b has them listed them by white student percentage in ascending order.

# Plot 2a and 2c are colored and arranged identically, but the bars are plotted with absolute magnitudes representing the population of students of
# the state and race.

plot_2_a = Student_total_by_race_and_state_factored %>% ggplot(aes(y = Total_per_state, x = State_abb, fill = RACE_ETHNICITY))+
  geom_col(position = "fill")+
  scale_x_discrete(name="State")+
  scale_y_continuous(name="Percent of students", labels = percent)+
  ggtitle("Percent Of Public School K-12 Students By Race And State")+
  scale_fill_discrete(name = "Race/Ethnicity")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "2_a_150.png", plot_2_a,
       width = 14, height = 7, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_2_b = Student_total_by_race_and_state_factored_lowest_white_percent_first %>% ggplot(aes(y = Total_per_state.x, x = State_abb.x, fill = RACE_ETHNICITY.x))+
  geom_col(position = "fill")+
  scale_x_discrete(name="State")+
  scale_y_continuous(name="Percent of students", labels = percent)+
  ggtitle("Percent Of Public School K-12 Students By Race And State")+
  scale_fill_discrete(name = "Race/Ethnicity")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "2_b_150.png", plot_2_b,
       width = 14, height = 7, dpi = 150, units = "in", device='png', limitsize = FALSE)




plot_2_c = Student_total_by_race_and_state_factored %>% ggplot(aes(y = Total_per_state, x = State_abb, fill = RACE_ETHNICITY))+
  geom_col(position = "stack")+
  scale_x_discrete(name="State")+
  scale_y_continuous(name="Number of students", labels = comma)+
  ggtitle("Number Of Public School K-12 Students By Race And State")+
  scale_fill_discrete(name = "Race/Ethnicity")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))



ggsave(filename = "2_c_150.png", plot_2_c,
       width = 12, height = 7, dpi = 150, units = "in", device='png', limitsize = FALSE)








# Because plot 2c fails to clearly demonstrate how the students of each state distribute across the country, plot 3a has faceted plots with
# individual labels for each race.

# Plot 3av is a variant of 3a, which is arranged vertically to better accommendate vertical scrolling.

plot_3_a = Student_total_by_race_and_state_factored %>% ggplot(aes(y = Total_per_state, x = State_abb))+
  geom_col()+
  scale_y_continuous(name="Number of students", labels = comma)+     # Note that 'labels = comma' requires the package 'scales'.
  facet_wrap(~RACE_ETHNICITY, scales = "free", ncol = 2)+
  ggtitle("The Number Of K-12 Students In Public Schools, Grouped By Race and State")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "3_a_150.png", plot_3_a,
       width = 30, height = 13, dpi = 150, units = "in", device='png', limitsize = FALSE)




plot_3_a_v = Student_total_by_race_and_state_factored %>% ggplot(aes(y = Total_per_state, x = State_abb))+
  geom_col()+
  scale_y_continuous(name="Number of students", labels = comma)+     # Note that 'labels = comma' requires the package 'scales'.
  facet_wrap(~RACE_ETHNICITY, scales = "free", ncol = 1)+
  ggtitle("The Number Of K-12 Students In Public Schools, Grouped By Race and State")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "3_a_v_150.png", plot_3_a_v,
       width = 15, height = 25, dpi = 150, units = "in", device='png', limitsize = FALSE)







LEA_geo_modified_2 = LEA_geo_modified %>%
  transmute(water_to_land = AWATER/ALAND)

Water_to_land_school_district = tibble(c(seq(0.001, 0.01, 0.001), seq(0.1, 0.9, 0.1), seq(0.99, 0.999, 0.001)), quantile(LEA_geo_modified_2$water_to_land, c(seq(0.001, 0.01, 0.001), seq(0.1, 0.9, 0.1), seq(0.99, 0.999, 0.001))))

# Info: Determine how much water area a distrit would have on average; 50th percentile has a <1% water to land ratio, however, the ratio
# goes up towards as the percentile value increases, to the point where the water area can be several times larger. Since water area
# doesn't hold any population, it is plausible that the water area can be entirely ignored.

str(LEA_geo_modified)



LEA_geo_modified_3 = left_join(LEA_geo, Student_total_by_race_and_district, by = c("GEOID" = "LEAID")) %>%
  filter(as.integer(STATEFP) < 58) %>%
  select(STATEFP, GEOID, ALAND, AWATER, INTPTLAT, INTPTLON, RACE_ETHNICITY, Total) %>%
  left_join(Student_total_by_district, by = c("GEOID" = "LEAID")) %>%
  mutate(percentage = Total / STUDENT_COUNT, density = STUDENT_COUNT * 2589988 / ALAND) %>%
  filter(is.na(STUDENT_COUNT) == F)

# 'ALAND' is in square meters, and the factor 258998 is to change the area into square miles.



LEA_geo_modified_4 = LEA_geo_modified_3

LEA_geo_modified_4_continental = LEA_geo_modified_4 %>%
  filter(!(STATEFP %in% c("02", "15")))




LEA_geo_modified_4 %>% ggplot(aes(x = ALAND, y = percentage))+
  geom_point(shape = 16, alpha = 0.01)+
  scale_x_continuous(trans = "log10")+
  facet_wrap(~RACE_ETHNICITY)

# The points are shaded proportionally to their total student count. It is necessary because without doing so, smaller
# districts with less students will be weighted the same as larger districts.

# Consider if a massive district is to be divided into several. Should the districts not be weighted by student population, 
# The same group of students, in this case, would get different amounts of representation in these two cases.

# However, do note that the cosmetic choice of shading the points based on population ended up being deprecated, as it
# leads to clarity issues.

LEA_geo_modified_4 %>% ggplot(aes(x = density, y = percentage))+
  geom_point(shape = 16, alpha = LEA_geo_modified_4$STUDENT_COUNT / 1000000)+
  scale_x_continuous(trans = "log10")+
  facet_wrap(~RACE_ETHNICITY)



arrange(LEA_geo_modified_4, desc(STUDENT_COUNT))
# Info: the largest school district has a total of 483K students - and it was the LA Unified school district.



# Due to clarity issues, the shading model adopted here in 4a is deprecated, and is replaced by 4b.
# Plot 4c is based on 4b, but instead, some values are trimmed out to make the pattern for three of the races more obvious (Note that these
# plots have individual axis).

plot_4_a = LEA_geo_modified_4 %>% ggplot(aes(x = density, y = percentage, alpha = STUDENT_COUNT))+
  geom_point(shape = 16)+
  scale_x_continuous(trans = "log10")+
  scale_alpha_continuous(range = c(0, 0.8), guide = "none")+
  facet_wrap(~RACE_ETHNICITY)+
  annotate("text", y=0.5, x=10, size = 10, label= "Deprecated")

ggsave(filename = "4_a_150.png", plot_4_a,
       width = 10, height = 8, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_4_b = LEA_geo_modified_4 %>% ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
  geom_point(shape = 16, alpha = 0.1)+
  scale_x_continuous(trans = "log10")+
  scale_size_continuous(range = c(0, 5), guide = "none")+
  scale_alpha_continuous(range = c(0, 1), guide = "none")+
  facet_wrap(~RACE_ETHNICITY)

ggsave(filename = "4_b_150.png", plot_4_b,
       width = 10, height = 8, dpi = 150, units = "in", device='png', limitsize = FALSE)





LEA_geo_modified_4_trimmed = LEA_geo_modified_4 %>%
  filter(!(RACE_ETHNICITY == "Asian" & percentage >= 0.5)) %>%
  filter(!(RACE_ETHNICITY == "Native Hawaiian or Other Pacific Islander" & percentage >= 0.02)) %>%
  filter(!(RACE_ETHNICITY == "Two or more races" & percentage >= 0.25))

# This is a manual adjustment to the percentage scale for certain races (the outliers make the majority compressed to the point where a
# general pattern cannot be shown. A total of 339 out of 91224 entries are removed across all races.


plot_4_c = LEA_geo_modified_4_trimmed %>% ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
  geom_point(shape = 16, alpha = 0.1)+
  scale_x_continuous(limits = c(0.001, 18000), trans = "log10", label = comma, name = "Students Per Square Mile")+
  scale_y_continuous(label = percent, name = "Percentage Of Students")+
  scale_size_continuous(range = c(0, 5), name = "School District\nStudent Total", label = comma)+
  scale_alpha_continuous(range = c(0, 1), guide = "none")+
  facet_wrap(~(RACE_ETHNICITY), scales = "free")+
  ggtitle("Distrubution Of Students By Race Based On Population Density")+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_text(size = 15), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "4_c_150.png", plot_4_c,
       width = 14, height =  12, dpi = 150, units = "in", device='png', limitsize = FALSE)


# Plot 4d uses linear scale for student population density instead of linear scale. The result shows that the trend observed in 4c is almost
# nonexistent in 4d.

plot_4_d = LEA_geo_modified_4_trimmed %>% ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
  geom_point(shape = 16, alpha = 0.1)+
  scale_x_continuous(limits = c(0.001, 500), label = comma, name = "Students Per Square Mile")+
  scale_y_continuous(label = percent, name = "Percentage Of Students")+
  scale_size_continuous(range = c(0, 5), name = "School District\nStudent Total", label = comma)+
  scale_alpha_continuous(range = c(0, 1), guide = "none")+
  facet_wrap(~(RACE_ETHNICITY), scales = "free")+
  ggtitle("Distrubution Of Students By Race Based On Population Density")+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_text(size = 15), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15))

ggsave(filename = "4_d_150.png", plot_4_d,
       width = 14, height =  12, dpi = 150, units = "in", device='png', limitsize = FALSE)







LEA_geo_modified_4_continental %>%
  filter(RACE_ETHNICITY == "American Indian or Alaska Native") %>%
  ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
  geom_point(shape = 16, alpha = 0.1)+
  scale_x_continuous(trans = "log10")+
  scale_alpha_continuous(range = c(0, 1), guide = "none")+
  ylim(0, 1)







# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "American Indian or Alaska Native") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 1)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "Asian") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 0.75)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "Black or African American") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 1)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "Hispanic/Latino") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 1)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "Native Hawaiian or Other Pacific Islander") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 0.02)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "Two or more races") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 0.25)
# 
# 
# 
# LEA_geo_modified_4 %>%
#   filter(RACE_ETHNICITY == "White") %>% 
#   ggplot(aes(x = density, y = percentage, size = STUDENT_COUNT))+
#   geom_point(shape = 16, alpha = 0.1)+
#   scale_x_continuous(trans = "log10")+
#   scale_alpha_continuous(range = c(0, 1), guide = "none")+
#   ylim(0, 1)

# These are tests conducted to find the appropriate range for plot scales.













LEA_geo_modified_numeric_continental = LEA_geo_modified_3 %>%
  filter(!(STATEFP %in% c("02", "15"))) %>%
  mutate(LAT_NUM = as.numeric(INTPTLAT)+0.5, LON_NUM = as.numeric(INTPTLON)+0.5, LAT_class = floor(LAT_NUM), LON_class = floor(LON_NUM))

# further removes Alaska and Hawaii, and separates school districts into brackets.


Student_count_by_race_longitude_latitude = group_by(LEA_geo_modified_numeric_continental, LAT_class, LON_class, RACE_ETHNICITY) %>%
  summarize(Race_total = sum(Total), All_total = sum(STUDENT_COUNT), Percentage = Race_total / All_total)

Student_count_by_race_longitude_latitude

sum(Student_count_by_race_longitude_latitude$All_total == 0)






# Among these plots, 5a is on the basis of percentage, while 5b is on the basis to absolute population. 5c is an attempt to use shading to deliver
# the same information as 5a, however, it fails to do so clearly, as certain races have low percentages in most area that shades aren't meaningful.

# It should be noted that these plots do have sections where there is no data - notably, in Nevada. While those areas could be filled with gray,
# as there are already too many colors in use including black, it is much preferable to leave the areas blank.

# Gradient scales do not work for this application, as the legend will clump up the different colors representing a small part of the total range
# i.e. The five colors from dark blue to cyan only ranges from 0 to 2000, less than 1% of the linear scale.





plot_5_a = ggplot(data = Student_count_by_race_longitude_latitude, aes(x = LON_class, y = LAT_class, color = Percentage))+
  geom_point(size = 5.5, alpha = 1)+
  coord_fixed()+
  borders("state")+
  scale_x_continuous(limits = c(-125, -65), breaks = seq(-130, -60, 2), name = "Latitude")+
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 1), name = "Longitude")+
  facet_wrap(~RACE_ETHNICITY, ncol = 2)+
  binned_scale(aesthetics = "color",
        scale_name = "stepsn", 
        palette = function(x) c("#001833", "#0058B8", "#028FEE", "#05DAFF", "#00FFD9", "green", "#C0FF02", "yellow", "#FDD003", "#FE9703", "red"),
        breaks = c(0.005, 0.015, 0.035, 0.065, 0.1, 0.2, 0.3, 0.45, 0.6, 0.8),
        limits = c(0, 1),
        show.limits = TRUE, 
        guide = "colorsteps",
        labels = percent
  )+
  ggtitle("Percent Of K-12 Students Within A 1 Degree Longtitude And 1 Degree Latitude Area By Race")+
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.75, "in"),
        plot.title = element_text(hjust = 0.5, size = 30), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 25))

ggsave(filename = "5_a.png", plot_5_a,
       width = 25, height = 23.5, dpi = 150, units = "in", device='png', limitsize = FALSE)




plot_5_b = ggplot(data = Student_count_by_race_longitude_latitude, aes(x = LON_class, y = LAT_class, color = Race_total))+
  geom_point(size = 5.5, alpha = 1)+
  coord_fixed()+
  borders("state")+
  scale_x_continuous(limits = c(-125, -65), breaks = seq(-130, -60, 2), name = "Latitude")+
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 1), name = "Longitude")+
  facet_wrap(~RACE_ETHNICITY, ncol = 2)+
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#001833", "#0058B8", "#028FEE", "#05DAFF", "#00FFD9", "green", "#C0FF02", "yellow", "#FDD003", "#FE9703", "red"),
               breaks = c(25, 100, 250, 1000, 2000, 5000, 10000, 25000, 50000, 100000),
               limits = c(0, 250000),
               show.limits = TRUE, 
               guide = "colorsteps",
               labels = comma
  )+
  ggtitle("Number Of K-12 Students Within A 1 Degree Longtitude And 1 Degree Latitude Block By Race")+
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.75, "in"),
        plot.title = element_text(hjust = 0.5, size = 30), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 25))



ggsave(filename = "5_b.png", plot_5_b,
       width = 25, height = 23.5, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_5_c = ggplot(data = Student_count_by_race_longitude_latitude, aes(x = LON_class, y = LAT_class, alpha = Percentage))+
  geom_point(size = 5.5, color = "#3eb0f7")+
  scale_alpha_continuous(range = c(0, 0.8), guide = "none")+
  coord_fixed()+
  scale_color_manual(values="#3eb0f7")+
  borders("state")+
  scale_x_continuous(limits = c(-125, -65), breaks = seq(-130, -60, 2), name = "Latitude")+
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 1), name = "Longitude")+
  facet_wrap(~RACE_ETHNICITY, ncol = 2)+
  ggtitle("Percent Of K-12 Students Within A 1 Degree Longtitude And 1 Degree Latitude Area By Race")+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 30), 
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 25))


ggsave(filename = "5_c.png", plot_5_c,
       width = 25, height = 23, dpi = 150, units = "in", device='png', limitsize = FALSE)






# Info: total number/percentage of students in the whole country by race

Countrywide_student_total_by_race = summarize(group_by(Student_total_by_race_and_state, RACE_ETHNICITY), total = sum(Total_per_state))

Countrywide_percentage_by_race = mutate(Countrywide_student_total_by_race, all_total = sum(Countrywide_student_total_by_race$total), percentage = total/all_total)


