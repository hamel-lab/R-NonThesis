#######################################
#                                     #
# Flathead catfish relative abundance #
# compared with local land use using  #
#     publically available data       #
#                                     #
#######################################


library(plyr)
library(dplyr)


# set wd
setwd("D:/WILD 8980/Project")

## add descriptive attribute fields to numerical ID according to national landfire documentation
# store attribute documentation for reference
d_attributes <- read.csv('./GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.csv')
head(d_attributes)

# clean up d_attributes by selecting only desired columns
d_att <- d_attributes[, c('Value', 'CL', 'NVC_CLASS', 'SC', 'NVC_SUBCL', 'FRM', 'NVC_FORM', 'DIV', 'NVC_DIV', 'MACRO_CD', 'NVC_MACRO', 'GR', 'NVC_GROUP', 'LEVEL3', 'ECOLSYS_LU', 'NVCMES')]
head(d_att)

 # section 1
section1 <- read.csv("./s1_500m_lulc.csv")
head(section1)

d_s1 <- section1[, c('Value', 'Count')]
print(d_s1)

# join d_s1 and and d_att on 'Value' vector and store as new object
   # 'fills in' descriptive attributes of numerical ID in 'Value'

s1_att <- inner_join(d_s1, d_att, by = 'Value', copy = FALSE)

# save s1_att as a .csv for back-up
write.csv(s1_att, file = "s1_500m_att.csv")

# section 2
section2 <- read.csv('./s2_500m_lulc.csv')
head(section2)

d_s2 <- section2[, c('Value', 'Count')]
head(d_s2)

s2_att <- inner_join(d_s2, d_att, by = 'Value', copy = FALSE)
head(s2_att)

write.csv(s2_att, file = "s2_500m_att.csv")

# section 3
section3 <- read.csv('./s3_500m_lulc.csv')
head(section3)

d_s3 <- section3[, c('Value', 'Count')]
head(d_s3)

s3_att <- inner_join(d_s3, d_att, by = 'Value', copy = FALSE)
head(s3_att)

write.csv(s3_att, file = "./s3_500m_att.csv")

# section 4 
section4 <- read.csv('./s4_500m_lulc.csv')
head(section4)

d_s4 <- section4[, c('Value', 'Count')]
head(d_s4)

s4_att <- inner_join(d_s4, d_att, by = 'Value', copy = FALSE)
head(s4_att)

write.csv(s4_att, file = "./s4_500m_att.csv")

# section 5
section5 <- read.csv('./s5_500m_lulc.csv')
head(section5)

d_s5 <- section5[, c('Value', 'Count')]
head(d_s5)

s5_att <- inner_join(d_s5, d_att, by = 'Value', copy = FALSE)
head(s5_att)

write.csv(s5_att, file = "./s5_500m_att.csv")

# section 6
section6 <- read.csv('./s6_500m_lulc.csv')
head(section6)

d_s6 <- section6[, c('Value', 'Count')]
head(d_s6)

s6_att <- inner_join(d_s6, d_att, by = 'Value', copy = FALSE)
head(s6_att)

write.csv(s6_att, file = "./s6_500m_att.csv")

# section 7
section7 <- read.csv('./s7_500m_lulc.csv')
head(section7)

d_s7 <- section7[, c('Value', 'Count')]
head(d_s7)

s7_att <- inner_join(d_s7, d_att, by = 'Value', copy = FALSE)
head(s7_att)

write.csv(s7_att, file = "./s7_500m_att.csv")

# section 8
section8 <- read.csv('./s8_500m_lulc.csv')
head(section8)

d_s8 <- section8[, c('Value', 'Count')]
head(d_s8)

s8_att <- inner_join(d_s8, d_att, by = 'Value', copy = FALSE)
head(s8_att)

write.csv(s8_att, file = "./s8_500m_att.csv")

# section 9 
section9 <- read.csv('./s9_500m_lulc.csv')
head(section9)

d_s9 <- section9[, c('Value', 'Count')]
head(d_s9)

s9_att <- inner_join(d_s9, d_att, by = 'Value', copy = FALSE)
head(s9_att)

write.csv(s9_att, file = "./s9_500m_att.csv")

# section 10
section10 <- read.csv('./s10_500m_lulc.csv')
head(section10)

d_s10 <- section10[, c('Value', 'Count')]
head(d_s10)

s10_att <- inner_join(d_s10, d_att, by = 'Value', copy = FALSE)
head(s10_att)

write.csv(s10_att, file = "./s10_500m_att.csv")

# section 11
section11 <- read.csv('./s11_500m_lulc.csv')
head(section11)

d_s11 <- section11[, c('Value', 'Count')]
head(d_s11)

s11_att <- inner_join(d_s11, d_att, by = 'Value', copy = FALSE)
head(s11_att)

write.csv(s11_att, file = './s11_500m_att.csv')

###############


###################################################
# notes: proximity to trib; SA water (open water and neighboring water-y cells); discharge?,
# sinewosity, tidal/inudation areas; 
#