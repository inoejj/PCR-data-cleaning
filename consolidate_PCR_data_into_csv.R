library(pacman)
p_load(tidyr, dplyr, stringr, quantmod, ggplot2, reshape2 , scales, purrr, data.table, lubridate, countrycode,
       gridExtra, ggthemes, viridis, knitr, gridExtra)
setwd("C:/Users/Jia Jie Choong/Desktop/R with Zhanyou/JJ please make these into excel files/rhamp tape correct extraction dna")


allFiles= list.files(pattern = "*.txt")
number_of_files= length(allFiles); number_of_files
all_data =data.frame()

for (i in 1:number_of_files){
  file_name_temp = as.character(allFiles[i])
  file_nales= rep(file_name_temp, 384)
  text1= readLines(allFiles[i])
 #  file_temp = read.csv(allFiles[i], sep = "\t", header = F)

  #r=grep("<string>([0-9]+.[0-9].*?)</string>", text1)
  r=regexpr("[A-P].*", text1)

  # FAM = file_temp[22:229,]
  mm=regmatches(text1, r)
  mm =gsub("\t", ",", mm)
  mm
  length(mm)
  FAM=mm[9:24]
  class(FAM)
  FAM_df = data.frame(FAM); dim(FAM_df); head(FAM_df)
  FAM_csv=separate(FAM_df, FAM,into =  c("Letter", "col_1",  "col_2",  "col_3",  "col_4",  "col_5",  "col_6",  "col_7",  "col_8",  "col_9",  "col_10",  "col_11",  "col_12",  "col_13",  "col_14",  "col_15",  "col_16",  "col_17",  "col_18",  "col_19",  "col_20",  "col_21",  "col_22",  "col_23",  "col_24"), sep=',')
  # write.csv(FAM_csv, "ooooooooo123.csv")
  
  VIC=mm[27:42]; VIC_df = data.frame(VIC)
  VIC_csv = separate(VIC_df, VIC, into =c("Letter", "col_1",  "col_2",  "col_3",  "col_4",  "col_5",  "col_6",  "col_7",  "col_8",  "col_9",  "col_10",  "col_11",  "col_12",  "col_13",  "col_14",  "col_15",  "col_16",  "col_17",  "col_18",  "col_19",  "col_20",  "col_21",  "col_22",  "col_23",  "col_24"), sep=',' )
  
  ROX=mm[45:60]; ROX_df = data.frame(ROX)
  ROX_csv = separate(ROX_df, ROX, into=c("Letter", "col_1",  "col_2",  "col_3",  "col_4",  "col_5",  "col_6",  "col_7",  "col_8",  "col_9",  "col_10",  "col_11",  "col_12",  "col_13",  "col_14",  "col_15",  "col_16",  "col_17",  "col_18",  "col_19",  "col_20",  "col_21",  "col_22",  "col_23",  "col_24"), sep=',' )
  
  ROX_csv=ROX_csv[, grep("col", names(ROX_csv))]; dim(ROX_csv)
  VIC_csv = VIC_csv[, grep("col", names(VIC_csv))]; dim(VIC_csv)
  FAM_csv = FAM_csv[, grep("col", names(FAM_csv))]; dim(FAM_csv)
  

wells= rep("", 384)
counter=1
  letters=c("A", "B","C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")

  for  (k in 1:24){
    if( k<10){
      k=paste0("0", k, sep="")
      }
  for (j in letters){
    
    pp=  paste0(j, k, sep="")
    wells[counter]= pp
    counter=counter+1
    #print(pp)
    }
  }
ROX_List= unlist(ROX_csv)
FAM_List= unlist(FAM_csv)
VIC_List = unlist(VIC_csv)

df_temp = data.frame(wells, ROX_List, FAM_List, VIC_List, file_nales)
names(df_temp) = c("Well","ROX", "FAM", "VIC", "Marker_name")
dim(df_temp)
head(df_temp)

if(i==1){
  
  all_data = df_temp
} else{
  all_data = rbind(all_data, df_temp)
}
print(dim(all_data))

}

write.csv(all_data, "rhamp_tape_data.csv", row.names = F)
