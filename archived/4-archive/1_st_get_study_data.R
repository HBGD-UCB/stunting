
#devtools::install_github("HBGDki/ghap")

library(ghap)

# this is for data downloaded from external git
set_git_base_path("U:/RawData")
get_git_base_path()

#studies <- get_study_list()

astudies <- get_study_list_anthro()
astudies<-as.data.frame(astudies)

print(astudies[,-c(1,3,4)])

table(astudies$intervention_type)
table(astudies$notes)




head(astudies)
tail(astudies[, c("study_id", "short_id", "short_description")], 3)


#Write list of studies
write.table(astudies, "U:/Data/astudies.txt", sep="\t")




#Get all studies with anthropometry data
#(Note: takes a long time: run overnight)
#jade needs to run
astudies <- get_study_list_anthro()
for (id in astudies$short_id){
  tmp <- use_study(id)
}
