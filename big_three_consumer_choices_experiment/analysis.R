# Code to reproduce Figure 8 from Noguchi and Stewart (2018)
# Copyright Neil Stewart 2022
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or any later version.

library(data.table)
library(ggplot2)
sessionInfo()



# Data from the 353 participants from Noguchi and Stewart (2018) Figure 8
data <- fread("data_from_Takao.csv")
nrow(unique(data[, .(participant_id)]))

data[, choice_type:=factor(choice_type, levels=c("pairwise", "attraction", "compromise", "similarity"))]



# Each row is a choice
data[, freq:=1]



# Add rows with zero frequency for choices the participant had but didn't make
choices <- unique(data[, .(choice_type, choice)])[order(choice_type, choice)]
trials <- unique(data[, .(participant_id, choice_type, third_alternative)])[order(participant_id, choice_type, third_alternative)]
skeleton <- merge(trials, choices, by=c("choice_type"), allow.cartesian=TRUE)[order(participant_id, choice_type, third_alternative, choice)]
all <- merge(data, skeleton, by=c("participant_id", "choice_type", "third_alternative", "choice"), all=TRUE)
all[is.na(freq), freq:=0]



# Calculate proportions of choices for each participant
freqs <- all[, .(freq=sum(freq)), by=.(participant_id, choice_type, third_alternative, choice)]
freqs[, total:=sum(freq), by=.(participant_id, choice_type, third_alternative)]
freqs[,prop:=freq/total]



# Average over participants to get means for each type of choice
means <- freqs[, .(prop=mean(prop), sd=sd(prop), N=length(prop)), by=.(choice_type, third_alternative, choice)]



# Plot Figure 8 replication
means[, se:=sd/sqrt(N)]
means[, lower:=prop-1.96*se]
means[, upper:=prop+1.96*se]

means[,x:=ifelse(choice %in% c("A", "B"), choice, third_alternative)]
means[,x:=factor(x, levels=c("Da", "Ca", "Sa", "A", "B", "Db", "Cb", "Sb"))]

means

ggplot(means, aes(x=x, y=prop, ymin=lower, ymax=upper, col=third_alternative, group=third_alternative)) + geom_point() + facet_wrap(~choice_type, scales="free_x", ncol=4) + guides(col="none") + ylim(0,0.65) + labs(y="Choice Proportion", x="Choice") + geom_errorbar(width=0.2) + geom_line()
ggsave("figure_8.pdf")


