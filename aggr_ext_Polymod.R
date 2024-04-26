library(Bernadette)
load("contact_matrices_home.rda")
load("contact_matrices_work.rda")
load("contact_matrices_school.rda")
load("contact_matrices_other_locations.rda")
# Import the projected contact matrix
conmat_home <- data.frame(contact_matrices_home[["Germany"]] )
conmat_work <- data.frame(contact_matrices_work[["Germany"]] )
conmat_school <- data.frame(contact_matrices_school[["Germany"]] )
conmat_other_locations <- data.frame(contact_matrices_other_locations[["Germany"]] )
age_groups <- c("0-4",
                "5-9",
                "10-14",
                "15-19",
                "20-24",
                "25-29",
                "30-34",
                "35-39",
                "40-44",
                "45-49",
                "50-54",
                "55-59",
                "60-64",
                "65-69",
                "70-74",
                "75+")
rownames(conmat_home) <- colnames(conmat_home) <- age_groups
rownames(conmat_work) <- colnames(conmat_work) <- age_groups
rownames(conmat_school) <- colnames(conmat_school) <- age_groups
rownames(conmat_other_locations) <- colnames(conmat_other_locations) <- age_groups
age_distr <- Bernadette::age_distribution(country = "Germany", year = 2020)
#Lookup table:
lookup_table <- data.frame(Initial = age_distr$AgeGrp,
                         Mapping = c(rep("0-18",  4),
                         rep("19-44", 5),
                         rep("45-64", 4),
                         rep("65+"  , 3)))

# lookup_table <- data.frame(Initial = age_distr$AgeGrp,
#                            Mapping = c(rep("0-11",  2),
#                                        rep("12-17",  2),
#                                        rep("18-24",  1),
#                                        rep("25-44", 4),
#                                        rep("45-64", 4),
#                                        rep("65-74", 2),
#                                        rep("75+"  , 1)))
# Aggregate the age distribution table:
aggr_age <- Bernadette::aggregate_age_distribution(age_distr, lookup_table)
# Aggregate the contact matrix:
aggr_cm_home <- Bernadette::aggregate_contact_matrix(conmat_home, lookup_table, aggr_age)
aggr_cm_work <- Bernadette::aggregate_contact_matrix(conmat_work, lookup_table, aggr_age)
aggr_cm_school <- Bernadette::aggregate_contact_matrix(conmat_school, lookup_table, aggr_age)
aggr_cm_other_locations <- Bernadette::aggregate_contact_matrix(conmat_other_locations, lookup_table, aggr_age)

aggr_cm_home <- aggr_cm_home[, c("0-18", "19-44", "45-64","65+")]
aggr_cm_home <- aggr_cm_home[c("0-18", "19-44", "45-64","65+"),]
aggr_cm_work <- aggr_cm_work[, c("0-18", "19-44", "45-64","65+")]
aggr_cm_work <- aggr_cm_work[c("0-18", "19-44", "45-64","65+"),]
aggr_cm_school <- aggr_cm_school[, c("0-18", "19-44", "45-64","65+")]
aggr_cm_school <- aggr_cm_school[c("0-18", "19-44", "45-64","65+"),]
aggr_cm_other_locations <- aggr_cm_other_locations[, c("0-18", "19-44", "45-64","65+")]
aggr_cm_other_locations <- aggr_cm_other_locations[c("0-18", "19-44", "45-64","65+"),]

# aggr_cm_home <- aggr_cm_home[, c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+")]
# aggr_cm_home <- aggr_cm_home[c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+"),]
# aggr_cm_work <- aggr_cm_work[, c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+")]
# aggr_cm_work <- aggr_cm_work[c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+"),]
# aggr_cm_school <- aggr_cm_school[, c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+")]
# aggr_cm_school <- aggr_cm_school[c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+"),]
# aggr_cm_other_locations <- aggr_cm_other_locations[, c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+")]
# aggr_cm_other_locations <- aggr_cm_other_locations[c("0-11", "12-17", "18-24", "25-44", "45-64","65-74","75+"),]

polymod <- rbind(unlist(aggr_cm_home[-1,-1]),
                 unlist(aggr_cm_work[-1,-1]),
                 unlist(aggr_cm_school [-1,-1]),
                 unlist(aggr_cm_other_locations[-1,-1]))

# polymod <- rbind(unlist(aggr_cm_home),
#                  unlist(aggr_cm_work),
#                  unlist(aggr_cm_school),
#                  unlist(aggr_cm_other_locations))

folder_path <- "C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Polymod"
file_path <- file.path(folder_path, "DE.csv")
#write.table(polymod, file = file_path, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(polymod, file = file_path, sep = ",", row.names = FALSE, col.names = TRUE)
