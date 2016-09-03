library(data.table)

ebola.april <- data.table(read.csv2("who_ebola_2016-04-27.csv", sep=","))
ebola.may   <- data.table(read.csv2("who_ebola_2016-05-11.csv", sep=","))
#ebola <- rbind(ebola.april, ebola.may)
ebola <- data.table(read.csv2("who_ebola.csv", sep=","))

cols <- c('Country', 'Age.Group', 'Gender', 'Ebola.measure', 'Case.definition', 'Indicator.type', 'Data.package.ID', 'Data.as.of', 'Numeric')
uncols <- c('Outbreak.identifier', 'Display.Value', 'Epi.week', 'Low', 'High', 'Comments')

ebola.april[Country=="Guinea", !uncols, with=FALSE]
ebola.may[Country=="Guinea", !uncols, with=FALSE]

ebola.april[Country=="Guinea" & Age.Group=="15-44" & Ebola.measure=="Number of cases" & Case.definition == "Confirmed" & Indicator.type == "Cumulative", !uncols, with=FALSE]
ebola.may[Country=="Guinea" & Age.Group=="15-44" & Ebola.measure=="Number of cases" & Case.definition == "Confirmed" & Indicator.type == "Cumulative", !uncols, with=FALSE]

ebola[Country=="Guinea" & Age.Group=="45+" & Ebola.measure=="Number of cases" & Case.definition == "Confirmed" & Indicator.type == "In past 21 days", !uncols, with=FALSE]
ebola.slice <- ebola[Country=="Guinea" & Ebola.measure=="Number of cases" & Case.definition == "Confirmed" & Indicator.type == "In past 21 days", !uncols, with=FALSE]

ebola.slice[, Date := as.Date(Data.as.of, "%d %B %Y")]
ebola.slice[, Cases := as.numeric(as.character(Numeric))]

ebola.slice <- ebola.slice[order(Date)]

scatter.smooth(Numeric ~ Cases, ebola.slice)
scatter.smooth(ebola.slice$Date, ebola.slice$Cases)

colnames(ebola)
