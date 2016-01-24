
swapout.IDs <- function () {
    
    ContractHeader <- d1$ContractHeader 
    ContractDetail <- d1$ContractDetail
    
    rws <- suppressWarnings(which(!is.na(as.numeric(ContractHeader$ContractID))))
    m1 <- data.frame (oldID=ContractHeader$ContractID[rws],
                      newID=paste(ContractHeader$woiid[rws], ContractHeader$EffSn[rws],sep="-"), stringsAsFactors=FALSE)
    ContractHeader$ContractID[rws] <- m1$newID
    
    news <- as.character(m1$newID[match(ContractDetail$ContractID, m1$oldID)])
    ContractDetail$ContractID[!is.na(news)] <- news[!is.na(news)]
   
}


download.contracts.one <- function (this.url) {
    ## this.url = db.orig
    biginfo <- html(this.url)
    d1 <- html_table(biginfo, header=TRUE)
    names(d1) <- c("rawResponses", "Team", "Player", "ContractHeader", "ContractHeaderNew",
                   "ContractDetail", "ContractDetailNew",
                   "ContractBonus", "LOOKUP", "Calculations",
                   "PerfBonuses",

##                   "ContractAlloc", "PlayerStatus",
                   "RetainedSalary", "RetainedHeader","RetainedDetail",
                   "Buyouts","ProcessedBuyouts",
                   "DeadHeader","DeadDetail",
                   "PerformanceBonusOverages", "BonusOverage",
                   "Group1"  #, "TransactionHeader","TransactionDetail","Calculations"
                   )
    for (index in 1:length(d1)) {
        colnames(d1[[index]]) <- d1[[index]][1,];
        d1[[index]] <- d1[[index]][-(1:2), -1]
    }

    return(d1)
}

download.contracts.current <- function (this.url) {
    ## this.url = db.new
    biginfo <- html(this.url)
    d1 <- html_table(biginfo, header=TRUE)
    names(d1) <- c("ContractHeader", "ContractDetail",
                   "ContractBonus")
    for (index in 1:length(d1)) {
        colnames(d1[[index]]) <- d1[[index]][1,];
        d1[[index]] <- d1[[index]][-(1:2), -1]
    }

    return(d1)
}



join.tables <- function(d1, new.data) {

    ##d1$rawResponses <- rbind(d1$rawResponses, base.data$rawResponses)
    d1$ContractHeader <- rbind(d1$ContractHeader, new.data$ContractHeader) 
    d1$ContractDetail <- rbind(d1$ContractDetail, new.data$ContractDetail)
    d1$ContractBonus <- rbind(d1$ContractBonus, new.data$ContractBonus)
    ##d1$ContractAlloc <- rbind(d1$ContractAlloc, base.data$ContractAlloc)
    ##d1$PlayerStatus <- rbind(d1$PlayerStatus, base.data$PlayerStatus)
    ##d1$TransactionHeader <- rbind(d1$TransactionHeader, base.data$TransactionHeader)
    ##d1$TransactionDetail <- rbind(d1$TransactionDetail, base.data$TransactionDetail)
    ##d1$RetainedSalary <- rbind(d1$RetainedSalary, base.data$RetainedSalary)
    ##d1$Buyouts <- rbind(d1$Buyouts, base.data$Buyouts)
    ##d1$PerformanceBonusOverages <- rbind(d1$PerformanceBonusOverages, base.data$PerformanceBonusOverages)
    ##d1$Group1 <- rbind(d1$Group1, base.data$Group1)
    ##d1$PerfBonuses <- rbind(d1$PerfBonuses, base.data$PerfBonuses)
    ##d1$ProcessedBuyouts <- rbind(d1$ProcessedBuyouts, base.data$ProcessedBuyouts)
    
    return(d1)

}

make.contracts.files <- function (redo.one=FALSE) {

    message ("Downloading contract files from Google Docs.")

    db.orig <- "https://docs.google.com/spreadsheets/d/1mXrRvjHOejifcLsdtzve-I2m-8oF70cGmb_jj5ppPOU/pubhtml"
    db.new <- "https://docs.google.com/spreadsheets/d/1tqB--T4FfyGud-GPPNdFVBJwXpL5k164jVmn9tbqju0/pubhtml"
    
    if (redo.one) {
        base.data <- download.contracts.one (db.orig)
        save (base.data, file="source-data/base-contracts.RData")
    } else {
        load ("source-data/base-contracts.RData")
    }
    
    d0 <- download.contracts.current (db.new)
    d1 <- join.tables (base.data, d0)

    save (d1, file = "source-data/raw-contracts-downloaded.RData")
    ## load ("source-data/raw-contracts-downloaded.RData")
    
    rawResponses <- d1$rawResponses
    Team <- d1$Team
    Player <- d1$Player
    ContractHeader <- d1$ContractHeader
    ContractDetail <- d1$ContractDetail
    ContractBonus <- d1$ContractBonus
    ContractAlloc <- d1$ContractAlloc
    PlayerStatus <- d1$PlayerStatus
    TransactionHeader <- d1$TransactionHeader
    TransactionDetail <- d1$TransactionDetail
    RetainedSalary <- d1$RetainedSalary
    Buyouts <- d1$Buyouts
    PerformanceBonusOverages <- d1$PerformanceBonusOverages

 
    ## attach(d1)
    Team <- rename(Team, oldid=TeamID, TeamID=WoiID)
    
    ContractMinor <- sqldf("
SELECT
ContractHeader.ContractID,
SUM(ContractDetail.MinorSalary) AS [TOTMIN],
SUM(ContractDetail.NHLSalary + ContractDetail.SignBonus) AS [TOTCONT],
SUM(ContractDetail.NHLSalary + ContractDetail.SignBonus)/ContractHeader.ContractLength as [Calc-AAV]
FROM
ContractHeader LEFT JOIN
ContractDetail
ON
ContractHeader.ContractID = ContractDetail.ContractID
group by ContractHeader.ContractID
",drv="SQLite")


    ## Player.ContractStatus,Player.NHLExpr,Player.YrsPro,Player.WhereAbt,Player.RtrStatus,Player.AgeELFA,


    ContractInfo <- sqldf("
SELECT 
Player.woiid,
Player.PlayerName,
Team.TeamID,
Team.ShortName,
case when ContractMinor.TotMin > 0 THEN 'TWO' ELSE 'ONE' END AS [Way],
ContractMinor.TotMin,
CASE WHEN (cONTRACTHeader.aav <> 0 AND ContractHeader.AAV <> 'NULL' AND ContractHeader.AAV IS NOT NULL) THEN ContractHeader.AAV ELSE CONTRACTMINOR.[calc-aav] end as [aav],
ContractMinor.[Calc-AAV],
ContractHeader.ContractID,
ContractHeader.ElgEffSn,
ContractHeader.EffSn,
ContractHeader.ContractLength,
ContractHeader.EffSn + ContractDetail.ContractYear - 1 AS [Year],
ContractHeader.AAV AS [Written-AAV],
ContractDetail.DetailID,
ContractDetail.NHLSalary,
ContractDetail.SignBonus,
ContractDetail.NDC,
ContractDetail.NTC,
ContractDetail.Retired,
ContractDetail.BoughtOut,
ContractDetail.slid,
ContractDetail.BonusElg,
ContractDetail.MaxPerfBonus,
ContractDetail.PerfBonusMet,
case when ContractDetail.MinorSalary is null then 0 else ContractDetail.MinorSalary end AS [MinorSal],
case when ContractDetail.JuniorSalary is null then 0 else ContractDetail.JuniorSalary end AS [JuniorSal],
case when (ContractDetail.MinSalary is null or ContractDetail.MinSalary = 0) then (ContractDetail.NHLSalary +  ContractDetail.SignBonus) ELSE ContractDetail.minSalary  end AS [MinimumSal],
ContractHeader.[Source],
ContractHeader.FAStatus,
Player.ContractStatus,
Player.NHLExpr,
Player.YrsPro,
Player.WhereAbt,
Player.RtrStatus,
Player.AgeELFA,
Player.pos
FROM
Player
LEFT join
Team
ON
Player.[CurrentTeam] = Team.TeamID
left JOIN
ContractHeader
on
Player.woiid = ContractHeader.woiid
left JOIN
ContractDetail
ON
ContractHeader.ContractID = ContractDetail.ContractID
left join
ContractMinor
on
ContractHeader.ContractID = ContractMinor.ContractID
",drv="SQLite") %>% left_join (ContractBonus)

## CASE WHEN ContractHeader.FAStatus = \"RFA\" THEN 0 ELSE 1 END AS [FAStatus],

    CurrentContract <- sqldf("
SELECT
ContractHeader.woiid,
ContractHeader.ContractID,
ContractHeader.FAStatus,
case when ContractMinor.TotMin > 0 THEN 'TWO' ELSE 'ONE' END AS [Way]
FROM ContractHeader LEFT JOIN ContractMinor ON ContractHeader.ContractID = ContractMinor.ContractID
WHERE
ContractHeader.EffSn + ContractHeader.ContractLength - 1 >= 2015
and
ContractHeader.EffSn + 0 < 2016
",drv="SQLite")

    TeamView <- sqldf("
select 
ContractInfo.woiid,
ContractInfo.PlayerName,
ContractInfo.TeamID,
ContractInfo.ShortName,
TRIM(ContractInfo.ContractStatus) AS ContractStatus,
ContractInfo.RtrStatus,
CurrentContract.Way,
ContractInfo.WhereAbt,
ContractInfo.pos,
SUM(CASE WHEN ContractInfo.Year = 2009 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2009CAP,
SUM(CASE WHEN ContractInfo.Year = 2009 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2009ACT,
SUM(CASE WHEN ContractInfo.Year = 2010 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2010CAP,
SUM(CASE WHEN ContractInfo.Year = 2010 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2010ACT,
SUM(CASE WHEN ContractInfo.Year = 2011 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2011CAP,
SUM(CASE WHEN ContractInfo.Year = 2011 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2011ACT,
SUM(CASE WHEN ContractInfo.Year = 2012 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2012CAP,
SUM(CASE WHEN ContractInfo.Year = 2012 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2012ACT,
SUM(CASE WHEN ContractInfo.Year = 2013 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2013CAP,
SUM(CASE WHEN ContractInfo.Year = 2013 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2013ACT,
SUM(CASE WHEN ContractInfo.Year = 2014 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2014CAP,
SUM(CASE WHEN ContractInfo.Year = 2014 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2014ACT,
SUM(CASE WHEN ContractInfo.Year = 2015 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2015CAP,
SUM(CASE WHEN ContractInfo.Year = 2015 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2015ACT,
SUM(CASE WHEN ContractInfo.Year = 2016 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2016CAP,
SUM(CASE WHEN ContractInfo.Year = 2016 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2016ACT,
SUM(CASE WHEN ContractInfo.Year = 2017 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2017CAP,
SUM(CASE WHEN ContractInfo.Year = 2017 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2017ACT,
SUM(CASE WHEN ContractInfo.Year = 2018 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2018CAP,
SUM(CASE WHEN ContractInfo.Year = 2018 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2018ACT,
SUM(CASE WHEN ContractInfo.Year = 2019 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2019CAP,
SUM(CASE WHEN ContractInfo.Year = 2019 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2019ACT,
SUM(CASE WHEN ContractInfo.Year = 2020 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2020CAP,
SUM(CASE WHEN ContractInfo.Year = 2020 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2020ACT,
SUM(CASE WHEN ContractInfo.Year = 2021 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2021CAP,
SUM(CASE WHEN ContractInfo.Year = 2021 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2021ACT,
SUM(CASE WHEN ContractInfo.Year = 2022 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2022CAP,
SUM(CASE WHEN ContractInfo.Year = 2022 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2022ACT,
SUM(CASE WHEN ContractInfo.Year = 2023 THEN ContractInfo.[AAV] ELSE 0 END) AS YE2023CAP,
SUM(CASE WHEN ContractInfo.Year = 2023 THEN ContractInfo.NHLSalary +  ContractInfo.SignBonus ELSE 0 END) AS YE2023ACT,
CurrentContract.FAStatus
from ContractInfo
left join 
CurrentContract on ContractInfo.woiid = currentcontract.woiid
GROUP BY
ContractInfo.woiid,
ContractInfo.PlayerName,
ContractInfo.TeamID,
ContractInfo.ShortName,
TRIM(ContractInfo.ContractStatus),
ContractInfo.RtrStatus,
ContractInfo.WhereAbt,
CurrentContract.FAStatus,
CurrentContract.Way
",drv="SQLite")

## Join!
##    Player <- rename(Player, WOI.ID = AltID)

    
    ContractInfo <- mutate(ContractInfo,
                           ##woiid = Player$woiid[match(ContractInfo$woiid, Player$woiid)],
                           MaxPerfBonus = anac(MaxPerfBonus),
                           BonusMax = anac(BonusMax),
                           PerfBonusMet = anac(PerfBonusMet),
                           BonusMet = anac(BonusMet))

    ## Fix performance bonuses -- from each table.
    ContractInfo$MaxPerfBonus[is.na(ContractInfo$MaxPerfBonus)] <- 0
    ContractInfo$BonusMax[is.na(ContractInfo$BonusMax)] <- 0
    ContractInfo$PerfBonusMet[is.na(ContractInfo$PerfBonusMet)] <- 0
    ContractInfo$BonusMet[is.na(ContractInfo$BonusMet)] <- 0

    ContractInfo <- mutate (ContractInfo,
                            BonusMax = pmax (MaxPerfBonus, BonusMax),
                            BonusMet = pmax (PerfBonusMet, BonusMet)) %>%
                                select (-MaxPerfBonus, -PerfBonusMet)
                            
    
    ## write.csv(ContractInfo, "source-data/ContractInfo.csv")
    write.csv(ContractMinor, "source-data/ContractMinor.csv")
    write.csv(CurrentContract, "source-data/CurrentContract.csv")
    write.csv(TeamView, "source-data/TeamView.csv")

    ##save (d1, file="source-data/contracts-raw.RData")
    
    message ("Processing contract data.")

    
    full.contracts <-
        mutate(ContractInfo,
               NHLSalary = gsub("[^\\$0-9\\.]","",NHLSalary),
               SignBonus = gsub("[^\\$0-9\\.]","",SignBonus),
               MinorSal = gsub("[^\\$0-9\\.]","",MinorSal),
               JuniorSal = gsub("[^\\$0-9\\.]","",JuniorSal),
               MinimumSal = gsub("[^\\$0-9\\.]","",MinimumSal)) %>%
                   mutate (TotalComp = (anac(NHLSalary) + anac(SignBonus)),
                           ContractID = paste0("x", ContractID)) %>% # read as string instead of customer
                               rename (NHL.Salary=NHLSalary,
                                       Signing.Bonus=SignBonus,
                                       Contract.Length=ContractLength,
                                       Contract.ID=ContractID)
    colnames(full.contracts)[colnames(full.contracts) == "Written-AAV"] <- "Written.AAV"
    
    # more formatting
    full.contracts <- mutate (full.contracts,
                              woiid=as.character(woiid),
                              TotalComp = anac(TotalComp),
                              Year = anac(Year),
                              Written.AAV = anac(Written.AAV),
                              NHL.Salary = anac(NHL.Salary),
                              MinorSal = anac(MinorSal),
                              Signing.Bonus = anac(Signing.Bonus))
    full.contracts$Signing.Bonus[is.na(full.contracts$Signing.Bonus) & !is.na(full.contracts$NHL.Salary)] <- 0
    
    # USA Today - player, year, $
    sub.contracts <- get(load ("source-data/roster-salaries.RData")) %>%
        rename (woiid = woi.id, TotalComp = salary) %>%
            mutate (Year = substr (season, 5, 8),
                    Contract.ID = paste0(Year,woiid),
                    Source = "USA Today") %>%
                        select (woiid, Year, TotalComp, Contract.ID)
    
    unmatched.sub.contracts <-
        filter (sub.contracts,
                is.na (match(paste(woiid, Year),
                             paste(full.contracts$woiid, full.contracts$Year)))) %>%
                                 mutate (Year = anac(Year),
                                         TotalComp = anac(TotalComp))
    

    #roster.unique$woi.id[roster.unique$woi.id == "mitchjo86"] <- "mitchjo85"



    # combine with USA Today contracts rbind_list don't have to have all matched columns    
    bigtime <- rbind_list (full.contracts, unmatched.sub.contracts) %>%
        mutate(Name = roster.unique$firstlast[match(woiid, roster.unique$woi.id)],
               Age = ageAsOfSep15 (paste0(Year-1, Year), as.character(roster.unique$DOB[match(woiid, roster.unique$woi.id)]))
               ) %>%
                   arrange (woiid)

    bigtime$Source[bigtime$Source %in% c("War On Ice","WOI","WOIs")] <- "war-on-ice.com"
    bigtime$Way[is.na(bigtime$Way)] <- "Unknown"
    bigtime$BoughtOut[is.na(bigtime$BoughtOut)] <- ""
    bigtime$slid[is.na(bigtime$slid)] <- ""

    ## Duplicates are needed to be removed.
    ## Run a check to see if there are still any duplicates and remove them if there are
    dupes <- duplicated(bigtime %>% select(woiid, Year, BoughtOut), fromLast=TRUE) ##| duplicated(bigtime %>% select(woiid, Year, BoughtOut), fromLast=TRUE)

    ##doubles <- unique(bigtime$Contract.ID[dupes])
    ##tryit <- filter(bigtime, dupes)
    
    bigtime <- filter(bigtime, !dupes)
    
    calculate.AAV <- function (contract) {
        ## contract = filter (bigtime, Contract.ID == "xwatsoau92-1")
        ## message(dim(contract), contract$woiid[1], contract$Contract.ID[1])
        contract$aAAV <- NA
        side.age <- contract$Age; side.age[is.na(side.age)] <- 0
        side.year <- contract$Year[1]; side.year[is.na(side.year)] <- 2000
        side.ID <- contract$woiid[1]; side.ID[is.na(side.ID)] <- "bob"

        if (!is.na(contract$Source[1]) && contract$Source[1] != "USA Today") {
            
            if (side.year >= 2010 & side.ID == "charazd77") { ##!(side.ID %in% c("luongro79", "kovalil83"))) {
                contract$aAAV[side.age < 40 & contract$Year >= 2006] <- mean (contract$TotalComp[side.age < 40 & contract$Year >= 2006])
                contract$aAAV[side.age >= 40 & contract$Year >= 2006] <- contract$TotalComp[side.age >= 40 & contract$Year >= 2006]
            } else contract$aAAV[contract$Year >= 2006] <- mean (contract$TotalComp[contract$Year >= 2006])
            
            if (any (contract$slid == 1)) { ## got to be a three-year deal, and we don't have ELCs before Lockout II.
                ## deconstruct it.
                if (nrow(contract) == 4) {
                    contract$aAAV[1] <- sum(contract$NHL.Salary[-1] + contract$Signing.Bonus[-4])/3
                    contract$aAAV[-1] <- sum(contract$NHL.Salary[-1] + contract$Signing.Bonus[-1])/3
                } else {
                    contract$aAAV[1] <- sum(contract$NHL.Salary[-(1:2)] + contract$Signing.Bonus[1:3])/3
                    contract$aAAV[2] <- sum(contract$NHL.Salary[-(1:2)] + contract$Signing.Bonus[2:4])/3
                    contract$aAAV[-(1:2)] <- sum(contract$NHL.Salary[-(1:2)] + contract$Signing.Bonus[3:5])/3
                }
            }
        }
        return(contract)
    }

    ## Calculate AAV
    contracts.complete <- bigtime[order(bigtime$woiid, bigtime$Year),] %>% filter (Contract.ID != "xNA") %>%
        group_by(Name, Contract.ID) %>% do (calculate.AAV(.)) %>% ungroup %>% arrange (woiid) %>% select (-aav)
    


    ## Fold in performance bonuses. From Rand
    perfbonuses <- d1$PerfBonuses %>% mutate (Year = substr(season,5,8))  ## rename (Year = SeasonEnd)  ##
    perf.rws <- match(paste(perfbonuses$woiid, perfbonuses$Year),
                      paste(contracts.complete$woiid, contracts.complete$Year))
    contracts.complete$BonusMet[perf.rws] <- perfbonuses$Met

    contracts.complete$BonusElg[is.na(contracts.complete$BonusElg)] <- "FALSE"
    contracts.complete$BonusElg <- contracts.complete$BonusElg == "TRUE"
    
    max.rws <- which(contracts.complete$BonusMax == 0 & contracts.complete$BonusElg)
    contracts.complete$BonusMax[max.rws] <- "(non-zero)"

    ## Fold in buyouts? No need here, do it in the table maker.
    
    # Get last year of contract for all players
    last.contract <- function (df) df[which.max(df$Year),]
    contracts.last <- contracts.complete %>% filter (BoughtOut == "", Retired == "") %>% group_by (woiid) %>% do(last.contract(.)) %>% select (woiid, Year)

    save (contracts.last, contracts.complete, file="common-data/contracts-complete.RData")
    write.csv (contracts.complete, "common-data/contracts-complete.csv")
    
    # Get free agents
    FAs.2015 <- contracts.last$woiid[contracts.last$Year==2015]
    FAs.2015 <- FAs.2015[!is.na(FAs.2015)]
    save (FAs.2015, file="common-data/contracts-FA2015.RData")
    
    # which ones should be updated (From DB, not DB-One)
    return(d0$ContractHeader$woiid)

}




## Contract tables.

