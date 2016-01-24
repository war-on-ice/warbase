
##library(dplyr)

.onAttach <- function (...) {
  packageStartupMessage("warbase v 1.8.7")
}



ageAsOfSep15 <- function (season, DOB) {
    #season = 20142015; DOB = c("1981-10-01", "1981-09-27", "1981-02-04")
    ymd <- cbind(as.numeric (substr(DOB,1,4)), as.numeric (substr(DOB,6,7)), as.numeric (substr(DOB,9,10)))
    as.numeric(substr(season,1,4)) - ymd[,1] - 1*(ymd[,2] >= 10) - 1*(ymd[,3] > 15 & ymd[,2] == 9)    
}
ageAsOfJun30 <- function (season, DOB) {
    #season = 20142015; DOB = c("1981-10-01", "1981-09-27", "1981-02-04")
    ymd <- cbind(as.numeric (substr(DOB,1,4)), as.numeric (substr(DOB,6,7)), as.numeric (substr(DOB,9,10)))
    as.numeric(substr(season,1,4)) - ymd[,1] - 1*(ymd[,2] >= 7) - 1*(ymd[,3] > 30 & ymd[,2] == 6)    
}
yeardiff <- function (d1, d2) as.numeric(substr(d1,1,4)) - as.numeric(substr(d2,1,4)) - 1*(substr(d1,5,10) < substr(d2,5,10))
ageAtEnd <- function (year1, dob) yeardiff(paste0(year1, "-06-30"), dob)

anac <- function(...) as.numeric(as.character(...))


#color ramp palette, better
crp <- function (points, values) {
  values[is.na(values)] <- 0.5;
  values[values<0 | values > 1] <- 0.5;
  rgb(colorRamp(points)(values), max=255)
}

swapnames <- function (df) {

    colnames(df)[colnames(df) == "ZS.O.on"] <- "ZSO"
    colnames(df)[colnames(df) == "ZS.N.on"] <- "ZSN"
    colnames(df)[colnames(df) == "ZS.D.on"] <- "ZSD"
    colnames(df)[colnames(df) == "ZSO.off"] <- "ZSOoff"
    colnames(df)[colnames(df) == "ZSN.off"] <- "ZSNoff"
    colnames(df)[colnames(df) == "ZSD.off"] <- "ZSDoff"
    colnames(df)[colnames(df) == "ZS.Ooff"] <- "ZSOoff"
    colnames(df)[colnames(df) == "ZS.Noff"] <- "ZSNoff"
    colnames(df)[colnames(df) == "ZS.Doff"] <- "ZSDoff"
    colnames(df)[colnames(df) == "ZS.O.off"] <- "ZSOoff"
    colnames(df)[colnames(df) == "ZS.N.off"] <- "ZSNoff"
    colnames(df)[colnames(df) == "ZS.D.off"] <- "ZSDoff"
    colnames(df)[colnames(df) == "team"] <- "Team"
    colnames(df)[colnames(df) == "opponent"] <- "Opponent"
    
    df
}
fix.column.names <- function (...) swapnames(...)


swapout.oldnew <- function (originals, swaplist) {
    new <- swaplist[match(originals, swaplist[,1]),2]
    new[is.na(new)] <- originals[is.na(new)]
    new
}


augment.html.div <- function (nm, name.object=goalie.variables) {
    matcher <- name.object[match(nm,name.object[,1]),2]
    matcher[is.na(matcher)] <- nm[is.na(matcher)]
    p1 <- paste0("<div title=\"",matcher,"\">",nm,"</div>")
    p1
}

## Definition of scoring chance: 
scoring.chance <- function (pbp.data) {
    pbp.data$danger.zone <- danger.zone[pbp.data$new.loc.section + 1]
    
    (pbp.data$etype %in% c("GOAL","SHOT","MISS","BLOCK")) &
        ((pbp.data$etype %in% c("GOAL","SHOT","MISS") & pbp.data$shot.feature %in% higher.prob.events) |
         (pbp.data$etype %in% c("GOAL","SHOT","MISS") & pbp.data$danger.zone == 2) |
         pbp.data$danger.zone == 3)

}



augmentflip <- function (event.table) {

    ## Leading to trailing.
    event.table$score.diff.cat[event.table$home == 0 & event.table$score.diff.cat < 7] <-
        6 - event.table$score.diff.cat[event.table$home == 0 & event.table$score.diff.cat < 7]

    ## PP/SH, Goalie Pulled
    event.table$gamestate[event.table$gamestate %in% 2:3 & event.table$home == 0] <- 5 - event.table$gamestate[event.table$gamestate %in% 2:3 & event.table$home == 0]
    event.table$gamestate[event.table$gamestate %in% 5:6 & event.table$home == 0] <- 11 - event.table$gamestate[event.table$gamestate %in% 5:6 & event.table$home == 0]
    
    return(event.table)
}





## For players, here are the additional rates that players need.
add.rates.players <- function (df) {
    #df=out
    #print(colnames(df))
    colnames(df)[colnames(df) == "TOI.on"] <- "TOI"
    
    if (!any(colnames(df) == "Gm")) df$Gm <- 1
    df <- mutate(df, "CF%"=100*CF / (CF + CA),
                 "CF%off"=100*CFoff / (CFoff + CAoff),
                 "CF%Rel" = 100*(CF/(CF+CA) - CFoff/(CFoff+CAoff)),
                 CF60 = CF/TOI*60,  CA60 = CA/TOI*60,
                 "C+/-" = CF - CA,  CP60 = (CF + CA)/TOI*60,
                 
                 "FF%" = 100*FF / (FF + FA),
                 "FF%off"=100*FFoff / (FFoff + FAoff),
                 "FF%Rel" = 100*(FF / (FF + FA) - FFoff/(FFoff+FAoff)),
                 FF60 = FF/TOI*60,  FA60 = FA/TOI*60,
                 "F+/-" = FF - FA,  FP60 = (FF + FA)/TOI*60,
                 
                 "SF%" = 100*SF / (SF + SA),
                 "SF%off"=100*SFoff / (SFoff + SAoff),
                 "SF%Rel" = 100*(SF / (SF + SA) - SFoff/(SFoff+SAoff)),
                 SF60 = SF/TOI*60,  SA60 = SA/TOI*60,
                 "S+/-" = SF - SA,
                 
                 "GF%" = 100*GF / (GF + GA),
                 "GF%off"=100*GFoff / (GFoff + GAoff),
                 "GF%Rel" = 100*(GF / (GF + GA) - GFoff/(GFoff+GAoff)),
                 GF60 = GF/TOI*60,  GA60 = GA/TOI*60,
                 "G+/-" = GF - GA,

                 "SCF%"=100*SCF / (SCF + SCA),
                 "SCF%off"=100*SCFoff / (SCFoff + SCAoff),
                 "SCF%Rel" = 100*(SCF/(SCF+SCA) - SCFoff/(SCFoff+SCAoff)),
                 SCF60 = SCF/TOI*60,  SCA60 = SCA/TOI*60,
                 "SC+/-" = SCF - SCA,  SCP60 = (SCF + SCA)/TOI*60,


                 "HSCF%"=100*HSCF / (HSCF + HSCA),
                 "HSCF%off"=100*HSCFoff / (HSCFoff + HSCAoff),
                 "HSCF%Rel" = 100*(HSCF/(HSCF+HSCA) - HSCFoff/(HSCFoff+HSCAoff)),
                 HSCF60 = HSCF/TOI*60,  HSCA60 = HSCA/TOI*60,
                 "HSC+/-" = HSCF - HSCA,  HSCP60 = (HSCF + HSCA)/TOI*60,


                 G60 = G/TOI*60,    A60 = A/TOI*60,
                 A160 = A1/TOI*60,  A260 = A2/TOI*60,
                 P60 = (G+A)/TOI*60,
                 P160 = (G+A1)/TOI*60,
                 
                 "ZSO%" = 100*ZSO/(ZSO+ZSD),
                 "ZSO%Rel" = 100*(ZSO/(ZSO+ZSD) - ZSOoff/(ZSOoff+ZSDoff)),
                 
                 PDO = 100*(GF/SF - GA/SA + 1),
                 PenD60=PenD/TOI*60,
                     
                 "FO%" = 100*FO_W/(FO_W + FO_L),
                 "FO%^" = 100*(FO_W+max(0, 8-(FO_W+FO_L)*0.4))/
                 (FO_W + FO_L + max(0, 20-(FO_W+FO_L))),
                 "TOI/Gm" = TOI/Gm,
                 "TOI%" = 100*TOI/(TOI+TOIoff),
                 

                 "OFOn%" = 100*SF/FF,
                 "OCOn%" = 100*SF/CF,
                 "OSh%" = 100*GF/SF,
                 "OFenSh%" = 100*GF/FF,
                 
                 "OFAOn%" = 100*(1 - SA/FA),
                 "OCAOn%" = 100*(1 - SA/CA),
                 "OSv%" = 100*(1 - GA/SA),
                 "OFenSv%" = 100*(1- GA/FA),

                 
                 "PFenSh%" = 100*G/(G+SH+MS),
                 "PSh%" = 100*G/(G+SH),
                 
                 iSF = G+SH,
                 iFF = G+SH+MS,
                 iCF = G+SH+MS+BK)

    df[["FO%"]][is.na(df[["FO%"]])] <- 40
    return(df)
}


teamshrink <- function (tmvec)
    paste({
        d1 <- unique(unlist(strsplit(unique(as.character(tmvec)), "/")))
        d1 <- d1[d1 != ""]
        d1}, collapse="/")

## this version is for dCorsi in particular. ACT, 2014-12-23
compress.player.game.0 <- function(df)  ## that is, from the individual game table.
    summarize(df,
              SH=sum(SHOT),    MS=sum(MISS),    G=sum(GOAL),
              HIT=sum(HIT),   "HIT-"=sum(HIT_TAKEN),   
              TK=sum(TAKE),             GV=sum(GIVE),
              PN=sum(PENL_TAKEN),      "PN-"=sum(PENL_DRAWN),
              PenD=sum(PENL_DRAWN) - sum(PENL_TAKEN),

              BK=sum(BLOCKED_SHOT),       AB=sum(BLOCK),
              A=sum(ASSIST+ASSIST_2), A1=sum(ASSIST),     A2=sum(ASSIST_2),#  A1=A-A2,
              iSC=sum(iSC),           iHSC=sum(isSC),
              
              FO_W=sum(FAC_WIN),        FO_L=sum(FAC_LOSE),

              
              CF=sum(CF), CA=sum(CA), CFoff=sum(CFoff), CAoff=sum(CAoff),
              FF=sum(FF), FA=sum(FA), FFoff=sum(FFoff), FAoff=sum(FAoff),
              SF=sum(SF), SA=sum(SA), SFoff=sum(SFoff), SAoff=sum(SAoff),
              GF=sum(GF), GA=sum(GA), GFoff=sum(GFoff), GAoff=sum(GAoff),

              
              SCF=sum(FF2+CF3+CF4),   SCA=sum(FA2+CA3+CA4),
              SCFoff=sum(FF2off+CF3off+CF4off),  SCAoff=sum(FA2off+CA3off+CA4off),
              HSCF=sum(FF3+CF4),      HSCA=sum(FA3+CA4),
              HSCFoff=sum(FF3off+CF4off),        HSCAoff=sum(FA3off+CA4off),

              
              ZSO=sum(ZSO),       ZSN=sum(ZSN),       ZSD=sum(ZSD),
              ZSOoff=sum(ZSOoff), ZSNoff=sum(ZSNoff), ZSDoff=sum(ZSDoff),

              TOI.on=sum(TOI, na.rm=TRUE)/60, TOIoff=sum(TOIoff, na.rm=TRUE)/60,

              cTOI60=sum(cTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              tTOI60=sum(tTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              
              cCF60=sum(cCF60*TOI/60, na.rm=TRUE)/TOI.on,
              tCF60=sum(tCF60*TOI/60, na.rm=TRUE)/TOI.on,

              cCA60=sum(cCA60*TOI/60, na.rm=TRUE)/TOI.on,
              tCA60=sum(tCA60*TOI/60, na.rm=TRUE)/TOI.on,

              "TOIC%"=cTOI60,  
              "TOIT%"=tTOI60,  

              "CorC%"=cCF60/(cCF60 + cCA60)*100,   
              "CorT%"=tCF60/(tCF60 + tCA60)*100,   
              
              Team=teamshrink(Team),
              
              Gm=length(unique(paste(season, gcode)))
              )


compress.player.game <- function(df)  ## that is, from the individual game table.
    summarize(df,
              SH=sum(SHOT),    MS=sum(MISS),    G=sum(GOAL),
              HIT=sum(HIT),   "HIT-"=sum(HIT_TAKEN),   
              TK=sum(TAKE),             GV=sum(GIVE),
              PN=sum(PENL_TAKEN),      "PN-"=sum(PENL_DRAWN),
              PenD=sum(PENL_DRAWN) - sum(PENL_TAKEN),
              
              BK=sum(BLOCKED_SHOT),       AB=sum(BLOCK),
              A=sum(ASSIST+ASSIST_2), A1=sum(ASSIST),     A2=sum(ASSIST_2),#  A1=A-A2,
              iSC=sum(iSC),           iHSC=sum(isSC),
              
              FO_W=sum(FAC_WIN),        FO_L=sum(FAC_LOSE),
              
              CF=sum(CF), CA=sum(CA), CFoff=sum(CFoff), CAoff=sum(CAoff),
              FF=sum(FF), FA=sum(FA), FFoff=sum(FFoff), FAoff=sum(FAoff),
              SF=sum(SF), SA=sum(SA), SFoff=sum(SFoff), SAoff=sum(SAoff),
              GF=sum(GF), GA=sum(GA), GFoff=sum(GFoff), GAoff=sum(GAoff),


              SCF=sum(FF2+CF3+CF4),   SCA=sum(FA2+CA3+CA4),
              SCFoff=sum(FF2off+CF3off+CF4off),  SCAoff=sum(FA2off+CA3off+CA4off),
              HSCF=sum(FF3+CF4),      HSCA=sum(FA3+CA4),
              HSCFoff=sum(FF3off+CF4off),        HSCAoff=sum(FA3off+CA4off),
             
              
              ZSO=sum(ZSO),   ZSN=sum(ZSN), ZSD=sum(ZSD),
              ZSOoff=sum(ZSOoff),ZSNoff=sum(ZSNoff),ZSDoff=sum(ZSDoff),

              TOI.on=sum(TOI, na.rm=TRUE)/60, TOIoff=sum(TOIoff, na.rm=TRUE)/60,
              
              cTOI60=sum(cTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              tTOI60=sum(tTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              
              cCF60=sum(cCF60*TOI/60, na.rm=TRUE)/TOI.on,
              cCA60=sum(cCA60*TOI/60, na.rm=TRUE)/TOI.on,

              tCF60=sum(tCF60*TOI/60, na.rm=TRUE)/TOI.on,
              tCA60=sum(tCA60*TOI/60, na.rm=TRUE)/TOI.on,

              "TOIC%"=cTOI60,  ##sum(TOIPctC*TOI/60, na.rm=TRUE)/TOI.on*100,
              "TOIT%"=tTOI60,  ##sum(TOIPctT*TOI/60, na.rm=TRUE)/TOI.on*100,
              "CorC%"=cCF60/(cCF60 + cCA60)*100,    ##sum(CorsiC*TOI/60, na.rm=TRUE)/TOI.on*100,
              "CorT%"=tCF60/(tCF60 + tCA60)*100,    ##sum(CorsiT*TOI/60, na.rm=TRUE)/TOI.on*100,
              
              Team=teamshrink(Team),

              
              Gm=length(unique(paste(season, gcode))),
              home=home[1]
              ##,Games=length(unique(paste(season, gcode)))
              )


compress.player.game.precomp <- function(df)  ## that is, from the individual game table.
    summarize(df,
              SH=sum(SHOT),    MS=sum(MISS),    G=sum(GOAL),
              HIT=sum(HIT),   "HIT-"=sum(HIT_TAKEN),   
              TK=sum(TAKE),             GV=sum(GIVE),
              PN=sum(PENL_TAKEN),      "PN-"=sum(PENL_DRAWN),
              PenD=sum(PENL_DRAWN) - sum(PENL_TAKEN),
              
              BK=sum(BLOCKED_SHOT),       AB=sum(BLOCK),
              A=sum(ASSIST+ASSIST_2), A1=sum(ASSIST),     A2=sum(ASSIST_2),#  A1=A-A2,
              iSC=sum(iSC),           iHSC=sum(isSC),
              
              FO_W=sum(FAC_WIN),        FO_L=sum(FAC_LOSE),
              
              CF=sum(CF), CA=sum(CA), CFoff=sum(CFoff), CAoff=sum(CAoff),
              FF=sum(FF), FA=sum(FA), FFoff=sum(FFoff), FAoff=sum(FAoff),
              SF=sum(SF), SA=sum(SA), SFoff=sum(SFoff), SAoff=sum(SAoff),
              GF=sum(GF), GA=sum(GA), GFoff=sum(GFoff), GAoff=sum(GAoff),

              SCF=sum(FF2+CF3+CF4),   SCA=sum(FA2+CA3+CA4),
              SCFoff=sum(FF2off+CF3off+CF4off),  SCAoff=sum(FA2off+CA3off+CA4off),
              HSCF=sum(FF3+CF4),      HSCA=sum(FA3+CA4),
              HSCFoff=sum(FF3off+CF4off),        HSCAoff=sum(FA3off+CA4off),
              
              ZSO=sum(ZSO),   ZSN=sum(ZSN), ZSD=sum(ZSD),
              ZSOoff=sum(ZSOoff),ZSNoff=sum(ZSNoff),ZSDoff=sum(ZSDoff),

              TOI.on=sum(TOI, na.rm=TRUE)/60, TOIoff=sum(TOIoff, na.rm=TRUE)/60,
              
              cTOI60=sum(cTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              tTOI60=sum(tTOI60*TOI/60, na.rm=TRUE)/TOI.on,
              
              cCF60=sum(cCF60*TOI/60, na.rm=TRUE)/TOI.on,
              cCA60=sum(cCA60*TOI/60, na.rm=TRUE)/TOI.on,

              tCF60=sum(tCF60*TOI/60, na.rm=TRUE)/TOI.on,
              tCA60=sum(tCA60*TOI/60, na.rm=TRUE)/TOI.on,

              "TOIC%"=cTOI60,  ##sum(TOIPctC*TOI/60, na.rm=TRUE)/TOI.on*100,
              "TOIT%"=tTOI60,  ##sum(TOIPctT*TOI/60, na.rm=TRUE)/TOI.on*100,
              "CorC%"=cCF60/(cCF60 + cCA60)*100,    ##sum(CorsiC*TOI/60, na.rm=TRUE)/TOI.on*100,
              "CorT%"=tCF60/(tCF60 + tCA60)*100,    ##sum(CorsiT*TOI/60, na.rm=TRUE)/TOI.on*100,
              
              Team=teamshrink(Team),
              
              Gm=sum(Gm),
              home=home[1]
              )



####################################################################
##
## Team choices, functions.

add.rates.teams <- function (df) {
    mutate(df,
           "CF%"=100*CF / (CF + CA),
           CF60 = CF/TOI*60,   CA60 = CA/TOI*60,
           "C+/-" = CF - CA,  "CP60" = (CF + CA)/TOI*60,
           "FF%" = 100*FF / (FF + FA),
           FF60 = FF/TOI*60,   FA60 = FA/TOI*60,
           "F+/-" = FF - FA,  "FP60" = (FF + FA)/TOI*60,
           "SF%" = 100*SF / (SF + SA),
           SF60 = SF/TOI*60, SA60 = SA/TOI*60, "S+/-" = SF - SA,
           "GF%" = 100*GF / (GF + GA),
           GF60 = GF/TOI*60, GA60 = GA/TOI*60, "G+/-" = GF - GA,

           "SCF%"=100*SCF / (SCF + SCA),
           SCF60 = SCF/TOI*60,  SCA60 = SCA/TOI*60,
           "SC+/-" = SCF - SCA,  SCP60 = (SCF + SCA)/TOI*60,

           "HSCF%"=100*HSCF / (HSCF + HSCA),
           HSCF60 = HSCF/TOI*60,  HSCA60 = HSCA/TOI*60,
           "HSC+/-" = HSCF - HSCA,  HSCP60 = (HSCF + HSCA)/TOI*60,

           "ZSO%" = 100*ZSO/(ZSO+ZSD),
           PDO = 100*(GF/SF - GA/SA + 1),
           "FO%" = 100*FO_W/(FO_W + FO_L),
           PenD60=PenD/TOI*60*60,
                     
           "OFOn%" = 100*SF/FF,
           "OCOn%" = 100*SF/CF,
           "OSh%" = 100*GF/SF,
           "OFenSh%" = 100*GF/FF,
           
           "OFAOn%" = 100*(1 - SA/FA),
           "OCAOn%" = 100*(1 - SA/CA),
           "OSv%" = 100*(1 - GA/SA),
           "OFenSv%" = 100*(1- GF/FF),

           
           MSF = FF-SF, MSA = FA-SA,
           BSF = CF-FF, BSA = CA-FA
           )  #), 2)    return(cbind(df, rates))
}

compress.team <- function(df) 
    dplyr::summarize(df,
                     "PN"=sum(PENL_TAKEN),
                     "PN-"=sum(PENL_DRAWN),
                     PenD=sum(PENL_DRAWN) - sum(PENL_TAKEN),
                     
                     "HIT"=sum(HIT),
                     "HIT-"=sum(HIT_TAKEN),
                     FO_W=sum(FAC_WIN),
                     FO_L=sum(FAC_LOSE),
                     CF=sum(CF), CA=sum(CA), 
                     FF=sum(FF), FA=sum(FA), 
                     SF=sum(SF), SA=sum(SA), 
                     GF=sum(GF), GA=sum(GA), 

                     SCF=sum(FF2+CF3+CF4),   SCA=sum(FA2+CA3+CA4),   
                     HSCF=sum(FF3+CF4),      HSCA=sum(FA3+CA4),      
             
                     ZSO=sum(ZSO),   ZSN=sum(ZSN), ZSD=sum(ZSD),
                     TOI=sum(TOI)/60, 
                     Gm=length(unique(paste(season, gcode))))




####################################################################
##
## Goalie choices, functions

goalie.compressor <- function(df)
    dplyr::summarize(df,
                     GU=sum(goals.0),
                     SU=sum(shots.0),
                     G.L=sum(goals.1),
                     S.L=sum(shots.1),
                     G.M=sum(goals.2),
                     S.M=sum(shots.2),
                     G.H=sum(goals.3 + goals.4),
                     S.H=sum(shots.3 + shots.4),
                     TOI=round(sum(TOI)/60, 1),
                     Gm=length(unique(paste0(season, gcode))),
                     Team={tms=unique(as.character(Team)); paste(tms[nchar(tms)>0],collapse="/")}
                     )
    
compress.goalietable <- function (dftable.sub) #, gamestest2=gamestest, startdate="2013-10-01", enddate="2014-09-30")
{dftable.sub %>% group_by(ID, Name) %>% goalie.compressor}

compress.goalietable.season <- function (dftable.sub) #, gamestest2=gamestest, startdate="2013-10-01", enddate="2014-09-30")
{dftable.sub %>% group_by(ID, Name, season) %>% goalie.compressor}

compress.goalietable.game <- function (dftable.sub) #, gamestest2=gamestest, startdate="2013-10-01", enddate="2014-09-30")
{dftable.sub %>% group_by(ID, Name, season, gcode) %>% goalie.compressor}

goalie.compressor.game <- function(df)
    dplyr::summarize(df,
              Team=Team[1],
              Opp=Opponent[1],
              Date=thisdate[1],
              G.U=sum(goals.0),
              S.U=sum(shots.0),
              G.L=sum(goals.1),
              S.L=sum(shots.1),
              G.M=sum(goals.2),
              S.M=sum(shots.2),
              G.H=sum(goals.3 + goals.4),
              S.H=sum(shots.3 + shots.4),
              TOI=round(sum(TOI)/60,1))

compress.goalietable.date <- function (dftable.sub) #, gamestest2=gamestest, startdate="2013-10-01", enddate="2014-09-30")
{dftable.sub %>% group_by(ID, Name, season, gcode) %>% goalie.compressor.game}








## c("AdjustedSvPct","UnadjSvPct", "SvPctLow","SvPctMed","SvPctHigh", "Goals.Low","Saves.Low", "Goals.Med","Saves.Med", "Goals.High","Saves.High","TOI","Games","ShotsPer60")
