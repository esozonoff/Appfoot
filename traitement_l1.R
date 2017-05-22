#extraction
database=read.csv2('1.source/l1database.csv')
head(database)

#résumé des variables et de leur type
for (i in 1:ncol(database)){
  print(colnames(database)[i])
  print(class(database[1,i]))
}
# Date->factor;Heure->factor

matchs = database
attach(matchs)

#création d'une variable date longue
newDate=strptime(Date, format="%d/%m/%Y", tz = "EST5EDT");class(newDate)
newHeure=strptime(Heure, format = "%H:%M");class(newHeure)
newDateL=with(matchs, paste(newDate, Heure, sep = " "))
matchs$DateL=strptime(newDateL, format = "%Y-%m-%d %H:%M", tz = "EST5EDT");class(matchs$DateL)
matchs$Ecart_Match=abs(matchs$Score_Dom-matchs$Score_Ext)

#creation variable cote gagnante
for (i in 1:nrow(matchs)){
  if(matchs$Score_Match[i]==1){
    matchs$CoteG[i] = P1[i]
    matchs$CoteG_Type[i] = "P1"
  }
  else if(matchs$Score_Match[i]==0){
    matchs$CoteG[i] = PN[i]
    matchs$CoteG_Type[i] = "PN"
  }
  else{
    matchs$CoteG[i] = P2[i]
    matchs$CoteG_Type[i] = "P2"
  }
  matchs$id_match[i] = i
}

#creation Dom et Ext (enlever les espaces superflus)
#t=gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",teams, perl=T)
#teams=t[duplicated(t)==F];length(teams)
#trim_string = function(string) gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",string, perl=T) 

#erreur ligne 3069
#matchs$Saison[3069]=2013

#row.names.data.frame(matchs)

#creation des bases de chaque équipe
#test=matchs[which(matchs$Dom=='St Etienne' | matchs$Ext=='St Etienne'),]
teams=levels(matchs$Domicile);length(teams)
for(i in 1:length(teams)){
  team = teams[i];print(team)
  frame = paste0("matchs_t",i);print(frame)
  #eval(parse(text=paste0("matchs_t",i,"=matchs[which(matchs$Dom=='",team,"' | matchs$Ext=='",team,"'),]")))
  eval(parse(text=paste0(frame,"=matchs[which(matchs$Dom=='",team,"' | matchs$Ext=='",team,"'),]")))
  eval(parse(text=paste0("saisons=",frame,"$Saison")))
  eval(parse(text=paste0("saisons=saisons[duplicated(saisons)==F]")));print(saisons)
  for(j in 1:length(saisons)){
    frame2 = paste0(frame,"_",saisons[j]);print(frame2)
    eval(parse(text=paste0(frame2,"=",frame,"[which(",frame,"$Saison==",saisons[j],"),]")))
  } 
}

#creation  des variables de base sur tout l'historique pour chaque equipe 
#var_base=c("nb_match","nb_match_Dom","nb_match_Ext"
#,"nb_victoire","nb_victoire_Dom","nb_victoire_Ext"
#,"nb_defaite","nb_defaite_Dom","nb_defaite_Ext"
#,"nb_nul","nb_nul_Dom","nb_nul_Ext"
#,"nb_point","nb_point_Dom","nb_point_Ext"
#,"sum_butm","sum_butm_Dom","sum_butm_Ext"
#,"sum_bute","sum_bute_Dom","sum_bute_Ext"
#,"sum_but","sum_but_Dom","sum_but_Ext")
for (i in 1:length(teams)){
  nteam=i;print(nteam)
  #équipe et frame
  equipe = teams[nteam];print(equipe)
  frame = paste0("matchs_t",nteam)
  #id_match
  eval(parse(text=paste0(frame,"$id_match=row.names.data.frame(",frame,")")))
  #"nb_match","nb_match_Dom","nb_match_Ext"
  eval(parse(text=paste0(frame,"$nb_match_H=row(",frame,")[,1]")))
  eval(parse(text=paste0(frame,"$nb_match_Dom_H=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"'))")))
  eval(parse(text=paste0(frame,"$nb_match_Ext_H=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"'))")))
  #"nb_victoire","nb_victoire_Dom","nb_victoire_Ext"
  eval(parse(text=paste0(frame,"$nb_victoire_H=cumsum(as.numeric((",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==1) | (",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==2)))")))
  eval(parse(text=paste0(frame,"$nb_victoire_Dom_H=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==1))")))
  eval(parse(text=paste0(frame,"$nb_victoire_Ext_H=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==2))")))
  #"nb_defaite","nb_defaite_Dom","nb_defaite_Ext"
  eval(parse(text=paste0(frame,"$nb_defaite_H=cumsum(as.numeric((",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==2) | (",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==1)))")))
  eval(parse(text=paste0(frame,"$nb_defaite_Dom_H=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==2))")))
  eval(parse(text=paste0(frame,"$nb_defaite_Ext_H=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==1))")))
  #"nb_nul","nb_nul_Dom","nb_nul_Ext"
  eval(parse(text=paste0(frame,"$nb_nul_H=cumsum(as.numeric(",frame,"$Score_Match==0))")))
  eval(parse(text=paste0(frame,"$nb_nul_Dom_H=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==0))")))
  eval(parse(text=paste0(frame,"$nb_nul_Ext_H=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==0))")))
  #"nb_point","nb_point_Dom","nb_point_Ext"
  eval(parse(text=paste0(frame,"$nb_point_H=",frame,"$nb_victoire_H * 3 + ",frame,"$nb_nul_H")))
  eval(parse(text=paste0(frame,"$nb_point_Dom_H=",frame,"$nb_victoire_Dom_H * 3 + ",frame,"$nb_nul_Dom_H")))
  eval(parse(text=paste0(frame,"$nb_point_Ext_H=",frame,"$nb_victoire_Ext_H * 3 + ",frame,"$nb_nul_Ext_H")))
  #"sum_butm","sum_butm_Dom","sum_butm_Ext"
  eval(parse(text=paste0(frame,"$sum_butm_Dom_H=cumsum(",frame,"$Score_Dom*as.numeric(",frame,"$Domicile=='",equipe,"'))")))
  eval(parse(text=paste0(frame,"$sum_butm_Ext_H=cumsum(",frame,"$Score_Ext*as.numeric(",frame,"$Domicile!='",equipe,"'))")))
  eval(parse(text=paste0(frame,"$sum_butm_H=",frame,"$sum_butm_Dom_H + ",frame,"$sum_butm_Ext_H")))
  #"sum_bute","sum_bute_Dom","sum_bute_Ext"
  eval(parse(text=paste0(frame,"$sum_bute_Dom_H=cumsum(",frame,"$Score_Ext*as.numeric(",frame,"$Domicile=='",equipe,"'))")))
  eval(parse(text=paste0(frame,"$sum_bute_Ext_H=cumsum(",frame,"$Score_Dom*as.numeric(",frame,"$Domicile!='",equipe,"'))")))
  eval(parse(text=paste0(frame,"$sum_bute_H=",frame,"$sum_bute_Dom_H + ",frame,"$sum_bute_Ext_H")))
  #"sum_but","sum_but_Dom","sum_but_Ext"
  eval(parse(text=paste0(frame,"$sum_but_H=",frame,"$sum_butm_H + ",frame,"$sum_bute_H")))
  eval(parse(text=paste0(frame,"$sum_but_Dom_H=",frame,"$sum_butm_Dom_H + ",frame,"$sum_bute_Dom_H")))
  eval(parse(text=paste0(frame,"$sum_but_Ext_H=",frame,"$sum_butm_Ext_H + ",frame,"$sum_bute_Ext_H")))
}

#creation  des variables de base à chaque saison pour chaque equipe 
for (i in 1:length(teams)){
  nteam=i
  #équipe et frame
  equipe = teams[nteam];print(equipe)
  frame1 = paste0("matchs_t",nteam);print(frame1)
  #saisons
  eval(parse(text=paste0("saisons=",frame1,"$Saison")))
  eval(parse(text=paste0("saisons=saisons[duplicated(saisons)==F]")))
  
  for(j in 1:length(saisons)){
    frame = paste0(frame1,"_",saisons[j]);print(frame)
    #eval(parse(text=paste0(frame,"=",frame1,"[which(",frame1,"$Saison==",saisons[j],"),1:20]")))
    #id_match
    eval(parse(text=paste0(frame,"$id_match=row.names.data.frame(",frame,")")))
    #"nb_match","nb_match_Dom","nb_match_Ext"
    eval(parse(text=paste0(frame,"$nb_match=row(",frame,")[,1]")))
    eval(parse(text=paste0(frame,"$nb_match_Dom=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"'))")))
    eval(parse(text=paste0(frame,"$nb_match_Ext=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"'))")))
    #"nb_victoire","nb_victoire_Dom","nb_victoire_Ext"
    eval(parse(text=paste0(frame,"$nb_victoire=cumsum(as.numeric((",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==1) | (",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==2)))")))
    eval(parse(text=paste0(frame,"$nb_victoire_Dom=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==1))")))
    eval(parse(text=paste0(frame,"$nb_victoire_Ext=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==2))")))
    #"nb_defaite","nb_defaite_Dom","nb_defaite_Ext"
    eval(parse(text=paste0(frame,"$nb_defaite=cumsum(as.numeric((",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==2) | (",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==1)))")))
    eval(parse(text=paste0(frame,"$nb_defaite_Dom=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==2))")))
    eval(parse(text=paste0(frame,"$nb_defaite_Ext=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==1))")))
    #"nb_nul","nb_nul_Dom","nb_nul_Ext"
    eval(parse(text=paste0(frame,"$nb_nul=cumsum(as.numeric(",frame,"$Score_Match==0))")))
    eval(parse(text=paste0(frame,"$nb_nul_Dom=cumsum(as.numeric(",frame,"$Domicile=='",equipe,"' & ",frame,"$Score_Match==0))")))
    eval(parse(text=paste0(frame,"$nb_nul_Ext=cumsum(as.numeric(",frame,"$Domicile!='",equipe,"' & ",frame,"$Score_Match==0))")))
    #"nb_point","nb_point_Dom","nb_point_Ext"
    eval(parse(text=paste0(frame,"$nb_point=",frame,"$nb_victoire * 3 + ",frame,"$nb_nul")))
    eval(parse(text=paste0(frame,"$nb_point_Dom=",frame,"$nb_victoire_Dom * 3 + ",frame,"$nb_nul_Dom")))
    eval(parse(text=paste0(frame,"$nb_point_Ext=",frame,"$nb_victoire_Ext * 3 + ",frame,"$nb_nul_Ext")))
    #"sum_butm","sum_butm_Dom","sum_butm_Ext"
    eval(parse(text=paste0(frame,"$sum_butm_Dom=cumsum(",frame,"$Score_Dom*as.numeric(",frame,"$Domicile=='",equipe,"'))")))
    eval(parse(text=paste0(frame,"$sum_butm_Ext=cumsum(",frame,"$Score_Ext*as.numeric(",frame,"$Domicile!='",equipe,"'))")))
    eval(parse(text=paste0(frame,"$sum_butm=",frame,"$sum_butm_Dom + ",frame,"$sum_butm_Ext")))
    #"sum_bute","sum_bute_Dom","sum_bute_Ext"
    eval(parse(text=paste0(frame,"$sum_bute_Dom=cumsum(",frame,"$Score_Ext*as.numeric(",frame,"$Domicile=='",equipe,"'))")))
    eval(parse(text=paste0(frame,"$sum_bute_Ext=cumsum(",frame,"$Score_Dom*as.numeric(",frame,"$Domicile!='",equipe,"'))")))
    eval(parse(text=paste0(frame,"$sum_bute=",frame,"$sum_bute_Dom + ",frame,"$sum_bute_Ext")))
    #"sum_but","sum_but_Dom","sum_but_Ext"
    eval(parse(text=paste0(frame,"$sum_but=",frame,"$sum_butm + ",frame,"$sum_bute")))
    eval(parse(text=paste0(frame,"$sum_but_Dom=",frame,"$sum_butm_Dom + ",frame,"$sum_bute_Dom")))
    eval(parse(text=paste0(frame,"$sum_but_Ext=",frame,"$sum_butm_Ext + ",frame,"$sum_bute_Ext")))
  }
  
}

#creation  des variables agrégées sur tout l'historique pour chaque equipe 
#var_agreg=c(mean_ecart
#,mean_butm,mean_butm_Dom,mean_butm_Ext
#,mean_bute,mean_bute_Dom,mean_bute_Ext
#,mean_butt,mean_butt_Dom,mean_butt_Ext
#,mean_point,mean_point_Dom,mean_point_Ext)
for (i in 1:length(teams)){
  nteam=i
  #équipe et frame
  equipe = teams[nteam];print(equipe)
  frame = paste0("matchs_t",nteam);print(frame)
  #id_match
  #eval(parse(text=paste0(frame,"$id_match=row.names.data.frame(",frame,")")))
  #mean_ecart,mean_ecart_Dom,mean_ecart_Ext
  eval(parse(text=paste0(frame,"$mean_ecart_H=cumsum(",frame,"$Ecart_Match)/",frame,"$nb_match_H")))
  #mean_butm,mean_butm_Dom,mean_butm_Ext
  eval(parse(text=paste0(frame,"$mean_butm_Dom_H=",frame,"$sum_butm_Dom_H/",frame,"$nb_match_Dom_H")))
  eval(parse(text=paste0(frame,"$mean_butm_Ext_H=",frame,"$sum_butm_Ext_H/",frame,"$nb_match_Ext_H")))
  eval(parse(text=paste0(frame,"$mean_butm_H=",frame,"$sum_butm_H/",frame,"$nb_match_H")))
  #mean_bute,mean_bute_Dom,mean_bute_Ext
  eval(parse(text=paste0(frame,"$mean_bute_Dom_H=",frame,"$sum_bute_Dom_H/",frame,"$nb_match_Dom_H")))
  eval(parse(text=paste0(frame,"$mean_bute_Ext_H=",frame,"$sum_bute_Ext_H/",frame,"$nb_match_Ext_H")))
  eval(parse(text=paste0(frame,"$mean_bute_H=",frame,"$sum_bute_H/",frame,"$nb_match_H")))
  #mean_but,mean_but_Dom,mean_but_Ext
  eval(parse(text=paste0(frame,"$mean_but_Dom_H=",frame,"$sum_but_Dom_H/",frame,"$nb_match_Dom_H")))
  eval(parse(text=paste0(frame,"$mean_but_Ext_H=",frame,"$sum_but_Ext_H/",frame,"$nb_match_Ext_H")))
  eval(parse(text=paste0(frame,"$mean_but_H=",frame,"$sum_but_H/",frame,"$nb_match_H")))
  #mean_point,mean_point_Dom,mean_point_Ext
  eval(parse(text=paste0(frame,"$mean_point_Dom_H=",frame,"$nb_point_Dom_H/",frame,"$nb_match_Dom_H")))
  eval(parse(text=paste0(frame,"$mean_point_Ext_H=",frame,"$nb_point_Ext_H/",frame,"$nb_match_Ext_H")))
  eval(parse(text=paste0(frame,"$mean_point_H=",frame,"$nb_point_H/",frame,"$nb_match_H")))
}

#creation des variables agrégées à chaque saison pour chaque equipe 
for (i in 1:length(teams)){
  nteam=i
  #équipe et frame
  equipe = teams[nteam];print(equipe)
  frame1 = paste0("matchs_t",nteam);print(frame1)
  #saisons
  eval(parse(text=paste0("saisons=",frame1,"$Saison")))
  eval(parse(text=paste0("saisons=saisons[duplicated(saisons)==F]")))
  for(j in 1:length(saisons)){
    frame = paste0(frame1,"_",saisons[j]);print(frame)
    #eval(parse(text=paste0(frame,"=",frame1,"[which(",frame1,"$Saison==",saisons[j],"),]")))
    #id_match
    #eval(parse(text=paste0(frame,"$id_match=row.names.data.frame(",frame,")")))
    #mean_ecart,mean_ecart_Dom,mean_ecart_Ext
    eval(parse(text=paste0(frame,"$mean_ecart=cumsum(",frame,"$Ecart_Match)/",frame,"$nb_match")))
    #mean_butm,mean_butm_Dom,mean_butm_Ext
    eval(parse(text=paste0(frame,"$mean_butm_Dom=",frame,"$sum_butm_Dom/",frame,"$nb_match_Dom")))
    eval(parse(text=paste0(frame,"$mean_butm_Ext=",frame,"$sum_butm_Ext/",frame,"$nb_match_Ext")))
    eval(parse(text=paste0(frame,"$mean_butm=",frame,"$sum_butm/",frame,"$nb_match")))
    #mean_bute,mean_bute_Dom,mean_bute_Ext
    eval(parse(text=paste0(frame,"$mean_bute_Dom=",frame,"$sum_bute_Dom/",frame,"$nb_match_Dom")))
    eval(parse(text=paste0(frame,"$mean_bute_Ext=",frame,"$sum_bute_Ext/",frame,"$nb_match_Ext")))
    eval(parse(text=paste0(frame,"$mean_bute=",frame,"$sum_bute/",frame,"$nb_match")))
    #mean_but,mean_but_Dom,mean_but_Ext
    eval(parse(text=paste0(frame,"$mean_but_Dom=",frame,"$sum_but_Dom/",frame,"$nb_match_Dom")))
    eval(parse(text=paste0(frame,"$mean_but_Ext=",frame,"$sum_but_Ext/",frame,"$nb_match_Ext")))
    eval(parse(text=paste0(frame,"$mean_but=",frame,"$sum_but/",frame,"$nb_match")))
    #mean_point,mean_point_Dom,mean_point_Ext
    eval(parse(text=paste0(frame,"$mean_point_Dom=",frame,"$nb_point_Dom/",frame,"$nb_match_Dom")))
    eval(parse(text=paste0(frame,"$mean_point_Ext=",frame,"$nb_point_Ext/",frame,"$nb_match_Ext")))
    eval(parse(text=paste0(frame,"$mean_point=",frame,"$nb_point/",frame,"$nb_match")))
  }
}

#creation  des variables de base et agrégées sur les k derniers matchs à chaque saison pour chaque equipe
for(k in 2:10){  
  print(k)
  for (i in 1:length(teams)){
    nteam=i
    #équipe et frame
    equipe = teams[nteam];print(equipe)
    frame1 = paste0("matchs_t",nteam);print(frame1)
    #saisons
    eval(parse(text=paste0("saisons=",frame1,"$Saison")))
    eval(parse(text=paste0("saisons=saisons[duplicated(saisons)==F]")))
    
    for(j in 1:length(saisons)){
      frame = paste0(frame1,"_",saisons[j]);print(frame)
      eval(parse(text=paste0("N=nrow(",frame,")")))
      
      #initialisation
      eval(parse(text=paste0(frame,"$nb_match_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_match_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_match_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_victoire_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_victoire_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_victoire_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_defaite_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_defaite_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_defaite_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_nul_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_nul_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_nul_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_point_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_point_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$nb_point_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_but_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_but_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_but_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_butm_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_butm_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_butm_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_bute_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_bute_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$sum_bute_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_but_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_but_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_but_Ext_",k,"=NaN")))      
      eval(parse(text=paste0(frame,"$mean_butm_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_butm_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_butm_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_bute_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_bute_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_bute_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_point_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_point_Dom_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_point_Ext_",k,"=NaN")))
      eval(parse(text=paste0(frame,"$mean_ecart_",k,"=NaN")))
      
      if(k<=N){
        for(m in k:N){
          #"nb_match_Dom","nb_match_Ext"
          eval(parse(text=paste0(frame,"$nb_match_Dom_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$nb_match_Ext_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$nb_match_",k,"=",k)))
          #"nb_victoire","nb_victoire_Dom","nb_victoire_Ext"
          eval(parse(text=paste0(frame,"$nb_victoire_",k,"[",m,"]=sum(as.numeric((",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==1) | (",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==2)))")))
          eval(parse(text=paste0(frame,"$nb_victoire_Dom_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==1))")))
          eval(parse(text=paste0(frame,"$nb_victoire_Ext_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==2))")))
          #"nb_defaite","nb_defaite_Dom","nb_defaite_Ext"
          eval(parse(text=paste0(frame,"$nb_defaite_",k,"[",m,"]=sum(as.numeric((",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==2) | (",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==1)))")))
          eval(parse(text=paste0(frame,"$nb_defaite_Dom_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==2))")))
          eval(parse(text=paste0(frame,"$nb_defaite_Ext_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==1))")))
          #"nb_nul","nb_nul_Dom","nb_nul_Ext"
          eval(parse(text=paste0(frame,"$nb_nul_",k,"[",m,"]=sum(as.numeric(",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==0))")))
          eval(parse(text=paste0(frame,"$nb_nul_Dom_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==0))")))
          eval(parse(text=paste0(frame,"$nb_nul_Ext_",k,"[",m,"]=sum(as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"' & ",frame,"$Score_Match[(",m,"-",k,"+1):",m,"]==0))")))
          #"nb_point","nb_point_Dom","nb_point_Ext"
          eval(parse(text=paste0(frame,"$nb_point_",k,"=",frame,"$nb_victoire_",k," * 3 + ",frame,"$nb_nul_",k)))
          eval(parse(text=paste0(frame,"$nb_point_Dom_",k,"=",frame,"$nb_victoire_Dom_",k," * 3 + ",frame,"$nb_nul_Dom_",k)))
          eval(parse(text=paste0(frame,"$nb_point_Ext_",k,"=",frame,"$nb_victoire_Ext_",k," * 3 + ",frame,"$nb_nul_Ext_",k)))
          #"sum_butm","sum_butm_Dom","sum_butm_Ext"
          eval(parse(text=paste0(frame,"$sum_butm_Dom_",k,"[",m,"]=sum(",frame,"$Score_Dom[(",m,"-",k,"+1):",m,"]*as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$sum_butm_Ext_",k,"[",m,"]=sum(",frame,"$Score_Ext[(",m,"-",k,"+1):",m,"]*as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$sum_butm_",k,"=",frame,"$sum_butm_Dom_",k," + ",frame,"$sum_butm_Ext_",k)))
          #"sum_bute","sum_bute_Dom","sum_bute_Ext"
          eval(parse(text=paste0(frame,"$sum_bute_Dom_",k,"[",m,"]=sum(",frame,"$Score_Ext[(",m,"-",k,"+1):",m,"]*as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]=='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$sum_bute_Ext_",k,"[",m,"]=sum(",frame,"$Score_Dom[(",m,"-",k,"+1):",m,"]*as.numeric(",frame,"$Domicile[(",m,"-",k,"+1):",m,"]!='",equipe,"'))")))
          eval(parse(text=paste0(frame,"$sum_bute_",k,"=",frame,"$sum_bute_Dom_",k," + ",frame,"$sum_bute_Ext_",k)))
          #"sum_but","sum_but_Dom","sum_but_Ext"
          eval(parse(text=paste0(frame,"$sum_but_",k,"=",frame,"$sum_butm_",k," + ",frame,"$sum_bute_",k)))
          eval(parse(text=paste0(frame,"$sum_but_Dom_",k,"=",frame,"$sum_butm_Dom_",k," + ",frame,"$sum_bute_Dom_",k)))
          eval(parse(text=paste0(frame,"$sum_but_Ext_",k,"=",frame,"$sum_butm_Ext_",k," + ",frame,"$sum_bute_Ext_",k)))
          #mean_butm,mean_butm_Dom,mean_butm_Ext
          eval(parse(text=paste0(frame,"$mean_butm_Dom_",k,"=",frame,"$sum_butm_Dom_",k,"/",frame,"$nb_match_Dom_",k)))
          eval(parse(text=paste0(frame,"$mean_butm_Ext_",k,"=",frame,"$sum_butm_Ext_",k,"/",frame,"$nb_match_Ext_",k)))
          eval(parse(text=paste0(frame,"$mean_butm_",k,"=",frame,"$sum_butm_",k,"/",frame,"$nb_match_",k)))
          #mean_bute,mean_bute_Dom,mean_bute_Ext
          eval(parse(text=paste0(frame,"$mean_bute_Dom_",k,"=",frame,"$sum_bute_Dom_",k,"/",frame,"$nb_match_Dom_",k)))
          eval(parse(text=paste0(frame,"$mean_bute_Ext_",k,"=",frame,"$sum_bute_Ext_",k,"/",frame,"$nb_match_Ext_",k)))
          eval(parse(text=paste0(frame,"$mean_bute_",k,"=",frame,"$sum_bute_",k,"/",frame,"$nb_match_",k)))
          #mean_but,mean_but_Dom,mean_but_Ext
          eval(parse(text=paste0(frame,"$mean_but_Dom_",k,"=",frame,"$sum_but_Dom_",k,"/",frame,"$nb_match_Dom_",k)))
          eval(parse(text=paste0(frame,"$mean_but_Ext_",k,"=",frame,"$sum_but_Ext_",k,"/",frame,"$nb_match_Ext_",k)))
          eval(parse(text=paste0(frame,"$mean_but_",k,"=",frame,"$sum_but_",k,"/",frame,"$nb_match_",k)))
          #mean_point,mean_point_Dom,mean_point_Ext
          eval(parse(text=paste0(frame,"$mean_point_Dom_",k,"=",frame,"$nb_point_Dom_",k,"/",frame,"$nb_match_Dom_",k)))
          eval(parse(text=paste0(frame,"$mean_point_Ext_",k,"=",frame,"$nb_point_Ext_",k,"/",frame,"$nb_match_Ext_",k)))
          eval(parse(text=paste0(frame,"$mean_point_",k,"=",frame,"$nb_point_",k,"/",frame,"$nb_match_",k)))
          #mean_ecart
          eval(parse(text=paste0(frame,"$mean_ecart_",k,"[",m,"]=sum(",frame,"$Ecart_Match[(",m,"-",k,"+1):",m,"])/",frame,"$nb_match_",k)))
        }
      }
    } 
  }
}

#creation de la base d'analyse
database=matchs[,c(20,6,16,1:5,7:10,13:15,11:12,17:19)]

#database$id_match=row.names.data.frame(database)
#matchs_t1[which(matchs_t1$id_match==12)-1,21:ncol(matchs_t1)]#37 variables
#matchs_t1_2005[which(matchs_t1$id_match==8),21:57]#37 variables
#matchs_t1_2005[which(matchs_t1$id_match==8),58:(58+36)]#k = 2 37 variables
#matchs_t1_2005[which(matchs_t1$id_match==8),95:(95+36)]#k = 3 37 variables etc jusqu'à k = 10
#eval(parse(text=paste0("M=ncol(",frame_Dom_1,")")))
#eval(parse(text=paste0("test=merge(x = database, y = ",frame_Dom_1,"[,17:M], by = 'id_match', all.x = TRUE)")))

nom_vars=c()
for (t in c("D_","E_")){
  for (i in 1:37){
    var = paste0(t,"H_",i)
    nom_vars=c(nom_vars,var)
    eval(parse(text=paste0("database$",var,"=rep(NA,4220)")))
  }
  for (k in 1:10){
    for (j in 1:37){
      if(k==1){
        var = paste0(t,"_",j)
      }else{
        var = paste0(t,"n",k,"_",j)
      }
      nom_vars=c(nom_vars,var)
      eval(parse(text=paste0("database$",var,"=rep(NA,4220)")))
    }  
  }  
}

for (m in 1:nrow(matchs)){
  sais=matchs$Saison[m];print(sais)
  dom=matchs$Domicile[m];print(dom)
  ext=matchs$Exterieur[m];print(ext)
  frame_Dom_1=paste0("matchs_t",which(teams==dom));print(frame_Dom_1)
  frame_Dom_2=paste0("matchs_t",which(teams==dom),"_",sais);print(frame_Dom_2)
  frame_Ext_1=paste0("matchs_t",which(teams==ext));print(frame_Ext_1)
  frame_Ext_2=paste0("matchs_t",which(teams==ext),"_",sais);print(frame_Ext_2)
  if(eval(parse(text=paste0("which(",frame_Dom_1,"$id_match==",m,")>1")))){
    eval(parse(text=paste0("database[",m,",21:(21+36)]=",frame_Dom_1,"[which(",frame_Dom_1,"$id_match==",m,")-1,21:(21+36)]")))
  }  
  if(eval(parse(text=paste0("which(",frame_Dom_2,"$id_match==",m,")>1")))){
    eval(parse(text=paste0("database[",m,",(21+37):(21+37+369)]=",frame_Dom_2,"[which(",frame_Dom_2,"$id_match==",m,")-1,21:390]")))
  }
  if(eval(parse(text=paste0("which(",frame_Ext_1,"$id_match==",m,")>1")))){
    eval(parse(text=paste0("database[",m,",(21+37+370):(21+37+370+36)]=",frame_Ext_1,"[which(",frame_Ext_1,"$id_match==",m,")-1,21:(21+36)]")))
  }
  if(eval(parse(text=paste0("which(",frame_Ext_2,"$id_match==",m,")>1")))){
    eval(parse(text=paste0("database[",m,",(21+37+370+37):(21+37+370+37+369)]=",frame_Ext_2,"[which(",frame_Ext_2,"$id_match==",m,")-1,21:390]")))
  } 
}

#Sauver la database
save(database,file="database_l1.Rda")

#Exportation  de la data frame vers excel
write.csv2(database, file="2.output/bdd_l1.csv")




