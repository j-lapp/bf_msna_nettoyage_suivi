# verifier le duration de l'entretien
ver_temps <- function(df){
  df <- df %>%
    dplyr::mutate(start = lubridate::ymd_hms(data$start, truncated = 1), 
                  end = lubridate::ymd_hms(data$start, truncated = 1),
                      duration_entretien = round(difftime(as.POSIXct(end),
                                                    as.POSIXct(start),
                                                    units = "mins"),1)
  )
  return(df)
}


make_log <- function(data, logbook, checkid="empty", index,question.name, explanation, parent.other.question=NA_character_, parent.other.answer=NA_character_, new.value=NA_character_, action="check"){
  
  if(length(index)>0){
  #   if(question.name == "uuid" || question.name == "distance_m"){
  #     if (question.name == "all"){oldval <- "-"} else {oldval <- as.character(data[[question.name]][unique(data$uuid) %in% index])}
  #     
  #     newlog <- data.frame(
  #       today = data$today[unique(data$uuid) %in% unique(index)],
  #       base = data$base[unique(data$uuid) %in% unique(index)],
  #       enumerateur = data$global_enum_id[unique(data$uuid) %in% unique(index)],
  #       uuid = index,
  #       question.name = question.name,
  #       ancienne.valeur = oldval,
  #       nouvelle.valeur = new.value,
  #       probleme = explanation,
  #       parent.other.question=ifelse(is.na(parent.other.question),NA,as.character(data[[parent.other.question]][data$uuid%in%index])),
  #       parent.other.answer=ifelse(is.na(parent.other.answer),NA,as.character(data[[parent.other.answer]][data$uuid%in%index])),
  #       checkid= checkid,
  #       action=action)
  #     
  # bind_rows(logbook,newlog)
  # }
  #   else {
      if (question.name == "all"){oldval <- "-"} else {oldval <- as.character(data[[question.name]][data$uuid %in% index])}
      
      newlog <- data.frame(
        today = data$today[data$uuid %in% index],
        base = data$base[data$uuid %in% index],
        enumerateur = data$global_enum_id[data$uuid %in% index],
        uuid = index,
        question.name = question.name,
        ancienne.valeur = oldval,
        nouvelle.valeur = new.value,
        probleme = explanation,
        parent.other.question=ifelse(is.na(parent.other.question),NA,as.character(data[[parent.other.question]][data$uuid%in%index])),
        parent.other.answer=ifelse(is.na(parent.other.answer),NA,as.character(data[[parent.other.answer]][data$uuid%in%index])),
        checkid= checkid,
        action=action)
      
      bind_rows(logbook,newlog)
    # }
    } else{
    logbook
  }
}


split_multiple_choice<-function(hh,questions,choices,sep="."){
  
  questions$type %>% ch %>% strsplit(.," ") %>% do.call(rbind,.)-> tosplit
  questions$choices <- ifelse(tosplit[,1]==tosplit[,2],NA,tosplit[,2])
  names(choices)<-paste0("ch_",names(choices))
  questionnaires<-merge(questions,choices,by.x="choices",by.y="ch_list_name",all=T)
  
  
  splitsmult<-function(hh,questionnaires,varname,sep=sep){
    chlist<-questionnaires$ch_name[which(questionnaires$name %in% varname)]
    binarysmult<-lapply(chlist,
                        function(x,hh,varname,sep){
                          filt<-grep(paste0("^",x," ","|"," ",x,"$","|","^",x,"$","|"," ",x," "),hh[[varname]])
                          hh[[paste0(varname,sep,x)]]<-c()
                          hh[[paste0(varname,sep,x)]][is.na(hh[[varname]])|hh[[varname]]=="NA"]<-NA
                          hh[[paste0(varname,sep,x)]][!is.na(hh[[varname]])&hh[[varname]]!="NA"]<-0
                          hh[[paste0(varname,sep,x)]][filt]<-1
                          return(hh[[paste0(varname,sep,x)]])
                        },hh=hh,varname=varname,sep=sep) %>% bind_cols
    names(binarysmult)<-paste(varname,chlist,sep=sep)
    return(binarysmult)
  }
  
  varname=questionnaires$name[grep("select_multiple",questionnaires$type)] %>% unique
  varname<-varname[varname%in%names(hh)]
  
  lapply(varname,splitsmult,hh=hh,questionnaires=questionnaires,sep=sep) %>% bind_cols -> splitteddata
  
  for (j in names(splitteddata)){
    hh[[j]]<-splitteddata[[j]]
  }
  return(hh)
}


impl_clean<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,action,othermain){
  for (k in 1:nrow(dclean))
  {
    Taction<-dclean[[action]][k]
    x1<-as.character(dclean[[uuid_log]][k])
    if(any(data[[uuid]]==x1)){
      if(!is.na(Taction)&Taction!="note"&Taction!="nothing"&Taction!="check"){
        if(Taction=="remove"){
          data<-data[which(!data[[uuid]]%in%dclean[[uuid_log]][k]),]
        } else if(Taction=="recode_all"){
          data[[dclean[[qmname]][k]]][data[[dclean[[qmname]][k]]]==dclean[[oldval]][k]]<-dclean[[newval]][k]
        } else if(Taction=="recode"){
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[othermain]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)
        } else if(Taction=="change") {
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[qmname]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)
        }
      }
    }
  }
  return(data)
}

cleaning_data <- function(db, clog, questions, choices){
  clean<-db %>% impl_clean("uuid",clog,"uuid","question.name","nouvelle.valeur","ancienne.valeur","action","parent.other.question")
  clean<- rec_missing_all(clean)
  clean<-clean %>% type_convert()
  clean<-clean %>% split_multiple_choice(questions,choices)
  return(clean)
  
}

