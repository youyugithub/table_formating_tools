# Table Formating Tools

```
num_to_str<-function(x,digits=2)sprintf(paste0("%.",digits,"f"),x)

summarize_median<-function(x1,x2,digits=2){
  c(paste0(num_to_str(median(x1,na.rm=T),digits=digits)," (",
           num_to_str(quantile(x1,na.rm=T,0.25),digits=digits),"-",
           num_to_str(quantile(x1,na.rm=T,0.75),digits=digits),")"),
    paste0(num_to_str(median(x2,na.rm=T),digits=digits)," (",
           num_to_str(quantile(x2,na.rm=T,0.25),digits=digits),"-",
           num_to_str(quantile(x2,na.rm=T,0.75),digits=digits),")"))
}

summarize_meansd<-function(x1,x2,digits=2){
  c(paste0(num_to_str(mean(x1,na.rm=T),digits=digits)," ± ",
           num_to_str(sd(x1,na.rm=T),digits=digits)),
    paste0(num_to_str(median(x2,na.rm=T),digits=digits)," ± ",
           num_to_str(sd(x2,na.rm=T),digits=digits)))
}

test_continuous<-function(x1,x2,digits=3){
  pvalue<-t.test(x1,x2)$p.value
  ifelse(pvalue<0.001,"<.001",num_to_str(pvalue,digits=digits))
}

summarize_table<-function(atable,digits=2,ignore_paren=TRUE){
  paren_idx<-substr(row.names(atable),1,1)=="("
  nonparen_idx<-!paren_idx
  if(!ignore_paren|all(nonparen_idx)){
    cbind(paste0("  ",row.names(atable)),
          paste0(atable[,"Group1"]," (",
                 num_to_str(100*atable[,"Group1"]/sum(atable[,"Group1"]),digits=2),"%)"),
          paste0(atable[,"Group2"]," (",
                 num_to_str(100*atable[,"Group2"]/sum(atable[,"Group2"]),digits=2),"%)"))
  }else{
    rbind(cbind(paste0("  ",row.names(atable)[nonparen_idx]),
                paste0(atable[nonparen_idx,"Group1"]," (",
                       num_to_str(100*atable[nonparen_idx,"Group1"]/sum(atable[nonparen_idx,"Group1"]),digits=2),"%)"),
                paste0(atable[nonparen_idx,"Group2"]," (",
                       num_to_str(100*atable[nonparen_idx,"Group2"]/sum(atable[nonparen_idx,"Group2"]),digits=2),"%)")),
          cbind(paste0("  ",row.names(atable)[paren_idx]),
                paste0(atable[paren_idx,"Group1"]),
                paste0(atable[paren_idx,"Group2"])))
  }
}

test_table<-function(atable,digits=3,ignore_paren=TRUE){
  paren_idx<-substr(row.names(atable),1,1)=="("
  nonparen_idx<-!paren_idx
  pvalue<-fisher.test(atable[nonparen_idx,])$p.value
  ifelse(pvalue<0.001,"<.001",num_to_str(pvalue,digits=digits))
}
```
