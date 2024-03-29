# Table Formating Tools

## two group version

```
num_to_str<-function(x,digits=2)sprintf(paste0("%.",digits,"f"),x)

num_to_pvalue<-function(x,digits=3){
  ifelse(x<0.001,"<.001",sprintf(paste0("%.",digits,"f"),x))
}

num_to_star<-function(x,digits=3){
  dplyr::case_when(
    x<=0.001~"***",
    x<=0.01~"**",
    x<=0.05~"*",
    x<=0.1~"·",
    TRUE~"")
}

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

summarize_percent<-function(x1,x2,true_val=TRUE,missing=TRUE,digits=2){
  x1<-x1==true_val
  x2<-x2==true_val
  if(missing){
    rbind(c(paste0(sum(x1,na.rm=T)," (",num_to_str(100*sum(x1,na.rm=TRUE)/sum(!is.na(x1))),"%",")"),
            paste0(sum(x2,na.rm=T)," (",num_to_str(100*sum(x2,na.rm=TRUE)/sum(!is.na(x2))),"%",")")),
          c(paste0("(Missing: ",sum(is.na(x1)),")"),
            paste0("(Missing: ",sum(is.na(x2)),")")))
  }else{
    c(paste0(sum(x1)," (",num_to_str(100*sum(x1,na.rm=TRUE)/sum(!is.na(x1))),"%",")"),
      paste0(sum(x2)," (",num_to_str(100*sum(x2,na.rm=TRUE)/sum(!is.na(x2))),"%",")"))
  }
}

test_percent<-function(x1,x2,true_val=TRUE,digits=3){
  x1<-x1==true_val
  x2<-x2==true_val
  pvalue<-prop.test(x=c(sum(x1,na.rm=TRUE),sum(x2,na.rm=TRUE)),
                    n=c(sum(!is.na(x1)),sum(!is.na(x2))))$p.value
  ifelse(pvalue<0.001,"<.001",num_to_str(pvalue,digits=digits))
}

```
## Single group version
```
num_to_str<-function(x,digits=1)sprintf(paste0("%.",digits,"f"),x)

summarize_table<-function(atable,digits=2,ignore_paren=TRUE){
  paren_idx<-substr(row.names(atable),1,1)=="("
  nonparen_idx<-!paren_idx
  if(!ignore_paren|all(nonparen_idx)){
    cbind(paste0("  ",row.names(atable)),
          paste0(atable," (",num_to_str(100*atable/sum(atable),digits=2),"%)"))
  }else{
    rbind(cbind(paste0("  ",row.names(atable)[nonparen_idx]),
                paste0(atable[nonparen_idx]," (",num_to_str(100*atable[nonparen_idx]/sum(atable[nonparen_idx]),digits=2),"%)")),
          cbind(paste0("  ",row.names(atable)[paren_idx]),
                paste0(atable[paren_idx])))
  }
}

summarize_percent<-function(x,true_val=TRUE,missing=any(is.na(x)),digits=2){
  x<-x==true_val
  if(missing){
    rbind(paste0(sum(x,na.rm=T)," (",num_to_str(100*sum(x,na.rm=TRUE)/sum(!is.na(x))),"%",")"),
          paste0("(Missing: ",sum(is.na(x)),")"))
  }else{
    paste0(sum(x)," (",num_to_str(100*sum(x,na.rm=TRUE)/sum(!is.na(x))),"%",")")
  }
}

summarize_median<-function(x,digits=2,missing=any(is.na(x))){
  if(missing){
    rbind(paste0(num_to_str(median(x,na.rm=T),digits=digits)," (",
                 num_to_str(quantile(x,na.rm=T,0.25),digits=digits),"-",
                 num_to_str(quantile(x,na.rm=T,0.75),digits=digits),")"),
          paste0("(Missing: ",sum(is.na(x)),")"))
  }else{
    rbind(paste0(num_to_str(median(x,na.rm=T),digits=digits)," (",
                 num_to_str(quantile(x,na.rm=T,0.25),digits=digits),"-",
                 num_to_str(quantile(x,na.rm=T,0.75),digits=digits),")"))
  }
}
```
## n group version
```
library(flextable)

num_to_str<-function(x,digits=1)sprintf(paste0("%.",digits,"f"),x)

summarize_table<-function(...,category=NULL,missing=NULL,digits=1){
  a_list<-list(...)
  if(is.null(category))category<-sort(unique(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_list[[idx]]<-factor(a_list[[idx]],category)
  }
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  a_mat<-c()
  if(missing){
    a_mat<-cbind(a_mat,paste0("    ",c(category,"(Missing)")))
  }else{
    a_mat<-cbind(a_mat,paste0("    ",category))
  }
  for(idx in 1:length(a_list)){
    a_value<-a_list[[idx]]
    a_table<-table(a_value,useNA="no")
    if(missing){
      a_col<-c(
        paste0(a_table," (",num_to_str(100*a_table/sum(a_table),digits=1),"%)"),
        sum(is.na(a_value)))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-paste0(a_table," (",num_to_str(100*a_table/sum(a_table),digits=1),"%)")
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

summarize_percent<-function(...,true_val=TRUE,missing=NULL,digits=1){
  a_list<-list(...)
  a_mat<-c()
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_val<-a_list[[idx]]==true_val
    if(missing){
      a_col<-rbind(
        paste0(sum(a_val,na.rm=T)," (",num_to_str(100*sum(a_val,na.rm=TRUE)/sum(!is.na(a_val))),"%",")"),
        paste0("(Missing: ",sum(is.na(a_list[[idx]])),")"))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-paste0(sum(a_val)," (",num_to_str(100*sum(a_val,na.rm=TRUE)/sum(!is.na(a_val))),"%",")")
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

summarize_median<-function(...,missing=NULL,digits=1){
  a_list<-list(...)
  a_mat<-c()
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_val<-a_list[[idx]]
    if(missing){
      a_col<-rbind(
        paste0(num_to_str(median(a_val,na.rm=T),digits=digits)," (",
               num_to_str(quantile(a_val,na.rm=T,0.25),digits=digits),"-",
               num_to_str(quantile(a_val,na.rm=T,0.75),digits=digits),")"),
        paste0("(Missing: ",sum(is.na(a_val)),")"))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-rbind(
        paste0(num_to_str(median(a_val,na.rm=T),digits=digits)," (",
               num_to_str(quantile(a_val,na.rm=T,0.25),digits=digits),"-",
               num_to_str(quantile(a_val,na.rm=T,0.75),digits=digits),")"))
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

summarize_meansd<-function(...,missing=NULL,digits=1){
  a_list<-list(...)
  a_mat<-c()
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_val<-a_list[[idx]]
    if(missing){
      a_col<-rbind(
        paste0(num_to_str(mean(a_val,na.rm=T),digits=digits)," (",
               num_to_str(sd(a_val,na.rm=T),digits=digits),")"),
        paste0("(Missing: ",sum(is.na(a_val)),")"))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-rbind(
        paste0(num_to_str(mean(a_val,na.rm=T),digits=digits)," (",
               num_to_str(sd(a_val,na.rm=T),digits=digits),")"))
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}
```
## fill
```
cbind.fill<-function(...,fill=""){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(fill, n-nrow(x), ncol(x))))) 
}
cbind.fill(matrix(1:4,2,2),matrix(1:9,3,3))
cbind.fill(1:4,2:3)

rbind.fill<-function(...,fill=""){
  nm<-list(...)
  fn<- function(x){
    if(is.vector(x)){
      t(x)
    }else{
      as.matrix(x)
    }
  }
  nm<-lapply(nm,fn)
  n <- max(sapply(nm, ncol)) 
  do.call(rbind, lapply(nm, function (x) 
    cbind(x, matrix(fill, nrow(x), n-ncol(x))))) 
}
rbind.fill(matrix(1:4,2,2),matrix(1:9,3,3))
rbind.fill(c(1,2,3),c(1,2))
```

# format numbers
```
num_to_str_g<-function(x,digits=4)sprintf(paste0("%.",digits,"g"),x)
num_to_str_f<-function(x,digits=2)sprintf(paste0("%.",digits,"f"),x)
num_to_str_s<- function(x,digits=2)gsub("\\.$", "",formatC(signif(x,digits=digits),digits=digits,format="fg",flag="#"))

num_to_str_LY<-function(x,digit_l=6,digit_m=3,digit_s=6){
  case_when(
    abs(x)>=10^digit_l&abs(x)>=1e9~paste0(formatC(signif(x/1e9,digits=digit_m),digits=digit_m,format="fg",flag="#"),"B"),
    abs(x)>=10^digit_l&abs(x)>=1e6~paste0(formatC(signif(x/1e6,digits=digit_m),digits=digit_m,format="fg",flag="#"),"M"),
    abs(x)>=10^digit_l&abs(x)>=1e3~paste0(formatC(signif(x/1e3,digits=digit_m),digits=digit_m,format="fg",flag="#"),"K"),
    abs(x)>=10^(digit_m-1)~as.character(as.integer(x)),
    abs(x)>=10^(-digit_s+digit_m)~formatC(signif(x,digits=digit_m),digits=digit_m,format="fg",flag="#"),
    x==0~sprintf(paste0("%.",digit_m,"f"),x),
    TRUE~sprintf(paste0("%.",digit_s,"f"),x))
}

```
