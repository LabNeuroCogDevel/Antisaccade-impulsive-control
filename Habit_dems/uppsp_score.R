#' Author: BTC
#' upps_scoring - scores uppsp 59-item version
#'   * wide format data only
#'   * items must start with column number
#'   * returns five factor scores and total
#' @param uppspdf - dataframe of recorded uppsp responses
#' @export
#' @examples
#'   uppsp<-readxl::read_excel("/Volumes/Phillips/mMR_PETDA/scripts/txt/PET_Sheets.xlsx",sheet="UPPS-P")
#'   uppsp<-uppsp[!is.na(uppsp$ID),]
#'   uppsp_scoring(uppsp)
uppsp_scoring<-function(uppspdf, uppscols=NULL){
   ######BTC & WF#########
   ##last updated 11202017########
  
#assign my data to this function
uppspdf<- Full_Merged_uppsp
uppscols<-colnames(Full_Merged_uppsp [3:61])
  #as factor categories  
   if(is.null(uppscols)) uppscols<-grep("([0-9]+).*$",names(uppspdf),value=TRUE)
   stopifnot(length(uppscols) == 59)

   response_factors <- c("Disagree Strongly","Disagree Some","Agree Some", "Agree Strongly")

   all_values <- unique(na.omit(unlist(uppspdf[,uppscols])))
   # [1] "Disagree Strongly" "Agree Some"        "Disagree Some"     "Agree Strongly"

   # TODO: use '! is.numeric(uppspdf[,uppscols[1]])' instead? no need for all_values
   need_factor <- any(all_values %in% response_factors)
   if(need_factor)  {
     #TODO: confirm 1=strongdisagree 4=strongagree is how non-rev score should be
     uppspdf[,uppscols] <- apply(uppspdf[,uppscols], 2 ,\(x) as.numeric(factor(x, levels=response_factors),simplify=F))
   }
   ####scales
   revitems<-c(2,3,5,7,8,9,10,12,13,15,17,18,20,22,23,25,26,29,30,31,34,35,36,39,40,41,44,45,46,47,49,50,51,52,54,56,57,58,59)
   nonrevitems<-c(1,4,6,11,14,16,19,21,24,27,28,32,33,37,38,42,43,48,53,55)

   uppspdf_adjust<-uppspdf
   for (item in uppscols){
      itemnumber<-gsub("UPPS.P.Q0_","",item)
      scores<-as.numeric(unlist(uppspdf[,item]))
      is_rev_coded <- as.numeric(itemnumber) %in% revitems
      if(is_rev_coded){
         revfinaloutscore<-(5-scores)
         uppspdf_adjust[,item]<-revfinaloutscore
      }
      debug_mean <- mean(uppspdf_adjust[,item],na.rm=T)
      debug_n <- length(which(!is.na(uppspdf_adjust[,item])))
      print(glue::glue("DEBUG: colum '{item}' is num '{itemnumber}'. rev? {is_rev_coded}; mean: {debug_mean}, n: {debug_n}"))
   }
   uppspdf_adjust_jd<-uppspdf_adjust[,uppscols]



   score_upps_scales<-function(itemnumbers){
      items_match<-sapply(FUN=function(x){sprintf("^%d\\.|_%d$|_%02d$",x,x,x)},itemnumbers)
      items_match_reg<-paste(items_match,collapse="|")
      rowSums(uppspdf_adjust[,grep(items_match_reg,names(uppspdf_adjust))],na.rm=TRUE)
   }

   ############scales####################################
   NUitems<-c(2,7,12,17,22,29,34,39,44,50,53,58)
   NUscores<-score_upps_scales(NUitems)
   Premeditems<-c(1,6,11,16,21,28,33,38,43,48,55)
   PREscores<-score_upps_scales(Premeditems)
   Persitems<-c(4,9,14,19,24,27,32,37,42,47)
   PERSscores<-score_upps_scales(Persitems)
   SSitems<-c(3,8,13,18,23,26,31,36,41,46,51,56)
   SSscores<-score_upps_scales(SSitems)
   PUitems<-c(5,10,15,20,25,30,35,40,45,49,52,54,57,59)
   PUscores<-score_upps_scales(PUitems)

   totalscores<-score_upps_scales(seq(1,59,1))
   #################################
   UPPS_scales_returndf<-data.frame(NUscores,PREscores,PERSscores,SSscores,PUscores,totalscores)
   names(UPPS_scales_returndf)<-c("upps_negurg","upps_pre","upps_pers","upps_ss","upps_pu","upps_tot")
   return(cbind(uppspdf, UPPS_scales_returndf))
}
