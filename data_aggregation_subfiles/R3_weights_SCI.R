########################
### R3_weights_SCI.R ###
########################

getwd()
t_start <-Sys.time()

# sanity_check_sci_matrix = T/F    ------------------------------------------
if(sanity_check_sci_matrix == T){
  check_df_sci_matrix <-c(rep(NA,dim(df_sci)[1]))
  ifrm(i)
  
  for(i in 1:nrow(df_sci)){
    id_firstANDsecond_cntry <- df_sci[,"fr_loc"] %in% df_sci[i,"user_loc"] & df_sci[,"user_loc"] %in% df_sci[i,"fr_loc"]
    df_sci[i,c("user_loc", "fr_loc", "scaled_sci")]
    df_sci[id_firstANDsecond_cntry,c("user_loc", "fr_loc", "scaled_sci")]
    
    
    check_df_sci_matrix[i] <- as.vector(table(id_firstANDsecond_cntry))[2] ==1 & df_sci[id_firstANDsecond_cntry,3] == df_sci[i,3]
  }
  ifrm(i) 
  ifrm(id_firstANDsecond_cntry)
  check_df_sci_matrix
  table(check_df_sci_matrix)
  length(check_df_sci_matrix)
  as.vector(table(check_df_sci_matrix)) == length(check_df_sci_matrix)
  as.vector(table(check_df_sci_matrix)) == dim(df_sci)[1]
  
  if(as.vector(table(check_df_sci_matrix)) == length(check_df_sci_matrix)){
    print("check_sci check successfull")}
  else{
    print("check_sci FAILED")}
  
  print("check_sci_matrix > ok")
  t_end_check_sci_matrix <-Sys.time()
  t_R3_check_sci <- t_end_check_sci_matrix - t_start
  
  ifrm(t_end_check_sci_matrix)
  ifrm(check_df_sci_matrix)
  }


# run_sci_weights_generation = T/F    ------------------------------------------
if(run_sci_weights_generation == F){
  load("data/0_R_data/R3_weights.Rdata")
}else if(run_sci_weights_generation == T){
    
  # # SCI WEIGHTS > set weights for SCI and add computed values into df_MAIN    ---------------------------
  get_DEGROOT_weights("ld_w_sci", df_sci$user_loc, df_sci$fr_loc, df_sci$scaled_sci)
 
  # save_data_R3_weights = T/F  ----------------------------------------
  if(save_data_R3_weights == T){
     save(list =  ls(.GlobalEnv),file = "data/0_R_data/R3_weights.Rdata")
   } # end save_data_R3_weights
  
}# end run_sci_weights_generation


 # end  ----------------------------------------
t_end <-Sys.time()
t_R3<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R3_weights_SCI.R", ";  runtime ", round(t_R3[[1]],2)," ", units(t_R3), sep=""))


ifrm(sanity_check_sci_matrix)
ifrm(run_sci_weights_generation)
ifrm(save_data_R3_weights)
