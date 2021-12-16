library(tidyverse)

file <- "./Day_16/Part_1/input.txt"
input <- readLines(file)

word <- input
# word <- "D2FE28"
# word <- "38006F45291200"
# word <- "8A004A801A8002F478"
# word <- "620080001611562C8802118E34"
# word <- "C0015000016115A2E0802F182340"
# word <- "A0016C880162017C3686B18A3D4780"

hex_to_bin <- function(x){
  R.utils::intToBin(strtoi(x, base = 16L))
}

binary_packet <- 
  word %>% 
  str_split("") %>% 
  map_chr(~paste0(hex_to_bin(.x), collapse=""))

packet_df <- data.frame("version"= c(NA_integer_),"type_id" = c(NA_integer_), 
                        "length_id" = c(NA_integer_), "result" = c(NA_real_),
                        "subpacket_limit" = 99999999, "to_parse" = binary_packet)

##probably track parent_id, then version, type, packet in a dataframe eventually

while(nrow(packet_df %>% filter(is.na(version)))){
  
  next_up <- which(is.na(packet_df$version))[1]
  
  binary_packet <- packet_df$to_parse[next_up] 
  # print(binary_packet)
  
  version <- strtoi(binary_packet %>% str_sub(1,3),2)
  version
  
  packet_df$version[next_up] <- version
  
  type_id <- strtoi(binary_packet %>% str_sub(4,6),2)
  type_id
  
  packet_df$type_id[next_up] <- type_id
  
  subpacket_limit <- packet_df$subpacket_limit[next_up] 
  
  if(type_id==4){
    remaining_string <- binary_packet %>% str_sub(7,)
    group_values <- c()
    while(substr(remaining_string,1,1)=="1"){
      group_values <- c(group_values, remaining_string %>% str_sub(2,5))
      remaining_string <- remaining_string %>% str_sub(6,)
    }
    if(substr(remaining_string,1,1)=="0"){
      last_group <- remaining_string %>% str_sub(2,5)
      if(str_length(last_group)<4){
        last_group <- c(last_group, rep(0,4-str_length(last_group)))}
      group_values <- c(group_values,last_group)
      remaining_string <- remaining_string %>% str_sub(6,)
    }
    result <- strtoi(paste0(group_values,collapse=""), 2)
    packet_df$result[next_up] <- result
    if((remaining_string != "")&(as.numeric(remaining_string)!=0&(subpacket_limit>1))){
      
      another_packet <- data.frame("version"= c(NA_integer_),"type_id" = c(NA_integer_), 
                                   "length_id" = c(NA_integer_), "result" = c(NA_real_),
                              "subpacket_limit" = subpacket_limit-1, "to_parse" = remaining_string)
      
      packet_df <- bind_rows(packet_df, another_packet)
    }
  }else{
    length_id <- strtoi(binary_packet %>% str_sub(7,7),2)
    length_id
    packet_df$length_id[next_up] <- length_id
    if(length_id == 0){
      length_val <- strtoi(binary_packet %>% str_sub(8,22),2)
      packet_to_parse <- binary_packet %>% str_sub(23,23+length_val-1)
      remaining_string <- binary_packet %>% str_sub(start = 23+length_val,)
      
      
      another_packet <- data.frame("version"= c(NA_integer_),"type_id" = c(NA_integer_), 
                                   "length_id" = c(NA_integer_), "result" = c(NA_real_),
                                   "subpacket_limit" = 99999999, "to_parse" = packet_to_parse)
      
      packet_df <- bind_rows(packet_df, another_packet)
      
      if(subpacket_limit>1 & as.numeric(remaining_string)!=0){
      another_packet <- data.frame("version"= c(NA_integer_),"type_id" = c(NA_integer_), 
                                   "length_id" = c(NA_integer_), "result" = c(NA_real_),
                                   "subpacket_limit" = subpacket_limit-1, "to_parse" = remaining_string)
      
      packet_df <- bind_rows(packet_df, another_packet)
      }
  
    }
    
    if(length_id == 1){
    
      length_val <- strtoi(binary_packet %>% str_sub(8,18),2)
      packet_to_parse <- binary_packet %>% str_sub(19,)
      another_packet <- data.frame("version"= c(NA_integer_),"type_id" = c(NA_integer_), 
                                   "length_id" = c(NA_integer_), "result" = c(NA_real_),
                                   "subpacket_limit" = length_val, "to_parse" = packet_to_parse)
      
      packet_df <- bind_rows(packet_df, another_packet)
  
    }
    
  }
}


packet_df %>% pull(version) %>% sum()
