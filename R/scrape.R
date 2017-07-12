#' Function
#'
#' This function returns a csv file with list of Indian scholars with citation count and whom they are cited by
#' @param
#' @keywords
#' @export
#' @examples

scrape<-function(ip)
{
  remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)  #docker server
  remDr$open()
  urls<-NULL
  ip<-gsub(" ","+",ip)
  ip<-paste("https://scholar.google.co.in/citations?hl=en&view_op=search_authors&mauthors=",ip,sep = "")
  remDr$navigate(ip)
  i<-2
  urls[1]<-ip

  while(TRUE)   #navigating
  {
    urls[i]<-unlist(remDr$getCurrentUrl())
    if((is.null(urls[i])==TRUE))
      break

    else
    {
      webElem <- remDr$findElement(using = 'css',".gs_btn_srt .gs_ico")
      webElem$clickElement()
      i<-i+1
    }
  }
  c_index<-0
  for(i in 1:length(urls))
  {
    name<-urls[i] %>% read_html() %>%
      html_nodes(".gsc_1usr_name") %>%
      html_text()

    c_index<-urls[i] %>% read_html() %>%
      html_nodes(".gsc_1usr_cby") %>%
      html_text()

    cited_by<-urls[i] %>% read_html() %>%
      html_nodes(".gsc_1usr_aff") %>%
      html_text()

    lapply(name, write, "names.txt", append=TRUE, ncolumns=1000)
    lapply(c_index, write, "citation.txt", append=TRUE, ncolumns=1000)
    lapply(c_index, write, "cited_by.txt", append=TRUE, ncolumns=1000)

  }
  unlist(name)
  name<-readLines("names.txt")
  citation<-readLines("citation.txt")
  citation<-gsub("Cited by","",citation)
  citation<-as.numeric(citation)
  cited_by<-readLines("cited_by.txt")
  corpus<-readLines("final_corpus.txt")

  status<-NULL
  for(i in 1:200)
    status[i]<-0

  for(i in 1:length(name))     #checking
  {
    p<-1
    split_names<-NULL
    pos<-NULL
    t<-name[i]
    t<-strsplit(t,"")
    t<-unlist(t)
    for(k in 1:length(t))     #finding position of splits
    {  if((t[k]==" ")==TRUE)
    {
      pos[p]<-k
      p<-p+1
    }
    }
    split_names[1]<-substr(name[i],1,pos[1]-1)
    split_names[2]<-substr(name[i],pos[1]+1,pos[2]-1)
    split_names[3]<-substr(name[i],pos[p-1]+1,length(t))
    split_names<-lapply(split_names, function(x) x[!is.na(x)])

    for(j in 1:length(split_names))
    {
      for(k in 1:length(corpus))
      {
        if((split_names[j]==corpus[k])==TRUE)
        {
          status[i]<-1
          break;
        }
      }
      if((is.na(status[i]))==TRUE)
        status[i]<-0

      if((status[i]==1)==TRUE)
      { break }
    }
  }

    for(i in 1:200)
  {
    if((status[i]==1)==TRUE)
    {  write.table(cbind(name[i],citation[i],cited_by[i]),"output.csv",append=TRUE,quote = FALSE,col.names=FALSE,row.names = FALSE,sep = ",")

    }
  }



}
