shelling(1000,50,1000,1000,6,6)


#times:the times of interation
#len_side:the length of the side of the square
#white:the number of the white color
#black:the number of the black color
#rate_white:how many white people the white prefers surrounded
#rate_black:how many black people the black prefers surrounded
#num_surrounded:how many people everyone prefer surrounded


shelling<-function(times,len_side,white,black,rate_white,rate_black,num_surrounded=0){
  size=white+black
  area=len_side*len_side
  alllist=sample(1:area,size,replace=FALSE)
  white_list=alllist[1:white]
  black_list=alllist[(white+1):size]
  blocklist=rep(0,area)
  blocklist[white_list]=1
  blocklist[black_list]=2
  block=matrix(blocklist,nrow=len_side,ncol=len_side)
  
  ####################################################################################################
while(times>0){
    blockx=c()
    blocky=c()
    for (x in 1:len_side){
      for (y in 1:len_side){
            if (block[x,y]!=0){
                    if (x==1|y==len_side|x==len_side|y==1){
                      blockx=c(blockx,x)
                      blocky=c(blocky,y)
                    }
            
                    else{
                            if(block[x,y]==2){
                              temporary_list=c(block[x-1,y-1],block[x-1,y],block[x-1,y+1],block[x,y-1],block[x,y+1],block[x+1,y-1],block[x+1,y],block[x+1,y+1])
                              empty=sum(temporary_list==0)
                              if (empty>(8-num_surrounded)){
                                blockx=c(blockx,x)
                                blocky=c(blocky,y)
                              }else{
                                num_white=sum(temporary_list==1)
                                num_black=sum(temporary_list==2)
                                if (num_black<rate_black){
                                  blockx=c(blockx,x)
                                  blocky=c(blocky,y)
                                }
                              }
                            }
                              if(block[x,y]==1){
                                temporary_list=c(block[x-1,y-1],block[x-1,y],block[x-1,y+1],block[x,y-1],block[x,y+1],block[x+1,y-1],block[x+1,y],block[x+1,y+1])
                                empty=sum(temporary_list==0)
                                if (empty>(8-num_surrounded)){
                                  blockx=c(blockx,x)
                                  blocky=c(blocky,y)
                                }else{
                                  num_white=sum(temporary_list==1)
                                  num_black=sum(temporary_list==2)
                                  if (num_white<rate_white){
                                    blockx=c(blockx,x)
                                    blocky=c(blocky,y)
                                  }
                                }
                              }
                
                }
          }
      }
    }
    
  
  
  if(length(blockx)!=0){
    for (i in 1:length(blockx)){
      a=sample(1:len_side,size=1)
      b=sample(1:len_side,size=1)
      while(block[a,b]!=0){
        a=sample(1:len_side,size=1)
        b=sample(1:len_side,size=1)
      }
      block[a,b]=block[blockx[i],blocky[i]]
      block[blockx[i],blocky[i]]=0
    }
  }
times=times-1  

}

  ####################################################################################################




  whitex=c()
  whitey=c()
  blackx=c()
  blacky=c()
  
  for(m in 1:len_side){
    for(n in 1:len_side){
      if (block[m,n]==1){
        whitex=c(whitex,m)
        whitey=c(whitey,n)
      }
      if (block[m,n]==2){
        blackx=c(blackx,m)
        blacky=c(blacky,n)
      }
    }
  }
  
  plot(whitex,whitey,col="red",pch=15,xlim=c(1,len_side),ylim=c(1,len_side))
  points(blackx,blacky,col="blue",pch=15)
  
  

}


