maxNum::[Int]->Int
maxNum[]=error "Not Implemented"
maxNum[x]=x
maxNum(x:xs)=max x (maxNum xs)
