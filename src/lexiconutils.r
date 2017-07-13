
LoadPosWordSet<-function(){
  iu.pos = scan("data/positive-words.txt", what='character', comment.char=';')
  pos.words = c(iu.pos)
  return(pos.words)
}

LoadNegWordSet<-function(){
  iu.neg = scan("data/negative-words.txt", what='character', comment.char=';')
  neg.words = c(iu.neg)
  return(neg.words)
}

poswords <- LoadPosWordSet()
negwords <- LoadNegWordSet()

