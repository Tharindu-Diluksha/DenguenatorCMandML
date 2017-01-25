circularIndex = function(index, lag=1, array.size) {
  newIndex = index-lag
  
  return(ifelse(newIndex<=0, (array.size-abs(newIndex)), newIndex))
}

joinFrames = function(frame1, frame2) {
  if(nrow(frame1)==0) {
    frame1 = frame2
  }
  else {
    frame1EndIndex = nrow(frame1)
    frame1[(frame1EndIndex+1):(frame1EndIndex+nrow(frame2)),] = frame2[1:nrow(frame2),]  
  }
  
  return(frame1)
}
