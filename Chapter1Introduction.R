library(ggplot2)


#Figure1.1(a) Plot of exam mark against the grades of student from grade 1-8 without any longitudinal pattern#
f1a<-data.frame(
  Grade=c(1,1,2,3,3,4,4,5,7,8),
  Exam_mark=c(1,3,0,1,4,2,5,4,6,5),
  Subject=c(1,3,2,2,3,1,4,5,4,5)
)
f1aplot<- ggplot(data = f1a, aes(x=Grade,y=Exam_mark))
f1aplot+geom_point()

#Figure1.1(b) Plot of exam mark against the grades of student from grade 1-8 with positive longitudinal pattern#
f1bplot<-ggplot(data = f1a, aes(x=Grade,y=Exam_mark, group = Subject))
f1bplot+geom_point()+geom_line()

#Figure1.1(c) Plot of exam mark against the grades of student from grade 1-8 with negative lingitudinal pattern#
f1c<-data.frame(
  Grade=c(1,1,2,3,3,4,4,5,7,8),
 Exam_mark=c(1,3,0,1,4,2,5,4,6,5),
  Subject=c(1,2,1,2,3,3,4,4,5,5)
)

f1cplot<-ggplot(data = f1c, aes(x=Grade,y=Exam_mark, group = Subject))
f1cplot+geom_point()+geom_line()

