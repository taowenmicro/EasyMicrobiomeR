data("Feldspar")

#Base Plot
base = ggtern(data=Feldspar,aes(Ab,An,Or))

#Plot with Points
base + geom_point()

#Plot with Points, custom shape and sizes
base + geom_point(shape=2,size=5)

#Plot with Poitns, and mappings
base + 
  geom_point(aes(fill=Feldspar,shape=Feldspar,size=P.Gpa),color='black') + 
  scale_shape_manual(values=c(21,22))
base + labs(x = "Left",y="Top",z="Right")
