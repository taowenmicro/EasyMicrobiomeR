data("Feldspar")

#Base Plot
base = ggtern(data=Feldspar,aes(Ab,An,Or))

#Clipping Masks
A = base + 
  geom_point(shape = 21, color = 'black', fill = 'red', size = 10) + 
  labs(title="With Mask")
A

B = base + 
  theme_nomask() + 
  geom_point(shape = 21, color = 'black', fill = 'orange', size = 10) +
  labs(title='Without Mask')
B

C = base + 
  geom_mask() +
  geom_point(shape = 21, color = 'black', fill = 'blue', size = 10) +
  labs(title = "Manual Mask, Underneath Points")
C

D = base + 
  geom_mask() +
  geom_point(shape = 21, color = 'black', fill = 'green', size=10) +
  labs(title = "Manual Mask, Turned Off") +
  theme_nomask()
D

#Assemble in a grid
grid.arrange(A,B,C,D,ncol=2)

