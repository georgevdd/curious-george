#!/usr/bin/env python

from pyscript import *

defaults.units=UNITS['mm']

blue=Color(.65,.65,1)
white=Color(1)

p = Page(size='a4')

outers = [Rectangle(height=400, width=400,
                    nw=P(x*410, y*410),
                    linewidth=1, bg=blue)
          for x in range(3)
          for y in range(3)]
inners = [Rectangle(height=382, width=382,
                    c = o.c,
                    linewidth=1, bg=white)
          for o in outers]

g = Group(*(outers + inners),
           c=p.bbox().c)
g.scale(0.1, p=g.c)

g2 = g.copy()
g2.scale(0.1, 1)
g2.shear(1, 0)
g2.e = g.w

g3 = g.copy()
g3.scale(1, 0.1)
g3.shear(-1, 90)
g3.s = g.n

p.append(g, g2, g3)

render(p, file='face-template.eps')
