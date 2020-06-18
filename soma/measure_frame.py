import bpy
import mathutils as mu
import sys

frame = [x for x in bpy.data.objects
         if type(x.data) == bpy.types.Mesh and x.layers[12]]

def bounds(vs):
    return tuple([
        [f([v[i] for v in vs]) for i in range(3)] for f in (min, max)])

def round_vector(v):
    return [round(c, 3) for c in v]

cuts = {}
for o in bpy.context.selected_objects:
  m = o.data
  if type(m) is not bpy.types.Mesh:
      continue
  mat_name = m.materials[0].name
  bs = bounds([v.co for v in m.vertices])
  bs = tuple([mu.Vector(round_vector(b)) * 1000 for b in bs])
  size = bs[1] - bs[0]
  lengths = [c for c in size if int(c) != 34]
  print(lengths, mat_name)
  if len(lengths) != 1:
      raise ValueError('Ooops: ' + o.name)
  cuts[lengths[0]] = cuts.get(lengths[0], 0) + 1

for length, count in cuts.items():
    print(length, '\t', count)
