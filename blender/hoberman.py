from math import acos, sqrt, pi

import bmesh
import bpy
from mathutils import Vector, Euler

DIHEDRAL_ANGLE = acos(-1 / sqrt(5)) # 63.43495
RING_ANGLES = [(pi/2, 0)] + [(pi/2 - DIHEDRAL_ANGLE, a * pi/180)
                             for a in range(0, 360, 72)]

def VertAtPos(mesh, verts_by_pos, vector):
  key = tuple(round(c, 2) for c in vector)
  vert = verts_by_pos.get(key)
  if vert is None:
    vert = mesh.verts.new(vector)
    verts_by_pos[key] = vert
  return vert

def VertAtEuler(mesh, verts_by_pos, euler):
  v = Vector((1, 0, 0))
  v.rotate(euler)
  return VertAtPos(mesh, verts_by_pos, v)

def BuildHoberman():
  mesh = bmesh.new()
  verts_by_pos = {}
  for x, z in RING_ANGLES:
    for y in range(10):
      a = VertAtEuler(mesh, verts_by_pos, Euler((x, pi/5 * y         , z), 'YXZ'))
      b = VertAtEuler(mesh, verts_by_pos, Euler((x, pi/5 * ((y+1)%10), z), 'YXZ'))
      mesh.edges.new((a, b))
  obj = bpy.data.objects.new('Hoberman', bpy.data.meshes.new('Hoberman'))
  mesh.to_mesh(obj.data)
  bpy.context.scene.objects.link(obj)

if __name__ == '__main__':
  BuildHoberman()
