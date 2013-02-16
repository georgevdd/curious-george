import io
import math
import os
import sys

import bpy
import mathutils as mu

if '.' not in sys.path:
  sys.path.append('.')
import gen_materials

file = io.open
pi = math.pi


FRAMES_PER_SOLUTION = 100


def LinkMaterials():
  with bpy.data.libraries.load(
      os.path.join(os.getcwd(), gen_materials.OUTPUT_FILENAME),
      link=True,
      relative=True) as (data_from, data_to):
    data_to.materials = data_from.materials


# RGB components are in the range [0,1].
#
# NMesh.materials is a list of Material objects. NMesh.faces[i].mat is the
# index into the mesh's material list of the material to use for the i'th
# face.

# When assigning to a face's vertex index list, a final zero is ignored.
# So, when creating a square face, rotate its vertex list if necessary
# to make sure vertex zero can be specified.
def RawVertices(verts, other_data):
  if len(verts) == 4 and verts[-1] == 0:
    return ((verts[-1:] + verts[:-1]),
            (other_data[-1:] + other_data[:-1]))
  else:
    return verts, other_data


def ReadShapes():
  meshes = eval(file('shapes.txt').read())

  for i, (name, (verts, faces)) in enumerate(meshes):
    poly = bpy.data.meshes.new(name)

    poly.vertices.add(len(verts))
    for j, v in enumerate(verts):
      poly.vertices[j].co = v

    mesh_materials = []
    for (_, (mat_name, uvs)) in faces:
      if mat_name not in mesh_materials:
        mesh_materials.append(mat_name)
    for mat_name in mesh_materials:
      poly.materials.append(bpy.data.materials[mat_name])

    poly.faces.add(len(faces))
    for j, (verts, (mat_name, uvs)) in enumerate(faces):
      f = poly.faces[j]
      f.vertices_raw, uvs_ = RawVertices(verts, uvs)
      uvs[:] = uvs_
      f.material_index = mesh_materials.index(mat_name)

    poly.update()

    uv_texture = poly.uv_textures.new(gen_materials.UV_LAYER_NAME)
    for ((_, (_, uvs)), face, face_data) in zip(faces, poly.faces,
                                                uv_texture.data):
      face_data.image = (poly.materials[face.material_index].
                         texture_slots[0].texture.image)
      uvs_ = [list(uv) for uv in uvs]
      face_data.uv_raw = sum(uvs_, [])

    obj = bpy.data.objects.new(name, poly)
    bpy.context.scene.objects.link(obj)


def GetOrCreateAction(obj):
  anim_data = obj.animation_data or obj.animation_data_create()
  action = anim_data.action
  if action is None:
    action = bpy.data.actions.new('%s_Action' % obj.name)
    for prop in 'location', 'rotation_euler':
      for index in range(3):
        action.fcurves.new(prop, index)
    anim_data.action = action
  return action


def SetKeys(action, time, keys):
  for curve in action.fcurves:
    value = keys[curve.data_path][curve.array_index]
    curve.keyframe_points.insert(time, value)


def ReadSolutions():
  solutions = eval(file('pysolutions.txt').read())
  dismantle_orders = eval(file('dismantle.txt').read())
  for solution, order in zip(solutions, dismantle_orders):
    d = dict(solution)
    solution[:] = [(name, d[name]) for name in order]

  exploded_offset_h = mu.Vector((8, 0, 0))
  exploded_offset_v = mu.Vector((0, 0, 3))

  exploded_keys = {}
  all_shapes = [s for (s, _) in solutions[0]]
  for i, name in enumerate(all_shapes):
    rot = mu.Euler((0, -pi/6, 2*pi*float(i)/len(all_shapes)))
    m = mu.Matrix()
    m.translation = exploded_offset_h
    exploded_pos = rot.to_matrix().to_4x4() * m * mu.Vector((0, 0, 4))

    exploded_keys[name] = {'rotation_euler': rot,
                           'location': exploded_pos}

  for i, solution in enumerate(solutions):
    for j, (name, (rot, location)) in enumerate(solution):
      base_time = i * FRAMES_PER_SOLUTION
      solve_time = base_time + FRAMES_PER_SOLUTION/2.0
      obj = bpy.data.objects[name]
      action = GetOrCreateAction(obj)

      solved_keys = {'rotation_euler': mu.Matrix(rot).to_euler(),
                     'location': mu.Vector(location)}
      presolved_keys = {'rotation_euler': solved_keys['rotation_euler'],
                        'location': solved_keys['location'] + exploded_offset_v}
      solve_offset = j*5 + 10

      SetKeys(action, solve_time - (solve_offset + 10), exploded_keys[name])
      SetKeys(action, solve_time - (solve_offset + 3), presolved_keys)
      SetKeys(action, solve_time - solve_offset, solved_keys)
      SetKeys(action, solve_time + solve_offset, solved_keys)
      SetKeys(action, solve_time + (solve_offset + 3), presolved_keys)
      SetKeys(action, solve_time + (solve_offset + 10), exploded_keys[name])

  bpy.context.scene.frame_start = 0
  bpy.context.scene.frame_end = i * FRAMES_PER_SOLUTION - 1

if __name__ == '__main__':
  LinkMaterials()
  ReadShapes()
  ReadSolutions()
  bpy.ops.wm.save_as_mainfile(filepath="tenfold.blend", check_existing=False)
  bpy.ops.wm.quit_blender()
