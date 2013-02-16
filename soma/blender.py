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


FRAMES_PER_SOLUTION = 10


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


def ReadSolutions():
  solutions = eval(file('pysolutions.txt').read())

  for i, solution in enumerate(solutions + solutions[:1]):
    for name, (rot, location) in solution:
      props = {'rotation_euler': mu.Matrix(rot).to_euler(),
               'location': location }

      obj = bpy.data.objects[name]

      anim_data = obj.animation_data or obj.animation_data_create()
      action = anim_data.action
      if action is None:
        action = bpy.data.actions.new('%s_Action' % name)
        for prop in 'location', 'rotation_euler':
          for index in range(3):
            action.fcurves.new(prop, index)
        anim_data.action = action

      for curve in action.fcurves:
        value = props[curve.data_path][curve.array_index]
        curve.keyframe_points.insert(i * FRAMES_PER_SOLUTION, value)

  bpy.context.scene.frame_end = i * FRAMES_PER_SOLUTION

if __name__ == '__main__':
  LinkMaterials()
  ReadShapes()
  ReadSolutions()
  bpy.ops.wm.save_as_mainfile(filepath="tenfold.blend", check_existing=False)
  bpy.ops.wm.quit_blender()
