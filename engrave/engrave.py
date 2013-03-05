# To be run with blender -P
import bpy

font = bpy.data.fonts.load('/usr/share/fonts/truetype/droid/DroidSans.ttf')
id_counter=0


def NewId():
  global id_counter
  id_counter += 1
  return id_counter


def MakeText(text, font, width=None):
  obj_id = NewId()
  text_curve = bpy.data.curves.new(name='Text%d' % obj_id, type='FONT')
  text_curve.font = font
  text_curve.body = text
  if width is not None:
    text_curve.text_boxes[0].width = width
  text_obj = bpy.data.objects.new(name='Text%d' % obj_id,
                                  object_data=text_curve)
  return text_obj


def MakeRectangleEdges(x, y, w, h):
  obj_id = NewId()
  plane_mesh = bpy.data.meshes.new(name='Plane%d' % obj_id)
  plane_mesh.vertices.add(4)
  plane_mesh.edges.add(4)
  for i, (xx, yy) in enumerate([(x, y), (x, y+h),
                                (x+w, y+h), (x+w, y)]):
    plane_mesh.vertices[i].co=(xx, yy, 0)
    plane_mesh.edges[i].vertices=[i, (i+1)&3]
  # Don't add any face to this mesh, otherwise it won't be converted
  # to a curve properly. Curve conversion seems to expect the input
  # mesh to be a series of vertices joined by edges.
  plane_obj = bpy.data.objects.new(name='Plane%d' % obj_id,
                                   object_data=plane_mesh)
  return plane_obj


def ConvertToCurve(obj):
  name = obj.name
  # The object to be converted seems to need to be both active and
  # selected. Only one object can be converted at a time.
  obj.select=True
  bpy.context.scene.objects.active=obj
  bpy.ops.object.convert(target='CURVE')
  # 'obj' still refers to the mesh here, so look up the new curve by
  # the same name.
  bpy.data.curves[name].dimensions='2D'


def JoinAll(objects):
  bpy.ops.object.select_all(action='DESELECT')
  for o in objects:
    o.select = True
  bpy.ops.object.join()


def Example():
  t1 = MakeText('First', font, width=3)
  t1.location = (1, 5, 0)
  t2 = MakeText('Second', font, width=3)
  t2.location = (1, 3, 0)
  t3 = MakeText('Third', font, width=3)
  t3.location = (1, 1, 0)
  boundary = MakeRectangleEdges(0, 0, 5, 7)
  all = [t1, t2, t3, boundary]

  for obj in all:
    bpy.context.scene.objects.link(obj)
    ConvertToCurve(obj)
  JoinAll(all)

  # The active object is now the result of the JoinAll() operation.
  bpy.ops.object.modifier_add(type='SOLIDIFY')
  bpy.context.scene.objects.active.modifiers[0].thickness=0.125


Example()
bpy.ops.export_mesh.stl(filepath='/tmp/engraving.stl')


if False:
  bpy.ops.wm.save_as_mainfile(filepath='/tmp/engraving.blend')
  bpy.ops.wm.quit_blender()
