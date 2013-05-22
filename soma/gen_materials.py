#!/usr/bin/env python

import os
import sys
import subprocess

import bpy
import mathutils as mu

if '.' not in sys.path:
  sys.path.append('.')
import gen_images


OUTPUT_DIR = 'build'
OUTPUT_FILENAME = os.path.join(OUTPUT_DIR, 'materials1.blend')
UV_LAYER_NAME = 'UV'


def LinkMaterials():
  lib_symlink = os.path.join(OUTPUT_DIR, 'lib')
  if not os.path.islink(lib_symlink):
    os.symlink('lib', lib_symlink)
  
  for path in [
      'lib/materials.blend',
    ]:
    with bpy.data.libraries.load(
        path,
        link=True,
        relative=True) as (data_from, data_to):
      data_to.materials = data_from.materials


STRIPES = ['solid', 'stripe_h', 'stripe_d']


def GenStripes():
  for x in STRIPES:
    image = bpy.data.images.new(x, width=128, height=128)
    image.source = 'FILE'
    image.filepath = os.path.join(gen_images.OUTPUT_DIR, '%s.png' % x)
    texture = bpy.data.textures.new(x.title(), type='IMAGE')
    texture.image = image


def GenMaterial(i):
    name = 'Face%s' % i

    image = bpy.data.images.new(name, width=256, height=256)
    image.source = 'FILE'
    image.filepath = gen_images.FaceImageFilename(i)

    texture = bpy.data.textures.new(name, type='IMAGE')
    texture.image = image

    material = bpy.data.materials.new(name)
    material.use_fake_user = True
    material.preview_render_type = 'CUBE'
    if i not in ('X', '9'):
      ii = int(i)
      col = mu.Color((1,0,0))
      col.h = (ii // 3) / 3
      print(col.h)
      stripe = STRIPES[ii % 3].title()
    else:
      stripe = None

    texture_slot = material.texture_slots.add()
    texture_slot.texture = bpy.data.textures['Plywood']
    texture_slot.mapping = 'CUBE'
    texture_slot.blend_type = 'COLOR'

    if stripe:
      texture_slot = material.texture_slots.add()
      texture_slot.texture = bpy.data.textures[stripe]
      texture_slot.texture_coords = 'UV'
      texture_slot.uv_layer = UV_LAYER_NAME
      texture_slot.scale[0] = 6
      texture_slot.scale[1] = 6
      texture_slot.color = col
      texture_slot.diffuse_color_factor = 0.75
      texture_slot.use_rgb_to_intensity = True
      texture_slot.blend_type = 'COLOR'

    texture_slot = material.texture_slots.add()
    texture_slot.texture = texture
    texture_slot.texture_coords = 'UV'
    texture_slot.uv_layer = UV_LAYER_NAME
    texture_slot.blend_type = 'LIGHTEN'


def GenMaterials():
  for i in gen_images.AllFacesIds():
    GenMaterial(i)


if __name__ == '__main__':
  subprocess.check_call(['mkdir', '-p', OUTPUT_DIR])
  LinkMaterials()
  GenStripes()
  GenMaterials()
  bpy.ops.wm.save_as_mainfile(filepath=OUTPUT_FILENAME, check_existing=False)
  bpy.ops.wm.quit_blender()
