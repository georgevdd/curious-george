#!/usr/bin/env python

import os
import subprocess
import sys

IMAGE_SIZE = 256
TEXT_SIZE = 220
BORDER_SIZE = 24
OUTPUT_DIR = 'build/faces'

NUM_EXTERNAL_FACES = 10

def AllFacesIds():
  return ['%d' % i for i in range(NUM_EXTERNAL_FACES)] + ['X']


def FaceChar(i):
  if i == 'X':
    return unicode('\xe2\x99\xa5', 'utf-8')  # Have a little heart
  else:
    return i


def FaceImageFilename(i):
  return os.path.join(OUTPUT_DIR, 'face%s.png' % str(i))


def DrawCentredText(image, font, text):
  iw, ih = image.size
  tw, th = font.getsize(text)
  pos = (iw-tw)/2, (ih-th)/2
  ImageDraw.Draw(image).text(pos, text, font=font)


def DrawBorder(image, n):  # n-pixel border
  w, h = image.size
  opts = {'fill': 'white'}
  d = ImageDraw.Draw(image)
  d.rectangle(((  0,   0), (w,   n)), **opts)  # Top
  d.rectangle(((  0, h-n), (w,   h)), **opts)  # Bottom
  d.rectangle(((  0,   n), (n, h-n)), **opts)  # Left
  d.rectangle(((w-n,   n), (w, h-n)), **opts)  # Right


def GenImages():
  font = ImageFont.truetype('/System/Library/Fonts/Menlo.ttc',
                            TEXT_SIZE,
                            1)  # Index into font collection
  for i in AllFacesIds():
    image = Image.new('RGB', (IMAGE_SIZE, IMAGE_SIZE))
    DrawCentredText(image, font, FaceChar(i))
    DrawBorder(image, BORDER_SIZE)
    image.save(FaceImageFilename(i))

  xx = 128

  image = Image.new('RGB', (xx, xx))
  ImageDraw.Draw(image).rectangle(((0,0), (xx-1,xx-1)), fill='white')
  image.save(os.path.join(OUTPUT_DIR, 'solid.png'))

  image = Image.new('RGB', (xx, xx))
  ImageDraw.Draw(image).rectangle(((0,0), (xx-1,xx/2-1)), fill='white')
  image.save(os.path.join(OUTPUT_DIR, 'stripe_h.png'))

  image = Image.new('RGB', (xx, xx))
  d = ImageDraw.Draw(image)
  d.polygon(((0,0), (0,xx/2-1), (xx/2-1,0)), fill='white')
  d.polygon(((xx,0), (0,xx), (xx/2-1, xx), (xx, xx/2-1)), fill='white')
  image.save(os.path.join(OUTPUT_DIR, 'stripe_d.png'))

if __name__ == '__main__':
  # Defer imports till now, so that this module can be imported by Blender
  # scripts.
  import ImageFont
  import Image
  import ImageDraw
  subprocess.check_call(['mkdir', '-p', OUTPUT_DIR])
  GenImages()
