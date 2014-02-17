#!/usr/bin/env python
import csv
import osgb
import sys
import xlrd

wb = xlrd.open_workbook('/Users/georgevdd/Downloads/Sitefinder May 2012 dataset.xlsx')
sheet = wb.sheet_by_name('May 2012')

out = csv.writer(file('/Users/georgevdd/Documents/sitefinder_latlong.csv', 'w'))

for n in xrange(7, sheet.nrows):
  row = [cell.value for cell in sheet.row(n)]
  gridref = row[2]
  try:
      row += osgb.OSGB36toWGS84(*osgb.ParseGridRef(gridref))
  except:
      print >> sys.stderr, 'Unable to parse grid ref "%s".' % gridref
  out.writerow(row)
  if (n % 100 == 0):
    print '.',
