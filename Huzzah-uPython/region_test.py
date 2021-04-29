import unittest
import region

fake_strip = list(range(100, 120))


def cr(start, stop):
  return region.ContiguousRegion(fake_strip, start, stop)


class ContiguousRegionGetItemTest(unittest.TestCase):

  def setUp(self):
    self.r = cr(0, 20)

  def test_get_single_item_returns_value(self):
    self.assertEqual(100, self.r[0])
    self.assertEqual(119, self.r[-1])

  def test_whole_slice_gives_equal_object(self):
    self.assertEqual(self.r, self.r[:])

  def test_contiguous_slice_gives_contiguous_region(self):
    self.assertEqual(cr(5, 10), self.r[5:10])

  def test_contiguous_reversed_slice_gives_contiguous_reversed_region(self):
    self.assertEqual(cr(12, 3), self.r[12:3:-1])

  def test_reverse_whole_slice_gives_whole_region_reversed(self):
    self.assertEqual(cr(20, 0), self.r[::-1])
    self.assertEqual(self.r, self.r[::-1][::-1])

  def test_slice_reversed_region_gives_reversed_region(self):
    self.assertEqual(cr(7, 4), self.r[10::-1][3:6])


class ContiguousRegionSetItemTest(unittest.TestCase):

  def setUp(self):
    self.fake_strip = list(range(100, 110))
    self.r = region.ContiguousRegion(self.fake_strip, 0, 10)

  def test_set_single_item_assigns_value(self):
    self.r[0] = 1000
    self.r[4] = 1004
    self.r[-1] = 1009
    self.assertEqual(self.fake_strip,
                     [1000, 101, 102, 103, 1004, 105, 106, 107, 108, 1009])

  def test_set_whole_slice_assigns_all_elements(self):
    self.r[:] = 1000
    self.assertEqual([1000] * 10, self.fake_strip)

  def test_set_contiguous_slice_fills_contiguous_region(self):
    self.r[2:7] = 1000
    self.assertEqual([100, 101, 1000, 1000, 1000, 1000, 1000, 107, 108, 109],
                     self.fake_strip)

  def test_set_contiguous_reversed_slice_fills_contiguous_region(self):
    self.r[8:3:-1] = 1000
    self.assertEqual([100, 101, 102, 103, 1000, 1000, 1000, 1000, 1000, 109],
                     self.fake_strip)

  def test_set_whole_reversed_slice_fills_whole_region(self):
    self.r[::-1] = 1000
    self.assertEqual([1000] * 10, self.fake_strip)

  def test_set_partial_reversed_slice_fills_partial_region(self):
    self.r[5::-1][1:3] = 1000
    self.assertEqual([100, 101, 102, 1000, 1000, 105, 106, 107, 108, 109],
                     self.fake_strip)


if __name__ == '__main__':
  unittest.main()
