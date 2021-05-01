class Region:
  """An indexable sequence of disjoint pixels in a single strip.

  See https://docs.micropython.org/en/latest/genrst/builtin_types.html
  for some limitations of slicing in MicroPython.
  """

  def __init__(self, strip, indices):
    self.strip = strip
    self.indices = list(indices)

  def __getitem__(self, key):
    if isinstance(key, slice):
      return Region(self.strip, [
        self.indices[n] for n in range(key.start or 0,
                                       key.stop or len(self.indices),
                                       key.step or 1)])
    else:
      return self.strip[self.indices[key]]

  def __setitem__(self, key, value):
    if isinstance(key, slice):
      for n in range(key.start or 0,
                     key.stop or len(self.indices),
                     key.step or 1):
        self.strip[self.indices[n]] = value
    else:
      self.strip[self.indices[key]] = value

  def __len__(self):
    return len(self.indices)

  def __eq__(self, other):
    return (self.strip == other.strip and
            self.indices == other.indices)

  def __repr__(self):
    return 'Region(%d, %s)' % (
        id(self.strip),
        self.indices)


class ContiguousRegion:
  """A contiguous indexable sequence of pixels in a single strip.

  See https://docs.micropython.org/en/latest/genrst/builtin_types.html
  for some limitations of slicing in MicroPython.
  """

  def __init__(self, strip, start, stop):
    self.strip = strip
    self.start = start
    self.stop = stop

  def __getitem__(self, key):
    sgn = 1 if self.stop >= self.start else -1
    if isinstance(key, slice):
      step = key.step or 1
      if step in (1, -1):
        start = (key.start if key.start is not None
                 else (0 if step == 1 else len(self)))
        stop = (key.stop if key.stop is not None
                else (len(self) if step == 1 else 0))

        if stop > len(self): stop = len(self)

        if sgn == -1:
          start = -start
          stop = -stop

        return ContiguousRegion(self.strip,
                                self.start + start,
                                self.start + stop)
      else:
        # TODO: unit tests for this branch
        start = key.start or 0
        stop = key.stop or len(self)
        step = key.step or 1

        if sgn == -1:
          start, stop = stop, start
          step = -step
        return Region(self.strip, [self.start + n
                                   for n in range(start, stop, step)])
    else:
      return self.strip[self.start + sgn * key]

  def __setitem__(self, key, value):
    sgn = 1 if self.stop >= self.start else -1
    length = abs(self.stop - self.start)

    if isinstance(key, slice):
      step = key.step or 1
      if step in (1, -1):
        start = (key.start if key.start is not None
                 else (0 if step == 1 else length-1))
        stop = (key.stop if key.stop is not None
                else (length if step == 1 else -1))

        if stop > length: stop = length

        if sgn == -1:
          start = -start
          stop = -stop

        for n in range(self.start + start, self.start + stop, sgn * step):
          self.strip[n] = value
      else:
        # TODO: Return a strided Region
        raise NotImplementedError
    else:
      self.strip[self.start + sgn * key] = value

  def __len__(self):
    return abs(self.stop - self.start)

  def __eq__(self, other):
    return (self.strip == other.strip and
            self.start == other.start and
            self.stop == other.stop)

  def __repr__(self):
    return 'ContiguousRegion(%d, %d, %d)' % (
        id(self.strip),
        self.start,
        self.stop)
