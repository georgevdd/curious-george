import utime as time

def bm(f, *args, **kwargs):
  start = time.ticks_us()
  n = 0
  try:
    while True:
      f(*args, **kwargs)
      n += 1
  except:
    pass
  stop = time.ticks_us()
  duration = time.ticks_diff(stop, start)
  print(n, 'iterations /', duration, 'µs =',
        (duration/n), 'µs/it =',
        (n/duration), 'it/µs')
