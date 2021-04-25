# /bin/bash

filename=${1:?}

mpy-cross "${filename}" && /Users/georgevdd/src/webrepl/webrepl_cli.py -p "${WEBREPL_PASSWORD:?}" {.,'192.168.1.234:'}/${filename%py}mpy
