#!/usr/bin/env python
# -*- coding: utf-8 -*-
import glob
def update_file(fn):
    """update file
    Arguments:
    - `fn`:
    """
    content = open(fn, "r").read()
    if content.find("$") == -1:
        content = content.strip() + "($0)"
        open(fn, 'w').write(content)
    else:
        print("%s ok"%fn)
if __name__ == '__main__':
    for fn in glob.glob("*"):
        if not fn.endswith("py"):
            update_file(fn)
