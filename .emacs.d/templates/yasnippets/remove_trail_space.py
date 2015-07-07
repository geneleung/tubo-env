#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os


def strip_content(fn):
    """update file
    Arguments:
    - `fn`:
    """
    content = open(fn, "r").read()
    if content.endswith("\n\n"):
        open(fn, 'w').write(content.strip())
        print("File: %s processed.\n" % (fn))

    else:
        print("%s ok" % fn)

if __name__ == '__main__':
    for root, dirs, fns in os.walk("."):
        for fn in fns:
            path = os.path.join(root, fn)
            strip_content(path)
