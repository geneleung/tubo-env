#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 2016-02-26
# should be called like: nohup /home/yyc/.git_scripts/utils/update_tags.sh >/dev/null 2>&1 &
#
global -uv || gtags -v
