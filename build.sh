#!/bin/bash
csc -s etc.scm -j etc
csc -s format.scm -j format
csc -static main.scm -o scheme-format
