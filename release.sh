#!/bin/sh

# Usage
# sh release.sh [version number]

srgb_dat=sRGB-D65.dat
adobergb_dat=AdobeRGB-d65.dat
suffix=

if [ ! -e ${srgb_dat} ] || [ ! -e ${adobergb_dat} ]; then
	sbcl --dynamic-space-size 2048 --load gen-datas.lisp --quit
fi

if [ $# -ge 1 ]; then
    suffix=-$1
fi

mvn clean package

# Make .exe for Windows
launch4jc Munsellpicker.jar.xml

zip -jr Munsellpicker${suffix}-bin.zip *.exe target/Munsellpicker.jar *.md *.dat
