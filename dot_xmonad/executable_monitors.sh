#!/bin/bash

LAPTOP='LVDS-1'
EXTERNAL='DP-2'

if [ `xrandr | grep -c "${EXTERNAL} disconnected"` -gt 0 ]; then
	xrandr --output ${EXTERNAL} --off
	xrandr --output ${LAPTOP} --auto --primary
else
	xrandr --output ${EXTERNAL} --auto --primary
	xrandr --output ${LAPTOP} --off
fi

