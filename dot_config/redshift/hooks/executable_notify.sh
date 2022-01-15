#!/bin/sh
case $1 in
	period-changed)
		exec notify-send -u low -t 5000 -i redshift "Redshift" "Changed to: $3"
esac

