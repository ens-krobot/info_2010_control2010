#!/bin/bash
#
# install-programs.sh
# -------------------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of [kro]bot.

if [ -z "$1" ]; then
    PREFIX="$HOME"
else
    PREFIX="$1"
fi

PROGRAMS=$(echo _build/{services,driver,clients,tools,usb-tools,simulator}/{*.best,**/*.best})

for prog in $PROGRAMS; do
    if [ -f $prog ]; then
        name=$(basename $prog)
        name=${name//_/-}
        name=${name/.best}
        install -vm 0755 $prog "$PREFIX/bin/$name"
    fi
done

mkdir -pv "$PREFIX/share/krobot/services"

install -vm 0644 utils/bash_completion "$PREFIX/share/krobot/bash_completion"

function instantiate {
    local destination="$PREFIX/share/krobot/$2"
    echo "Creation of $destination"
    sed "s#@PREFIX@#$PREFIX#g" "$1" > "$destination"
}

instantiate dbus-conf/bus.conf.in bus.conf
for service in dbus-conf/services/*.service.in; do
    instantiate $service services/$(basename ${service/.in})
done
