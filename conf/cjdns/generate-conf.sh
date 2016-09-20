#!/bin/sh

set -eu

cjdroute --genconf | cjdroute --cleanconf >$(dirname $0)/cjdroute.conf
