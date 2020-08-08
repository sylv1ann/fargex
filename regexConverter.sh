#!/bin/bash


echo "$1"  | \
tr '+' '|' | \
tr -d '$'  | \
sed 's#(\([a-z0-9A-Z]\{0,\}\))#\1#g'
