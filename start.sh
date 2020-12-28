#!/bin/bash

cp -r ./template ./day$1
curl --cookie cookies.txt https://adventofcode.com/2020/day/$1/input -o ./day$1/input.txt
