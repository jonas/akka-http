#!/bin/sh

find . -name "*.scala" -o -name "*.java" -o -name "*.conf" -o -name "*.md" | xargs sed -i 's/[[:space:]]\+$//'
