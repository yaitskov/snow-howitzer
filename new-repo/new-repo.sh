#!/bin/bash

curl -v -L \
     -X POST \
     -H "Accept: application/vnd.github+json" \
     -H "Authorization: Bearer $GITAPPTOKEN" \
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/user/repos \
     -d '{"name":"'${NEWREPO:-programmatically-created}'", "description": "test", "homepage": "http://github.com/yaitskov", "private": false, "is_template": false }'
