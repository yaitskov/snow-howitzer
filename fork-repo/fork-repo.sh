#!/bin/bash

curl -v -L \
     -X POST \
     -H "Accept: application/vnd.github+json" \
     -H "Authorization: Bearer $GITAPPTOKEN" \
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/repos/shutterrecoil/forkmenow/forks \
     -d '{"name":"forkmenow", "default_branch_only": true}'
