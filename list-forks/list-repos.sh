#!/bin/bash

curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer $GITAPPTOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  "https://api.github.com/user/repos?page=${PAGE:-1}&per_page=100&type=forks"
