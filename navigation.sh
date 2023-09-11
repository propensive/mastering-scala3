#!/bin/bash

function forward() {
  git log --reverse --pretty=%H main | grep -A 1 "$(git rev-parse HEAD)" | tail -n1 | xargs git checkout
}

function nextCommit() {
  git log --reverse --pretty=%H main | grep -A 1 "$(git rev-parse HEAD)" | tail -n1
}

function back() {
  git checkout "HEAD^1"
}
