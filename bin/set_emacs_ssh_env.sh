#!/bin/sh

emacsclient -e "(setenv \"SSH_AGENT_PID\" \"$SSH_AGENT_PID\")"
emacsclient -e "(setenv \"SSH_AUTH_SOCK\" \"$SSH_AUTH_SOCK\")"
