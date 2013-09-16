#!/bin/bash

ssh_keyfile=~/.ssh/id_rsa_test
if [ ! -e ${ssh_keyfile}.pub ]; then
  ssh-keygen -t rsa -f ${ssh_keyfile}
fi

echo "Enter a name for this github ssh key: "
read title
key=$(cat ${ssh_keyfile}.pub)

read -r -d '' body <<EOF
{
  "title" : "${title}",
  "key" : "${key}"
}
EOF

curl -XPOST -u "pheaver" https://api.github.com/user/keys -d "${body}"
