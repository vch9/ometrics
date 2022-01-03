#! /bin/bash

set -e

mrs=$(curl -X GET "https://gitlab.com/api/v4/projects/3836952/merge_requests?state=all")

i=0

check_mr() {
	if [ -z "$1" ]; then
		echo "Missing git repository, example:"
		echo "$0 https://gitlab.com/vch9/ometrics-test.git"
		exit 1
	fi

	if [ ! -z "$2" ]; then
		branch_option=" --branch $2 "
	fi

	if [ ! -z "$3" ]; then
		hash_option=" --hash $3 "
	fi

	echo "Running: ometrics check-clone $1 $branch_option $hash_option"
	ometrics check-clone "$1 $branch_option $hash_option"
}

while true; do
	mr=$(echo $mrs | jq ".[${i}]")
	source_branch=$(echo $mr | jq '.source_branch')
	trimmed_source=$(echo $source_branch | sed -r 's/[\"]//g')
	log_file="$trimmed_source.log"
	check_mr "https://gitlab.com/nomadic-labs/tezos.git" $source_branch > $log_file 2>&1

	i=$i+1
done
