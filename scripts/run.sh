#! /bin/bash

set -e

mrs=$(curl --silent -X GET "https://gitlab.com/api/v4/projects/3836952/merge_requests?state=opened")

i=0

check_mr() {
	ometrics check-clone $1 \
			   --html \
			   --clickable \
			   --exclude-re "test_*" \
			   --exclude "tezt/" \
			   --branch "$2" \
			   --title "$3"	\
			   --output "$5"
}

for i in {0..99}; do
	mr=$(echo $mrs | jq ".[${i}]")
	id=$(echo $mr | jq '.id')
	source_branch=$(echo $mr | jq '.source_branch')
	trimmed_source=$(echo $source_branch | sed -r 's/[\"]//g')
	source_project=$(echo $mr | jq '.source_project_id')
	http_url_to_project=$(curl --silent -X GET "https://gitlab.com/api/v4/projects/${source_project}" | jq '.http_url_to_repo')
	title=$(echo $mr | jq '.title')

	mkdir -p $1
	output="$1/$id.html"

	check_mr $http_url_to_project $source_branch "${title}" "$id" $output
done
