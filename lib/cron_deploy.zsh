#!/usr/bin/env zsh

source "$HOME/.zsh.custom/zsh/after/generic/00_env_secret.zsh"

working_dir="/home/kaspi/Projects/tryouts_learn/swiss_wastewater_covid_virus_load"

cd "$working_dir"

echo -e "\n\nBuild page by cronjob $(date --iso-8601=seconds)\n\n" >> "$working_dir/logging.log"

Rscript -e "library(rmarkdown); rmarkdown::render('wastewater.Rmd', output_file = 'docs/index.html', params = list(generate_date = '$(date --iso-8601=seconds)'))" >> "$working_dir/logging.log" 2>&1

git status >> "$working_dir/logging.log" 2>&1

git add --all >> "$working_dir/logging.log" 2>&1
git commit -m "$(date +%y%m%d-%H%M%S)" >> "$working_dir/logging.log" 2>&1
git push >> "$working_dir/logging.log" 2>&1
