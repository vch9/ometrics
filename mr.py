#!/bin/env python3

# usage:
#   mr.py

import subprocess
import os

before = subprocess.Popen(
    "git --no-pager log --merges -n1 --pretty=format:%H",
    shell=True,
    stdout=subprocess.PIPE
).stdout.read().decode("utf-8")

ometrics = "./main.exe"

ml_files = {}

def read_undocumented_entries(file):
    return subprocess.Popen(
        f"{ometrics} {file}",
        shell=True,
        stdout=subprocess.PIPE
    ).stdout.read().decode("utf-8").splitlines()

modified_files = subprocess.Popen(
    f"git diff {before} --name-only | egrep '*.mli?$'",
    shell=True,
    stdout=subprocess.PIPE
).stdout.read().decode("utf-8").splitlines()

for file in modified_files:
    if os.path.isfile(file):
        # For a mli, the ml file may have been treated already
        if not file[:-1] in ml_files:
            ml_files[file] = read_undocumented_entries(file)

subprocess.call(f"git checkout -q {before}", shell=True)

for file in modified_files:
    if os.path.isfile(file) and not file[:-1] in ml_files:
        undocumented_entries_after = ml_files[file]
        undocumented_entries_before = read_undocumented_entries(file)
        ib, ia = 0, 0
        print_filename = True

        undocumented_entries_number = len(undocumented_entries_after)
        undocumented_entries_number_before = len(undocumented_entries_before)

        while ia < undocumented_entries_number:
            if ib < undocumented_entries_number_before and undocumented_entries_after[ia] == undocumented_entries_before[ib]:
                ia += 1
                ib += 1
                continue
            elif undocumented_entries_number_before <= ib or undocumented_entries_after[ia] < undocumented_entries_before[ib]:
                if print_filename:
                    print(f"# `{file}`")
                    print_filename = False

                print(f"- `{undocumented_entries_after[ia]}`")
                ia += 1
                continue
            else:
                ib += 1
                continue

    else:
        if not file[:-1] in ml_files:
            print(f"# `{file}`")
            for e in ml_files[file]:
                print(f"- `{e}`")

subprocess.call("git checkout -q -", shell=True)
