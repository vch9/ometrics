## 0.1.3

### Fixes

- Read channels before waiting for process (!12 @shrmtv)
- Simplify [base_entry_name] with [split_on_char]. [Filename.extension] could
  fail if the extension contains unexpected chars (e.g. '/').

## 0.1.2

### Fixes

- Add fingerprints in GitLab format

## 0.1.1

### Fixes

- Fix duplicates in files (#17)
- Fix guard in String.sub
- Fix writting to files

### Removed

- ometrics library is no longer public

## 0.1.0

### Added

- Documentation analysis in a merge request
