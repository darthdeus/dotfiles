#!/usr/bin/env python3
import os
import argparse

def load_ignored_patterns(directory, ignore_filenames):
    ignored_patterns = set()
    for ignore_filename in ignore_filenames:
        ignore_path = os.path.join(directory, ignore_filename)
        if os.path.exists(ignore_path):
            with open(ignore_path, "r", encoding="utf-8", errors="ignore") as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith("#"):  # Ignore comments and empty lines
                        ignored_patterns.add(line)
    return ignored_patterns

def is_ignored(filepath, ignored_patterns):
    return any(pattern in filepath for pattern in ignored_patterns)

def is_test_file(filepath):
    filename = os.path.basename(filepath)
    return "tests/" in filepath or filename.startswith("test_") or filename.endswith("_test.rs")

def get_rust_files(directory, include_tests=False):
    ignored_patterns = load_ignored_patterns(directory, [".promptignore", ".gitignore", ".ignore"])
    rust_files = []
    test_files = []

    for root, _, files in os.walk(directory):
        for file in files:
            # if file.endswith(".rs") or file.endswith("prelude.lua"):
            if file.endswith(".rs"):
                filepath = os.path.join(root, file)
                if is_ignored(filepath, ignored_patterns):
                    continue
                if is_test_file(filepath):
                    test_files.append(filepath)
                else:
                    rust_files.append(filepath)

    rust_files.sort()
    test_files.sort()

    if include_tests:
        return rust_files + test_files
    return rust_files

def cat_rust_files(directory, paths_only=False, show_sizes=False, include_tests=False):
    rust_files = get_rust_files(directory, include_tests=include_tests)

    if paths_only or show_sizes:
        files_with_sizes = [(fp, sum(1 for _ in open(fp, "r", encoding="utf-8", errors="ignore"))) for fp in rust_files]
        files_with_sizes.sort(key=lambda x: x[1], reverse=True)  # Sort by line count

        for filepath, size in files_with_sizes:
            if show_sizes:
                print(f"{size:5d} {filepath}")
            else:
                print(filepath)
    else:
        with open("/dev/stdout", "w", encoding="utf-8", errors="ignore") as output:
            for filepath in rust_files:
                with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                    output.write(f.read())
                    output.write("\n")  # Separate files with a newline

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--paths", action="store_true", help="Only print file paths, sorted by name")
    parser.add_argument("--sizes", action="store_true", help="Print file sizes (line count) and sort by size")
    parser.add_argument("--include-tests", action="store_true", help="Include test files in output")
    args = parser.parse_args()

    repo_path = '.'  # Change this if needed
    cat_rust_files(repo_path, paths_only=args.paths, show_sizes=args.sizes, include_tests=args.include_tests)

