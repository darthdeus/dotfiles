#!/usr/bin/env python3

import os

def load_ignored_patterns(directory):
    ignored_patterns = set()
    ignore_files = [".gitignore", ".ignore"]
    
    for ignore_file in ignore_files:
        ignore_path = os.path.join(directory, ignore_file)
        if os.path.exists(ignore_path):
            with open(ignore_path, "r", encoding="utf-8", errors="ignore") as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith("#"):  # Ignore comments and empty lines
                        ignored_patterns.add(line)
    
    return ignored_patterns

def is_ignored(filepath, ignored_patterns):
    for pattern in ignored_patterns:
        if pattern in filepath:
            return True
    return False

def analyze_rust_files(directory, num_files=10):
    rust_files = []
    ignored_patterns = load_ignored_patterns(directory)
    
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".rs"):
                filepath = os.path.join(root, file)
                if is_ignored(filepath, ignored_patterns):
                    continue
                try:
                    size = os.path.getsize(filepath)
                    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                        lines = f.readlines()
                        total_lines = len(lines)
                        comment_lines = sum(1 for line in lines if line.strip().startswith("//"))
                    rust_files.append((filepath, total_lines, comment_lines, size))
                except Exception as e:
                    print(f"Error processing {filepath}: {e}")
    
    rust_files.sort(key=lambda x: x[3], reverse=True)  # Sort by file size
    return rust_files[:num_files]

if __name__ == "__main__":
    repo_path = '.'  # Change this to your Rust project directory if needed
    largest_rust_files = analyze_rust_files(repo_path)
    
    print("Largest Rust files in the repository:")
    print(f"{'File Path':<50} {'Lines':>10} {'Comments':>10} {'Size (MB)':>10}")
    print("-" * 85)
    for filepath, total_lines, comment_lines, size in largest_rust_files:
        print(f"{filepath:<50} {total_lines:>10} {comment_lines:>10} {size / (1024 * 1024):>10.2f}")

