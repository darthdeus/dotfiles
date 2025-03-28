import os
import subprocess
import argparse

# Parse command-line arguments
parser = argparse.ArgumentParser(description='Convert a video to MP4 with specified aspect ratio.')
parser.add_argument('input_file', help='Path to the input video file')
parser.add_argument('--portrait', dest='portrait', action='store_true', help='Set aspect ratio to 9:16. Default is 16:9.')
parser.set_defaults(portrait=False)
args = parser.parse_args()

# Determine aspect ratio
aspect_ratio = "1920x1080" if not args.portrait else "1080x1920"

# Define output directory relative to the input file
input_dir = os.path.dirname(os.path.realpath(args.input_file))
outputs_dir = os.path.join(input_dir, "outputs")
os.makedirs(outputs_dir, exist_ok=True)

# Extract filename and define output path
base_filename = os.path.splitext(os.path.basename(args.input_file))[0]
output_file_path = os.path.join(outputs_dir, base_filename + ".mp4")

# Convert the file
print(f"Converting {args.input_file} -> {output_file_path}")
subprocess.run([
    "ffmpeg", "-i", args.input_file, "-vcodec", "libx264", "-acodec", "aac",
    "-s", aspect_ratio, "-profile:v", "baseline", "-strict", "-2", "-f", "mp4", "-crf", "30",
    output_file_path
])

# Open the output directory in Thunar
subprocess.run(["thunar", outputs_dir])

