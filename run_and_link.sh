# Script takes in tiger file and returns assembly
# code ready to run in SPIM
if (($# != 1)); then
	echo "Usage: $0 <path/to/filename.tig>"
	exit 1
fi

file_tig=$1
runtime=runtime.s

echo "Running Stack"
stack build --silent

echo "Running Compiler"
stack run -- ${file_tig} -f

echo "Appending to Runtime"
file_noext=${file_tig%.tig}
file_s=${file_noext}.s
cat runtime.s $file_s > out.s