ndays=$(ls AOC_2025/tests/test*.fs | wc -l)
echo $ndays

cp AOC_2025/tests/test0.fs AOC_2025/tests/test$ndays.fs
sed -i -e "s/Day0/Day${ndays}/g" AOC_2025/tests/test$ndays.fs
sed -i "/<Compile Include=\"Program.fs\" \/>/i\ \ \ \ <Compile Include=\"test${ndays}.fs\" \/>" AOC_2025/tests/tests.fsproj

cp AOC_2025/src/aoc0.fs AOC_2025/src/aoc$ndays.fs
sed -i -e "s/Day0/Day${ndays}/g" AOC_2025/src/aoc$ndays.fs
sed -i "/<Compile Include=\"aoc0.fs\" \/>/i\ \ \ \ <Compile Include=\"aoc${ndays}.fs\" \/>" AOC_2025/src/src.fsproj

touch AOC_2025/data/aoc${ndays}_input.txt

