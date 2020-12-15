ndays=$(ls AOC_2020/tests/Test*.fs | wc -l)
echo $ndays

cp AOC_2020/tests/Test0.fs AOC_2020/tests/Test$ndays.fs
sed -i -e "s/0/${ndays}/g" AOC_2020/tests/Test$ndays.fs
sed -i "/<Compile Include=\"Program.fs\" \/>/i\ \ \ \ <Compile Include=\"Test${ndays}.fs\" \/>" AOC_2020/tests/tests.fsproj

cp AOC_2020/src/aoc0.fs AOC_2020/src/aoc$ndays.fs
sed -i -e "s/0/${ndays}/g" AOC_2020/src/aoc$ndays.fs
sed -i "/<None Include=\"aoc0.fs\" \/>/i\ \ \ \ <Compile Include=\"aoc${ndays}.fs\" \/>" AOC_2020/src/AdventOfCode.fsproj

touch AOC_2020/data/aoc${ndays}_input.txt

