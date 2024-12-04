ndays=$(ls tests/test*.fs | wc -l)
echo $ndays

cp tests/test0.fs tests/test$ndays.fs
sed -i -e "s/Day0/Day${ndays}/g" tests/test$ndays.fs
sed -i "/<Compile Include=\"Program.fs\" \/>/i\ \ \ \ <Compile Include=\"test${ndays}.fs\" \/>" tests/tests.fsproj

cp src/aoc0.fs src/aoc$ndays.fs
sed -i -e "s/Day0/Day${ndays}/g" src/aoc$ndays.fs
sed -i "/<Compile Include=\"aoc0.fs\" \/>/i\ \ \ \ <Compile Include=\"aoc${ndays}.fs\" \/>" src/src.fsproj

touch data/aoc${ndays}_input.txt

