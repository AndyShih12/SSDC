function usage {
    cat <<EOF
Usage: $0 UAI
This script takes in a single uai file as a starting point, and then runs a couple of programs to generate the required compiled output for our online collapsed importance sampler.
EOF
    exit 1
}

if [[ $# -ne 1 ]]; then
    usage
fi

base=$(basename $1 .uai)
echo $base
echo $base"_processed"
mkdir -pv ./$base"_processed"
cp $1 ./$base"_processed"
cd ./$base"_processed"

java -jar ./jython-standalone-2.7.0.jar ./order/order.py $base".uai"

mkdir -pv $base"sdd"
cd $base"sdd"
./bn -u ../$base".uai" -o ../orders/$base".order" -W ../$base".vtree" -Y ../$base".litmap" -b ../$base".base.cnf" -z
