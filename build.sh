build_type="$1"

case "$1" in
    "debug");;
    "release");;

    *)
    echo "Specify debug or release for first argument."
    exit 1
    ;;
esac

cmake -S . -B builds/"$build_type"\
    -DCMAKE_BUILD_TYPE="${build_type^}"\
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++

cmake --build builds/"$build_type" --parallel
if [[ $? -ne 0 ]]; then
    echo "Failed to build." >& 2
    exit 1
fi

cd example
exec_path="../builds/"$build_type"/lom"
lflags="--link-flags -lraylib"

./"$exec_path" "${@:2}" --link-flags -lraylib -lm
