#!/usr/bin/env bash

# do not stop even the compiler fail
set +e

ROOT_DIR="${1:-.}"

# counters
should_pass_total=0
should_pass_ok=0
should_fail_total=0
should_fail_ok=0

run_one_test() {
    local test_dir="$1" # testcase folder (test1, test2 ...)
    local expect="$2"   # whether this case should pass or fail

    local anvil_file
    local sv_anvil
    local output
    local sv_user
    local top_module

    # anvil_file: search for .anvil
    anvil_file=$(find "$test_dir" -maxdepth 1 -type f -name "*.anvil" | head -n 1)
    # sv_anvil: search for .anvil.sv
    sv_anvil=$(find "$test_dir" -maxdepth 1 -type f -name "*.anvil.sv" | head -n 1)
    # sv_user: find user file (.sv) but exclude (_assert.sv) and (.anvil.sv)
    sv_user=$(find "$test_dir" -maxdepth 1 -type f -name "*.sv" \
        ! -name "*.anvil.sv" \
        ! -name "*_assert.sv" | head -n 1)
    # checks whether any important file is missing
    if [ -z "$anvil_file" ] || [ -z "$sv_anvil" ] || [ -z "$sv_user" ]; then
        echo "####### Test Invalid: $test_dir"
        echo "        Missing one of: *.anvil, *.anvil.sv, user *.sv"
        return 2
    fi

    # output: assertion wrapper
    output="${sv_user%.sv}_assert.sv"

    # Derive the top module name from the output file name
    top_module=$(basename "$output" .sv)

    echo "----------------------------------------"
    echo "Test dir   : $test_dir"
    echo "Expectation: $expect"
    echo "ANVIL_FILE : $anvil_file"
    echo "SV_ANVIL   : $sv_anvil"
    echo "SV_USER    : $sv_user"
    echo "OUTPUT     : $output"
    echo "TOP_MODULE : $top_module"

    # Clean previous build files
    make clean > /dev/null 2>&1

    # Generate the assertion wrapper
    make assert \
        ANVIL_FILE="$anvil_file" \
        SV_ANVIL="$sv_anvil" \
        SV_USER="$sv_user" \
        OUTPUT="$output" \
        TOP_MODULE="$top_module" \
        > /dev/null 2>&1
    assert_status=$?
    if [ $assert_status -ne 0 ]; then
        echo "####### Test Failed: $test_dir"
        echo "        make assert failed"
        return 1
    fi

    # Compile with Verilator
    make run \
        ANVIL_FILE="$anvil_file" \
        SV_ANVIL="$sv_anvil" \
        SV_USER="$sv_user" \
        OUTPUT="$output" \
        TOP_MODULE="$top_module" \
        > /dev/null 2>&1
    compile_status=$?
    if [ $compile_status -ne 0 ]; then
        echo "####### Test Failed: $test_dir"
        echo "        Verilator build failed"
        return 1
    fi

    # Run simulation
    make build > /dev/null 2>&1
    sim_status=$?
    if [ "$expect" = "pass" ]; then
        if [ $sim_status -eq 0 ]; then
            echo "####### Test Passed: $test_dir"
            return 0
        else
            echo "####### Test Failed: $test_dir"
            echo "        Simulation failed unexpectedly"
            return 1
        fi
    else
        if [ $sim_status -ne 0 ]; then
            echo "####### Test Passed: $test_dir"
            echo "        Assertion failure occurred as expected"
            return 0
        else
            echo "####### Test Failed: $test_dir"
            echo "        Simulation succeeded unexpectedly"
            return 1
        fi
    fi
}

# Run all should_pass tests
if [ -d "$ROOT_DIR/should_pass" ]; then
    for d in "$ROOT_DIR"/should_pass/*; do
        [ -d "$d" ] || continue
        should_pass_total=$((should_pass_total + 1))
        run_one_test "$d" "pass"
        [ $? -eq 0 ] && should_pass_ok=$((should_pass_ok + 1))
    done
fi

# Run all should_fail tests
if [ -d "$ROOT_DIR/should_fail" ]; then
    for d in "$ROOT_DIR"/should_fail/*; do
        [ -d "$d" ] || continue
        should_fail_total=$((should_fail_total + 1))
        run_one_test "$d" "fail"
        [ $? -eq 0 ] && should_fail_ok=$((should_fail_ok + 1))
    done
fi

echo "========================================"
echo "######### should_pass = $should_pass_ok/$should_pass_total"
echo "######### should_fail = $should_fail_ok/$should_fail_total"