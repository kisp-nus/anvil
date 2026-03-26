#default
# ANVIL_FILE := ./should_pass/test1/example1_anvil_sender.anvil
# SV_ANVIL   := ./should_pass/test1/example1_anvil_sender.anvil.sv
# SV_USER    := ./should_pass/test1/example1_user_receiver.sv
# OUTPUT     := ./should_pass/test1/example1_anvil_sender_assert.sv
# TOP_MODULE := example1_anvil_sender_assert

ANVIL_FILE ?=
SV_ANVIL   ?=
SV_USER    ?=
OUTPUT     ?=
TOP_MODULE ?=
MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PROPERTIES_DIR ?= $(abspath $(MAKEFILE_DIR)/../../properties)
SIM_OUT ?= sim.out

.PHONY: assert run build clean check-vars all

all: assert run build

check-vars:
	@test -n "$(ANVIL_FILE)" || (echo "ANVIL_FILE is not set" && exit 1)
	@test -n "$(SV_ANVIL)"   || (echo "SV_ANVIL is not set" && exit 1)
	@test -n "$(SV_USER)"    || (echo "SV_USER is not set" && exit 1)
	@test -n "$(OUTPUT)"     || (echo "OUTPUT is not set" && exit 1)
	@test -n "$(TOP_MODULE)" || (echo "TOP_MODULE is not set" && exit 1)

assert: check-vars
	dune exec anvil -- assert $(ANVIL_FILE) $(SV_ANVIL) $(SV_USER) > $(OUTPUT)

run: check-vars
	verilator -Wall --binary --exe --assert --trace --build \
	-I$(PROPERTIES_DIR) --timing \
	--top-module $(TOP_MODULE) \
	$(SV_USER) $(SV_ANVIL) $(OUTPUT) \
	-o $(SIM_OUT) \
	-Wno-UNDRIVEN -Wno-UNUSED

build:
	./obj_dir/$(SIM_OUT)

clean:
	rm -rf obj_dir *.vcd *.fst *.log