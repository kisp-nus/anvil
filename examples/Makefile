MODULE_NAME ?= top
TOP_MODULE ?= $(MODULE_NAME)
USER_SV_MODULE ?= $(MODULE_NAME)

ifdef TRACE_ON
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1 --trace --trace-structs
else
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1
endif

TEST_DRIVER = sim_main.cpp

all: obj_dir/V$(MODULE_NAME)

.PHONY: run clean
.PHONY: run clean assert

ANVIL_FLAGS = -- 

ifdef LTOFF
    ANVIL_FLAGS += -disable-lt-checks
endif

ifdef CHECK
	ANVIL_FLAGS += -just-check
endif

ifdef VERBOSE
	ANVIL_FLAGS += -verbose
endif

$(MODULE_NAME).anvil.sv: $(MODULE_NAME).anvil
	dune exec anvil $(ANVIL_FLAGS) $< > $@

obj_dir/V$(MODULE_NAME): $(MODULE_NAME)_driver.cpp $(MODULE_NAME).anvil.sv
	verilator --cc --exe --build $(VERILATOR_FLAGS) $^

$(MODULE_NAME)_driver.cpp: $(TEST_DRIVER)
	sed "s/Vtop/V$(MODULE_NAME)/g" $< > $@

run: obj_dir/V$(MODULE_NAME)
	@ $< $(TIMEOUT)

clean:
	rm -rf *obj_dir *.anvil.sv *_driver.cpp *.error

MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PROPERTIES_DIR ?= $(abspath $(MAKEFILE_DIR)/../extern_properties)
assert:
	dune exec anvil -- -sv-extern extern $(MODULE_NAME).anvil $(USER_SV_MODULE).sv > $(USER_SV_MODULE)_assert.sv 

	verilator -Wall --binary --exe --assert --trace --build \
	-I$(PROPERTIES_DIR) --timing \
	--top-module $(USER_SV_MODULE)_assert \
	$(USER_SV_MODULE).sv $(MODULE_NAME).anvil.sv $(USER_SV_MODULE)_assert.sv \
	-o sim.out \
	-Wno-UNDRIVEN -Wno-UNUSED

	./obj_dir/sim.out > $(MODULE_NAME).test

help:
	@echo "Makefile targets and options:"
	@echo ""
	@echo "  MODULE_NAME=foo		Specify the top module name (default: top)"
	@echo "	 USER_SV_MODULE=foo 	Specify the external user SV module used for assertion flow"
	@echo "  clean             		Clean generated files"
	@echo "  run               		Build and run the simulation for MODULE_NAME"
	@echo "	 assert            		Generate assertion wrapper and run the simulation for MODULE_NAME and USER_SV_MODULE"
	@echo ""
	@echo "  VERBOSE=1         		Enable verbose output during compilation"
	@echo "  TRACE_ON=1        		Enable waveform tracing in simulation"
	@echo "  LTOFF=1           		Disable lifetime checks during Anvil compilation"
	@echo "  CHECK=1           		Only type-check Anvil code (no SV generation)"
