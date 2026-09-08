MODULE_NAME ?= top

# accept an .anvil file from anywhere: make modelcheck FILE=path/to/x.anvil
# (module stem derived from the file name; outputs land in this directory)
ANVIL_SRC = $(MODULE_NAME).anvil
ifdef FILE
MODULE_NAME := $(notdir $(basename $(FILE)))
ANVIL_SRC := $(FILE)
endif

TOP_MODULE ?= $(MODULE_NAME)

ifdef TRACE_ON
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1 --trace --trace-structs
else
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1
endif

TEST_DRIVER = sim_main.cpp

all: obj_dir/V$(MODULE_NAME)

# model checking (currently backed by EBMC; the target name stays tool-neutral)
MC_BOUND ?= 20
MC_FLAGS ?=

.PHONY: run clean modelcheck

modelcheck: $(MODULE_NAME).anvil.sv
	ebmc $< --top $(TOP_MODULE) --bound $(MC_BOUND) \
	     --reset '$(TOP_MODULE).rst_ni==0' $(MC_FLAGS)

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

$(MODULE_NAME).anvil.sv: $(ANVIL_SRC)
	dune exec anvil $(ANVIL_FLAGS) $< > $@

obj_dir/V$(MODULE_NAME): $(MODULE_NAME)_driver.cpp $(MODULE_NAME).anvil.sv
	verilator --cc --exe --build $(VERILATOR_FLAGS) $^

$(MODULE_NAME)_driver.cpp: $(TEST_DRIVER)
	sed "s/Vtop/V$(MODULE_NAME)/g" $< > $@

run: obj_dir/V$(MODULE_NAME)
	@ $< $(TIMEOUT)

clean:
	rm -rf *obj_dir *.anvil.sv *_driver.cpp *.error

help:
	@echo "Makefile targets and options:"
	@echo ""
	@echo "  MODULE_NAME=foo   Specify the top module name (default: top)"
	@echo "  clean             Clean generated files"
	@echo "  run               Build and run the simulation for MODULE_NAME"
	@echo "  modelcheck        Compile and model-check MODULE_NAME (MC_BOUND=20, MC_FLAGS=)"
	@echo ""
	@echo "  VERBOSE=1         Enable verbose output during compilation"
	@echo "  TRACE_ON=1        Enable waveform tracing in simulation"
	@echo "  LTOFF=1           Disable lifetime checks during Anvil compilation"
	@echo "  CHECK=1           Only type-check Anvil code (no SV generation)"
