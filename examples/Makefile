MODULE_NAME ?= top
TOP_MODULE ?= $(MODULE_NAME)
SV_EXTERN_VERBOSE := $(if $(V),-v,)

ifdef TRACE_ON
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1 --trace --trace-structs
else
	VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1
endif

TEST_DRIVER = sim_main.cpp

all: obj_dir/V$(MODULE_NAME)

.PHONY: run clean sv_extern sv_extern_clean

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

sv_extern:
	$(MAKE) $(MODULE_NAME).anvil.sv MODULE_NAME=$(MODULE_NAME)
	dune exec anvil -- -sv-extern $(MODULE_NAME).anvil > $(MODULE_NAME)_ultimate_wrapper.sv
	python3 extern_generate_filelist.py $(MODULE_NAME)_ultimate_wrapper.sv $(SV_EXTERN_VERBOSE)
	verilator -Wall --binary --exe --assert --trace --build -f $(MODULE_NAME)_ultimate_wrapper_filelist.f --timing --top-module $(MODULE_NAME)_ultimate_wrapper -o sim.out -Wno-UNDRIVEN -Wno-UNUSED -Wno-DECLFILENAME
	./obj_dir/sim.out +verilator+quiet > $(MODULE_NAME).test 2>&1

sv_extern_clean:
	rm -f $(MODULE_NAME)_ultimate_wrapper.sv
	rm -f $(MODULE_NAME)_ultimate_wrapper_filelist.f
	rm -rf obj_dir

help:
	@echo "Makefile targets and options:"
	@echo ""
	@echo "  MODULE_NAME=foo   Specify the top module name (default: top)"
	@echo "  clean             Clean generated files"
	@echo "  run               Build and run the simulation for MODULE_NAME"
	@echo ""
	@echo "	 SV extern verification:"
	@echo "  sv_extern         Generate Anvil SV, generate the ultimate wrapper and filelist,"
	@echo "                    build the SV extern verification with Verilator, and run sim.out"
	@echo "  sv_extern_clean   Remove MODULE_NAME.anvil.sv, ultimate wrapper, filelist, and obj_dir"
	@echo "  VERBOSE=1         Enable verbose output during compilation"
	@echo "  TRACE_ON=1        Enable waveform tracing in simulation"
	@echo "  LTOFF=1           Disable lifetime checks during Anvil compilation"
	@echo "  CHECK=1           Only type-check Anvil code (no SV generation)"