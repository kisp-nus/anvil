# Anvil SystemVerilog Extern Verification Integration

## Overview

This integration adds an automated SystemVerilog verification flow for Anvil designs that connect Anvil processes with external SystemVerilog modules.

The integration automatically generates:
- `dut_wrapper_<top_module>`
- `<top_module>_ultimate_wrapper`
- control and data interface connections
- `tb1`, `tb2`, `tb3`, or `tb4` instances
- a Verilator filelist
- a `.test` simulation output file

Design Handling:
- **Spawn handling:** Only processes actually referenced by `spawn` are instantiated in the generated DUT wrapper.
- **Channel handling:** Top-level `chan` declarations are used to determine which spawned processes are connected through each endpoint.
- **Message handling:** Every message in an active channel is checked and mapped to the appropriate verification testbench (`tb1`–`tb4`) based on its synchronization type.
- **Interface handling:** Shared control and data interfaces are generated so both sides of a channel connect to the same verification signals.
- **Unused logic:** Processes that are not spawned and channels that are not used are ignored.


## Relevant Files
anvil_verification_clean/
│
├── extern_properties/
│   ├── interface_control.sv
│   ├── interface_data.sv
│   ├── tb1.sv
│   ├── tb2.sv
│   ├── tb3.sv
│   └── tb4.sv
│
├── examples/
│   ├── Makefile
│   ├── extern_generate_filelist.py
│   │
│   ├── extern_dyn_dyn.anvil
│   ├── extern_axi_cpu_master.sv
│   │
│   ├── extern_dyn_dyn_invalid_valid.anvil
│   └── extern_axi_cpu_master_invalid_valid.sv
│
└── externCodegen.ml


## Verification Structure
```text
                <top_module>_ultimate_wrapper
                ┌────────────────────────────┐
                │                            │
                │  Clock / Reset             │
                │                            │
                │  Control Interface         │
                │  Data Interface            │
                │          │                 │
                │          ▼                 │
                │  dut_wrapper_<top_module>  │
                │  ┌──────────────────────┐  │
                │  │                      │  │
                │  │ Anvil-generated DUT  │  │  ← Anvil process
                │  │                      │  │
                │  │        ↕             │  │
                │  │ valid / ack / data   │  │
                │  │        ↕             │  │
                │  │ Anvil extern wrapper │  │  ← SystemVerilog extern process
                │  │                      │  │
                │  └──────────────────────┘  │
                │          │                 │
                │          ▼                 │
                │    tb1 / tb2 / tb3 / tb4   │
                │                            │
                └────────────────────────────┘
```


## Requirements
## Container Requirement
The `.anvil` file must contain a **top-level container process** that defines the channel connections and spawns the processes to be verified.

Supported Example:

```anvil
proc extern_dyn_dyn() {
  chan ep_le -- ep_ri : axi_cpu_req_ch<logic[31]>;

  spawn axi_cpu_master_ext(ep_ri);      // from User SystemVerilog File
  spawn memory_controller_dut(ep_le);   // from Anvil File

  loop {
    cycle 1
  }
}

Unsupported Example:
```anvil
proc memory_controller_dut() {
  chan ep_le -- ep_ri : axi_cpu_req_ch<logic[31]>;

  spawn axi_cpu_master_ext(ep_ri);

  ...
}
```

## Assertions
### TB1: Dynamic / Dynamic Verification
#### Valid must be held before handshake
If `valid` has been asserted but `ack` has not yet arrived, the sender must continue asserting `valid`:

```systemverilog
property holds_valid_before_handshake;
    @(posedge clk_i) disable iff (!rst_assert_ni)
    (state_curr == WAIT_ACK) |-> tb1_control_if.valid;
endproperty
```
#### Data must remain stable
While a transaction is waiting for acknowledgement or is still within its lifetime, the data must remain unchanged:

```systemverilog
property holds_data_stable;
    @(posedge clk_i) disable iff (!rst_assert_ni)
    (state_curr == WAIT_ACK || state_curr == HOLD_DATA)
    |-> tb1_data_if.data == tracked_data;
endproperty
```

---
### TB2: Static / Dynamic Verification
#### Data must remain stable
Since the sender operates at a static interval while the receiver acknowledges dynamically, the data must remain stable while waiting for the first handshake, during the static interval, or while the previous data is still within its lifetime:

```systemverilog
property holds_data_stable_syn;
    @(posedge clk_i) disable iff (!rst_assert_ni)
    (state_curr == HOLD_DATA_BEFORE_FIRST_HANSHAKE ||
     state_curr == HOLD_DATA_STATIC_INTERVAL ||
     (state_curr == HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL &&
      !(static_remaining == 0 &&
        lifetime_remaining == 0 &&
        tb2_control_if.ack)))
    |-> tb2_data_if.data == tracked_data;
endproperty
```

---
### TB3: Dynamic / Static Verification
#### Data must remain stable
When a valid transaction is received, the data must remain unchanged throughout the required static interval and lifetime:

```systemverilog
property holds_data_stable;
    @(posedge clk_i) disable iff (!rst_assert_ni)
    (state_curr == HOLD_STATIC ||
     state_curr == HOLD_LIFETIME)
    |-> (tb3_data_if.data == tracked_data);
endproperty
```

---
### TB4: Static / Static Verification
#### Data must remain stable
Since both sender and receiver operate statically, the data must remain unchanged until both the static interval and lifetime requirements have completed:

```systemverilog
property holds_data_stable;
    @(posedge clk_i) disable iff (!rst_assert_ni)
    (state_curr == HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL)
    |-> tb4_data_if.data == tracked_data;
endproperty
```


## Compiler Usage
SystemVerilog extern verification is enabled using the `-sv-extern` compiler option:

```bash
dune exec anvil -- -sv-extern <top_module>.anvil
```

## Running Examples

1. Generates the normal Anvil SystemVerilog file <top_module>.anvil.sv.
```bash
make MODULE_NAME=<top_module>
```

2. Generates the verification wrapper for the selected top module.
```bash
dune exec anvil -- -sv-extern <top_module>.anvil > <top_module>_ultimate_wrapper.sv
```

3. Generates the Verilator filelist containing all required SystemVerilog files.
```bash
python3 extern_generate_filelist.py <top_module>_ultimate_wrapper.sv
```

4. Compiles the verification design with Verilator and enables SystemVerilog assertions.
```bash
verilator -Wall --binary --exe --assert --trace --build -f <top_module>_ultimate_wrapper_filelist.f --timing --top-module <top_module>_ultimate_wrapper -o sim.out -Wno-UNDRIVEN -Wno-UNUSED -Wno-DECLFILENAME
```

5. Runs the simulation and stores the verification output in <top_module>.test.
```bash
./obj_dir/sim.out +verilator+quiet > <top_module>.test 2>&1
```

All of the above steps can be run together using:
```bash
make sv_extern MODULE_NAME=<top_module>
```

### Example 1:  Dynamic Sender and Dynamic Receiver, Correct `valid` Behaviour
Files used:
```text
extern_dyn_dyn.anvil
extern_axi_cpu_master.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_dyn_dyn
```

The external SystemVerilog module keeps `valid` asserted until the transaction can complete, so the assertions should pass.
The simulation result is written to:
```text
extern_dyn_dyn.test
```

Expected output:
```text
received AXI request  268435520
received AXI request  268435520
received AXI request  268435520
...
```

If the design is correct, **no assertion failure** message will be printed.

### Example 2: Dynamic Sender and Dynamic Receiver, Invalid `valid` Behaviour
Files used:
```text
extern_dyn_dyn_invalid_valid.anvil
extern_axi_cpu_master_invalid_valid.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_dyn_dyn_invalid_valid
```

This example intentionally drops `valid` before the handshake is completed.
The simulation result is written to:
```text
extern_dyn_dyn_invalid_valid.test
```

Expected output includes:
```text
Assertion Failed: holds_valid_before_handshake
```

This example is **expected to fail**, showing that `tb1` correctly detects an invalid `valid` handshake.

### Example 3: Static Sender and Dynamic Receiver

Files used:
```text
extern_static_dyn.anvil
extern_dram_read_data_source.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_static_dyn
```

This example uses a **static sender** and a **dynamic receiver**. The sender does not use a `valid` signal, while the receiver provides an `ack` signal when the data is accepted.

The simulation result is written to:
```text
extern_static_dyn.test
```

Expected output:
```text
Received DRAM read data: ...
DRAM data changed to: ...
Received DRAM read data: ...
...
```

If the design is correct, **no assertion failure** message will be printed. This shows that `tb2` correctly verifies data stability for a static sender with a dynamic receiver.


### Example 4: Dynamic Sender and Static Receiver

Files used:
```text
extern_dyn_static.anvil
extern_cpu_command_source.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_dyn_static
```

This example uses a **dynamic sender** and a **static receiver**. The sender provides a `valid` signal, while no `ack` signal is required from the static receiver.

The simulation result is written to:
```text
extern_dyn_static.test
```

Expected output:
```text
CPU issued config command: ...
Received CPU config command: ...
CPU issued config command: ...
Received CPU config command: ...
...
```

If the design is correct, **no assertion failure** message will be printed. This shows that `tb3` correctly checks that the data remains stable during the required static hold period.


### Example 5: Static Sender and Static Receiver

Files used:
```text
extern_static_static.anvil
extern_dram_init_table_source.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_static_static
```

This example uses a **static sender** and a **static receiver**. Since both sides operate statically, no `valid` or `ack` signal is required. Only the data interface is generated.

The simulation result is written to:
```text
extern_static_static.test
```

Expected output:
```text
Received DRAM timing word: ...
Received DRAM timing word: ...
Received DRAM timing word: ...
DRAM timing word changed to: ...
...
```

If the design is correct, **no assertion failure** message will be printed. This shows that `tb4` correctly verifies data stability for a fully static channel.


### Example 6: Multiple Messages in One Channel

Files used:
```text
extern_combined_messages.anvil
extern_memory_subsystem_sources.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_combined_messages
```

This example contains multiple messages in the same channel, with different synchronization behaviours:

```text
axi_req         -> TB1: Dynamic / Dynamic
read_data_beat  -> TB2: Static / Dynamic
cfg_cmd         -> TB3: Dynamic / Static
timing_word     -> TB4: Static / Static
```

Each message is handled independently and should generate the corresponding verification testbench and interfaces.

The simulation result is written to:
```text
extern_combined_messages.test
```

Expected output contains messages from all four transactions:
```text
Received AXI request: ...
Received DRAM read data: ...
Received CPU config command: ...
Received DRAM timing word: ...
...
```

If the design is correct, **no assertion failure** message will be printed.

This example verifies that multiple messages with different synchronization modes can coexist within the same channel.


### Example 7: Multiple Channels and SystemVerilog Files

Files used:
```text
extern_multiple_channels.anvil
extern_axi_cpu_master.sv
extern_dram_read_data_source.sv
extern_cpu_command_source.sv
extern_dram_init_table_source.sv
```

Run the example using:
```bash
make sv_extern MODULE_NAME=extern_multiple_channels
```

This example uses multiple independent channels and multiple external SystemVerilog modules in the same design.

The channels correspond to the four supported synchronization combinations:

```text
AXI request channel       -> TB1
DRAM read channel         -> TB2
CPU configuration channel -> TB3
DRAM timing channel       -> TB4
```

Each external SystemVerilog module is connected to its own channel, while the Anvil DUT communicates with all four channels through separate endpoints.

The simulation result is written to:
```text
extern_multiple_channels.test
```

Expected output contains activity from all four channels:
```text
Received AXI request: ...
Received DRAM read data: ...
Received CPU config command: ...
Received DRAM timing word: ...
...
```

If the design is correct, **no assertion failure** message will be printed.

This example verifies that the generated wrapper supports multiple channels, multiple spawned external processes, and multiple SystemVerilog source files in a single design.