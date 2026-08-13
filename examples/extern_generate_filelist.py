#!/usr/bin/env python3
"""
Generate filelist for top module.

Structure:
project/
+-- examples/             <- RTL + script + Makefile here
|   +-- script.py
|   +-- top_module.sv
|   +-- design.sv
|   +-- Makefile
+-- extern_properties/    <- tb1.sv, tb2.sv, etc.
    +-- tb1.sv
    +-- tb2.sv
    +-- tb3.sv
    +-- tb4.sv
"""

import re
import os
from pathlib import Path
from collections import defaultdict

# ============================================================
# DIRECTORY STRUCTURE
# ============================================================

def get_base_dir():
    """Get base directory (examples/ folder where script runs)."""
    return Path(os.environ.get('BASE_DIR', os.getcwd())).resolve()


def get_rtl_dir():
    """RTL files are directly in examples/ (same as BASE_DIR)."""
    return get_base_dir()


def get_extern_dir():
    """extern_properties/ is sibling of examples/."""
    base = get_base_dir()
    extern = base.parent / 'extern_properties'
    if extern.exists():
        return extern
    return None


def get_known_mappings():
    """Get tb1-tb4 from extern_properties/ directory."""
    extern_dir = get_extern_dir()
    mappings = {}

    if extern_dir is None:
        return mappings

    for tb_name in ['tb1', 'tb2', 'tb3', 'tb4']:
        for ext in ['.sv', '.v']:
            c = extern_dir / f'{tb_name}{ext}'
            if c.exists():
                mappings[tb_name] = c.resolve()
                break

    return mappings


def get_interface_files():
    """Get interface files from extern_properties/ directory."""
    extern_dir = get_extern_dir()
    interfaces = {}

    if extern_dir is None:
        return interfaces

    # interface_control.sv contains tb1_control_if, tb2_control_if, tb3_control_if
    ctrl_file = extern_dir / 'interface_control.sv'
    if ctrl_file.exists():
        interfaces['control'] = ctrl_file.resolve()

    # interface_data.sv contains data_if
    data_file = extern_dir / 'interface_data.sv'
    if data_file.exists():
        interfaces['data'] = data_file.resolve()

    return interfaces


def detect_interface_usage(all_file_contents):
    """Detect which interfaces are used based on file contents."""
    combined = '\n'.join(all_file_contents)

    # Check for control interface usage (tb1_control_if, tb2_control_if, tb3_control_if)
    uses_control = bool(re.search(r'\b(tb1_control_if|tb2_control_if|tb3_control_if)\b', combined))

    # Check for data interface usage (data_if)
    uses_data = bool(re.search(r'\bdata_if\b', combined))

    # Check which tb modules are used
    uses_tb1 = bool(re.search(r'\btb1\b', combined))
    uses_tb2 = bool(re.search(r'\btb2\b', combined))
    uses_tb3 = bool(re.search(r'\btb3\b', combined))
    uses_tb4 = bool(re.search(r'\btb4\b', combined))

    # interface_control.sv needed if tb1/tb2/tb3 used OR control interfaces used directly
    need_control = uses_control or uses_tb1 or uses_tb2 or uses_tb3

    return need_control, uses_data


# ============================================================
# VERILOG PARSER
# ============================================================

def parse_verilog_file(filepath):
    """Extract module definitions and instantiations."""
    try:
        with open(filepath, 'r', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        print(f"Warning: Could not read {filepath}: {e}")
        return [], set()

    content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    content = re.sub(r'"[^"]*"', '', content)

    modules_defined = re.findall(r'\bmodule\s+(\w+)', content)

    keywords = {
        'module', 'endmodule', 'function', 'task', 'if', 'else', 'case',
        'for', 'while', 'assign', 'always', 'always_ff', 'always_comb',
        'initial', 'begin', 'end', 'wire', 'reg', 'logic', 'input', 'output',
        'parameter', 'localparam', 'generate', 'genvar', 'typedef', 'struct'
    }

    instantiations = set()

    # Remove parameter blocks #(...) with balanced parentheses before parsing
    def remove_param_blocks(s):
        result = []
        i = 0
        while i < len(s):
            if s[i:i+2] == '#(' or (s[i] == '#' and i+1 < len(s) and s[i+1:].lstrip().startswith('(')):
                # Find the opening paren
                j = i + 1
                while j < len(s) and s[j] != '(':
                    j += 1

                if j < len(s):
                    # Count balanced parens
                    depth = 1
                    j += 1
                    while j < len(s) and depth > 0:
                        if s[j] == '(':
                            depth += 1
                        elif s[j] == ')':
                            depth -= 1
                        j += 1
                    i = j  # Skip past the parameter block
                    continue

            result.append(s[i])
            i += 1

        return ''.join(result)

    content_no_params = remove_param_blocks(content)

    # Now simple pattern: module_name instance_name (
    inst_pattern = r'\b(\w+)\s+(\w+)\s*\('

    for match in re.finditer(inst_pattern, content_no_params):
        module_name = match.group(1)
        if module_name.lower() not in keywords and not module_name[0].isdigit():
            instantiations.add(module_name)

    return modules_defined, instantiations


def get_top_module_name(top_file):
    """Extract first module name from top file."""
    modules, _ = parse_verilog_file(top_file)
    return modules[0] if modules else Path(top_file).stem


def get_base_stem(filename):
    """Get base module name from filename (handles dotted names like try_new.anvil.sv)."""
    stem = Path(filename).stem  # try_new.anvil
    # Take first part before any dots
    return stem.split('.')[0]  # try_new


def get_design_stem(top_file):
    """
    Get the original Anvil top name from an ultimate-wrapper filename.

    Example:
      extern_dyn_dyn_invalid_valid_ultimate_wrapper.sv
          -> extern_dyn_dyn_invalid_valid
    """
    stem = get_base_stem(top_file)
    suffix = '_ultimate_wrapper'
    if stem.endswith(suffix):
        return stem[:-len(suffix)]
    return stem


# ============================================================
# MAIN GENERATOR
# ============================================================

def generate_filelist(top_file, output=None, verbose=False):
    """Generate filelist starting from top module file."""

    top_file = Path(top_file).resolve()
    base_dir = get_base_dir()
    rtl_dir = get_rtl_dir()
    extern_dir = get_extern_dir()
    known_mappings = get_known_mappings()

    top_module_name = get_top_module_name(top_file)
    # Use top file's base stem for output name (not first module defined)
    top_file_stem = get_base_stem(top_file)

    # Original Anvil top corresponding to this ultimate wrapper.
    # Example:
    #   extern_dyn_dyn_invalid_valid_ultimate_wrapper.sv
    #       -> extern_dyn_dyn_invalid_valid
    design_stem = get_design_stem(top_file)

    if output is None:
        output = f'{top_file_stem}_filelist.f'

    interface_files = get_interface_files()

    if verbose:
        print(f"Base (examples/): {base_dir}")
        print(f"RTL directory:     {rtl_dir}")
        print(f"Extern directory:  {extern_dir}")
        print(f"Top file:          {top_file}")
        print(f"Top module:        {top_module_name}")
        print(f"Output:            {output}")
        print(f"\nKnown mappings (from extern/):")
        for mod, fpath in known_mappings.items():
            print(f"  {mod} -> {fpath}")
        print(f"\nInterface files:")
        for iface, fpath in interface_files.items():
            print(f"  {iface} -> {fpath}")

    # Data structures
    module_to_file = {}
    file_modules = {}
    file_insts = {}
    files_to_process = [top_file]
    processed_files = set()
    all_files = []

    # Load known mappings
    for mod, fpath in known_mappings.items():
        module_to_file[mod] = fpath

    # Build index of ALL RTL files in examples/.
    #
    # IMPORTANT:
    # Do not rely only on the filename. A generated file such as
    #   extern_dyn_dyn.anvil.sv
    # can define several modules:
    #   axi_cpu_master_ext
    #   memory_controller_dut
    #   extern_dyn_dyn
    #
    # The ultimate wrapper instantiates the first two by module name,
    # so we must pre-scan every RTL file and map each module definition
    # back to the file that defines it.
    file_index = {}
    rtl_files = []

    for ext in ['.v', '.sv']:
        for f in rtl_dir.glob(f'*{ext}'):
            f = f.resolve()
            rtl_files.append(f)

            # Keep the old filename-based fallback.
            file_index[f.stem] = f
            base = get_base_stem(f)
            if base != f.stem and base not in file_index:
                file_index[base] = f

    # Pre-index MODULE DEFINITIONS before recursive dependency traversal.
    #
    # IMPORTANT:
    # Different examples may contain modules with the same child-process
    # names (for example axi_cpu_master_ext or memory_controller_dut).
    # The current design's generated <design>.anvil.sv must therefore
    # take priority over older/generated examples in the same directory.
    preferred_rtl = (rtl_dir / f'{design_stem}.anvil.sv').resolve()

    if preferred_rtl.exists():
        modules, _ = parse_verilog_file(preferred_rtl)
        for module_name in modules:
            # Keep explicit tb1-tb4 mappings from extern_properties/,
            # but prefer the current design for ordinary RTL modules.
            if module_name not in known_mappings:
                module_to_file[module_name] = preferred_rtl

    # Index all remaining RTL files only as fallbacks.
    for f in rtl_files:
        if f == preferred_rtl:
            continue

        modules, _ = parse_verilog_file(f)
        for module_name in modules:
            if module_name not in module_to_file:
                module_to_file[module_name] = f

    if verbose:
        print(f"\nIndexed {len(rtl_files)} RTL files in {rtl_dir}")
        print(f"Current design:     {design_stem}")
        print(f"Preferred RTL:      {preferred_rtl if preferred_rtl.exists() else 'NOT FOUND'}")
        print(f"Indexed {len(module_to_file)} module definitions/mappings")

    # Process files recursively
    while files_to_process:
        current_file = files_to_process.pop(0)

        if current_file in processed_files:
            continue
        processed_files.add(current_file)
        all_files.append(current_file)

        if verbose:
            print(f"\nProcessing: {current_file.name}")

        modules, insts = parse_verilog_file(current_file)
        file_modules[current_file] = modules
        file_insts[current_file] = insts

        for m in modules:
            if m not in module_to_file:
                module_to_file[m] = current_file

        if verbose:
            print(f"  Defines: {modules}")
            print(f"  Instantiates: {insts}")

        for inst in insts:
            if inst in module_to_file:
                dep_file = module_to_file[inst]
                if dep_file not in processed_files and dep_file not in files_to_process:
                    files_to_process.append(dep_file)
            elif inst in file_index:
                dep_file = file_index[inst]
                module_to_file[inst] = dep_file
                if dep_file not in processed_files and dep_file not in files_to_process:
                    files_to_process.append(dep_file)
                if verbose:
                    print(f"  Found: {inst} -> {dep_file.name}")
            else:
                if verbose:
                    print(f"  Unresolved: {inst}")

    # Build dependency graph
    file_deps = defaultdict(set)
    for f in all_files:
        for inst in file_insts.get(f, set()):
            if inst in module_to_file:
                dep_file = module_to_file[inst]
                if dep_file != f:
                    file_deps[f].add(dep_file)

    # Topological sort
    visited = set()
    order = []

    def visit(f):
        if f in visited:
            return
        visited.add(f)
        for dep in file_deps.get(f, set()):
            visit(dep)
        order.append(f)

    for f in all_files:
        visit(f)

    # Detect interface usage from all processed files
    all_contents = []
    for f in all_files:
        try:
            with open(f, 'r', errors='ignore') as fh:
                all_contents.append(fh.read())
        except:
            pass

    need_control, need_data = detect_interface_usage(all_contents)

    # Build final order: interfaces first, then RTL in dependency order
    interface_order = []
    if need_data and 'data' in interface_files:
        interface_order.append(interface_files['data'])
    if need_control and 'control' in interface_files:
        interface_order.append(interface_files['control'])

    final_order = interface_order + order

    if verbose:
        print(f"\nInterface requirements:")
        print(f"  need_control (tb1/tb2/tb3): {need_control}")
        print(f"  need_data: {need_data}")

    # Write filelist
    with open(output, 'w') as out:
        out.write(f"// Filelist for {top_file_stem}\n")
        out.write(f"// Files: {len(final_order)}\n\n")
        for f in final_order:
            out.write(f"{f}\n")

    print(f"Generated {output} with {len(final_order)} files")

    return final_order, output


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 2:
        print("Usage: python3 script.py <top_file.sv> [-v]")
        print("")
        print("Run from examples/ directory:")
        print("  cd project/examples")
        print("  python3 extern_generate_filelist.py top_module.sv -v")
        sys.exit(1)

    top_file = sys.argv[1]
    verbose = '-v' in sys.argv
    output = None

    if '-o' in sys.argv:
        idx = sys.argv.index('-o')
        if idx + 1 < len(sys.argv):
            output = sys.argv[idx + 1]

    generate_filelist(top_file, output=output, verbose=verbose)