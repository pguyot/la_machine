#!/usr/bin/env python3
"""
JSON to Shuffled Erlang Function Converter

This script converts choreography sequences from JSON format to a single Erlang
function containing all choreographies as separate blocks in shuffled order.
"""

import json
import sys
import re
import random
from typing import Dict, List, Tuple

def load_choreographies_json(file_path: str) -> Dict[str, str]:
    """
    Load choreographies from JSON file.
    
    Args:
        file_path: Path to the JSON file
        
    Returns:
        Dictionary of choreographies
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            choreographies = json.load(file)
        print(f"Loaded {len(choreographies)} choreographies from {file_path}")
        return choreographies
    except FileNotFoundError:
        print(f"Error: File {file_path} not found")
        return {}
    except json.JSONDecodeError as e:
        print(f"Error parsing JSON: {e}")
        return {}
    except Exception as e:
        print(f"Error reading file: {e}")
        return {}

def format_choreography_commands(commands: str) -> str:
    """
    Format choreography commands for Erlang output.
    
    Args:
        commands: Raw choreography command string
        
    Returns:
        Formatted command string
    """
    if not commands.strip():
        return ""
    
    # Clean up the commands - remove extra whitespace
    commands = re.sub(r'\s+', ' ', commands.strip())
    
    # Remove trailing comma if present
    commands = re.sub(r',\s*$', '', commands)
    
    return commands

def handle_special_choreographies(name: str, commands: str) -> List[Tuple[str, str]]:
    """
    Handle special choreographies that have multiple parts (like poke).
    
    Args:
        name: Choreography name
        commands: Commands string
        
    Returns:
        List of tuples (block_name, command_block)
    """
    # Check if this is a multi-part choreography
    if ' ], [ ' in commands:
        # Split into multiple parts
        parts = commands.split(' ], [ ')
        command_blocks = []
        
        for i, part in enumerate(parts):
            # Clean up the part
            part = part.strip()
            if i == 0:
                # First part - remove leading bracket if present
                part = part.lstrip('[ ')
            if i == len(parts) - 1:
                # Last part - remove trailing bracket if present
                part = part.rstrip(' ]')
            
            # Create a unique name for each part
            block_name = f"{name}_part_{i+1}" if len(parts) > 1 else name
            command_blocks.append((block_name, format_choreography_commands(part)))
        
        return command_blocks
    else:
        # Single part choreography
        return [(name, format_choreography_commands(commands))]

def parse_choreographies(choreographies: Dict[str, str]) -> List[Tuple[str, str]]:
    """
    Iterates choreographies and return as list of (name, commands) tuples.
    
    Args:
        choreographies: Dictionary of choreographies
        seed: Random seed for reproducible shuffling (optional)
        
    Returns:
        List of shuffled (name, commands) tuples
    """
    
    # Convert to list of blocks, handling multi-part choreographies
    all_blocks = []
    for name, commands in choreographies.items():
        blocks = handle_special_choreographies(name, commands)
        all_blocks.extend(blocks)
        
    print(f"Converted {len(all_blocks)} choreography blocks")
    return all_blocks

def generate_erlang_functions(blocks: List[Tuple[str, str]], function_prefix: str = "scenario_") -> str:
    """
    Generate all Erlang functions for all choreographies with the same order.
    
    Args:
        choreographies: Dictionary of choreographies
        function_prefix: Prefix of the Erlang function
        
    Returns:
        Complete Erlang functions as string
    """
    
    all_blocks = []
    
    # Process each shuffled block
    for block_name, commands in blocks:
        if commands.strip():  # Only add non-empty blocks
            # The function
            formatted_block = f"{function_prefix}{block_name}() ->\n\t[\n\t\t{commands}\n\t]."
            all_blocks.append(formatted_block)
    
    # Join all blocks with nl
    erlang_code = "\n\n".join(all_blocks)
    
    return erlang_code

def generate_erlang_export(blocks: List[Tuple[str, str]], function_prefix: str = "scenario_") -> str:
    """
    Generate all Erlang exports for all choreographies with the same order.
    
    Args:
        choreographies: Dictionary of choreographies
        function_prefix: Prefix of the Erlang function
        
    Returns:
        Complete Erlang exports as string
    """
    
    all_exports = []
    
    # Process each shuffled block
    for block_name, commands in blocks:
        if commands.strip():  # Only add non-empty blocks
            # The export
            exportstring = f"\t{function_prefix}{block_name}/0"
            all_exports.append(exportstring)
    
    # Join all blocks with nl
    erlang_code = ",\n".join(all_exports)
    
    return erlang_code

def generate_complete_erlang_module(choreographies: Dict[str, str], module_name: str = "la_machine_scenarios", function_prefix: str = "scenario_") -> str:
    """
    Generate complete Erlang module with one function for each choreography of the choreographies.
    
    Args:
        choreographies: Dictionary of choreographies
        module_name: Name of the Erlang module
        function_prefix: Prefix of the Erlang functions
        
    Returns:
        Complete Erlang module code
    """

    # Get the blocks : (name, commands) tuples : List[Tuple[str, str]]
    blocks = parse_choreographies(choreographies)

    # Generate the functions
    functions_code = generate_erlang_functions(blocks, function_prefix)

    # generate the exports
    exports = generate_erlang_export(blocks, function_prefix)

    # Module header
    
    header = f"""%%
%% This file is part of La Machine
%%
%% Copyright 2024-2025 Olivier Mével <olivier@multiplie.fr>
%%                     Paul Guyot <pguyot@kallisys.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0
%%

-module({module_name}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    count/1,
    get/2
]).

-export([
{exports}
    ]).

-export_type([
    scenario/0
]).

-type scenario() :: [scenario_part()].
-type scenario_part() :: [scenario_element()].
-type scenario_element() ::
    {{servo, non_neg_integer()}}
    | {{servo, non_neg_integer(), non_neg_integer()}}
    | {{aac, binary()}}
    | {{wait, non_neg_integer() | sound}}.

%% To save memory and leverage mmap, each scenario is a dedicated exported
%% function, named "{function_prefix}<type>".
%% Suffixes are ignored but scenarios are sorted lexicographically to get
%% ith scenario with get/2.

-spec count(atom()) -> non_neg_integer().
count(Type) ->
    Prefix = "{function_prefix}" ++ atom_to_list(Type),
    PrefixLen = length(Prefix),
    Exports = ?MODULE:module_info(exports),
    lists:foldl(
        fun(Export, AccCount) ->
            case Export of
                {{FuncName, 0}} ->
                    FuncPrefix = lists:sublist(atom_to_list(FuncName), PrefixLen),
                    if
                        FuncPrefix =:= Prefix -> AccCount + 1;
                        true -> AccCount
                    end;
                {{_FuncName, _OtherArity}} ->
                    AccCount
            end
        end,
        0,
        Exports
    ).

-spec get(atom(), pos_integer()) -> scenario().
get(Type, Ith) ->
    Prefix = "{function_prefix}" ++ atom_to_list(Type),
    PrefixLen = length(Prefix),
    Exports = ?MODULE:module_info(exports),
    AllOfType = lists:foldl(
        fun(Export, AccL) ->
            case Export of
                {{FuncName, 0}} ->
                    FuncPrefix = lists:sublist(atom_to_list(FuncName), PrefixLen),
                    if
                        FuncPrefix =:= Prefix -> [FuncName | AccL];
                        true -> AccL
                    end;
                {{_FuncName, _OtherArity}} ->
                    AccL
            end
        end,
        [],
        Exports
    ),
    Sorted = lists:sort(AllOfType),
    Selected = lists:nth(Ith, Sorted),
    ?MODULE:Selected().

"""
    
    # Add test section
    test_section = """

-ifdef(TEST).
count_test_() ->
    [
        ?_assert(count(play) > 0),
        ?_assertEqual(0, count(poke)),
        ?_assertEqual(0, count(unknown))
    ].

get_test_() ->
    [
        ?_assert(is_list(get(play, 1))),
        ?_assertEqual([], get(unknown, 1))
    ].
-endif.
"""
    
    return header + functions_code + test_section

def save_erlang_file(erlang_code: str, output_path: str) -> bool:
    """
    Save Erlang code to file.
    
    Args:
        erlang_code: Generated Erlang code
        output_path: Output file path
        
    Returns:
        True if successful, False otherwise
    """
    try:
        with open(output_path, 'w', encoding='utf-8') as file:
            file.write(erlang_code)
        return True
    except Exception as e:
        print(f"Error writing Erlang file: {e}")
        return False

def main():
    """Main function to run the JSON to shuffled Erlang function converter."""
    # Default file paths
    json_file = "choreographies.json"
    erlang_file = "la_machine_scenarios.erl"
    
    # Handle command line arguments
    if len(sys.argv) > 1:
        json_file = sys.argv[1]
    if len(sys.argv) > 2:
        erlang_file = sys.argv[2]
    
    print(f"Converting choreographies from: {json_file}")
    print(f"Output Erlang file: {erlang_file}")
    print("-" * 50)
    
    # Load choreographies from JSON
    choreographies = load_choreographies_json(json_file)
    
    if not choreographies:
        print("No choreographies found or error occurred")
        return
    
    print(f"\nConverting {len(choreographies)} choreographies:")
    
    # Generate Erlang module with shuffled function
    erlang_code = generate_complete_erlang_module(choreographies, "la_machine_scenarios", "scenario_")
    
    # Save to Erlang file
    if save_erlang_file(erlang_code, erlang_file):
        print(f"\nSuccessfully generated Erlang functions: {erlang_file}")
    else:
        print(f"\nFailed to save Erlang file: {erlang_file}")

if __name__ == "__main__":
    main()