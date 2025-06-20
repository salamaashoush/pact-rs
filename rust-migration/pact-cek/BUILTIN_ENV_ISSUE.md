# Builtin Environment Access Issue

## Problem

The current implementation has an issue with partial application of native functions. When a native function is partially applied, we create a `PartialNativeFn` structure that doesn't store the CEKEnv. Later, when we complete the application, we need the env to call the native implementation, but it's not available.

## Affected Code

1. `apply_partial_native` in eval.rs - needs env but doesn't have it
2. Line 1067 in eval.rs - another location where env is needed

## Temporary Solution

For now, we've updated most of the code to properly thread the CEKEnv through function applications. The main apply_lambda path works correctly. The partial application cases need to be redesigned to store the env.

## Proper Fix

The proper fix would be to:
1. Add `env: CEKEnv` field to `PartialNativeFn` structure
2. Update all places that create `PartialNativeFn` to include the env
3. Use that env when applying the partial

This follows the pattern used in `PartialClosure` and other closure types that do store the env.